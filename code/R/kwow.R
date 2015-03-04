#! /usr/bin/Rscript --vanilla

##########################
# Author: John J. Horton 
# Created Date: 2013-11-07 
# Project: kwow 
##########################

library(foreign)
library(ggplot2)
library(lme4)
library(memisc)
library(plm)
library(plyr)
library(reporttools)
library(reshape2)
library(scales)
library(stargazer)
library(stringr)
library(xtable)
library(sandwich)
library(lmtest)
library(JJHmisc)
library(lfe)
library(data.table)

source("construct_datasets.R")

set.seed(12345)

# Data from Bureau of Labor Statistics, Department of Labor
national.df <- import.top.national("../../data/national_M2012_dl.csv", 99)
# Impute volume trend estimations
national.df <- import.occupation.trend("../../data/occupation.table.1.2.csv", national.df)
# Impute education requirements
national.df <- import.education("../../data/education.categories.csv", national.df)
# Data from MTurk
mturk.df <- import.mturk("../../data/mturk_output.csv")
nrow(mturk.df)
head(mturk.df)
sapply(mturk.df, class)
# rename weird columns 
names(mturk.df)[which("Answer.know_job" == names(mturk.df))] <- "know.job"
names(mturk.df)[which("Answer.know_anyone" == names(mturk.df))] <- "social.knowledge"
names(mturk.df)[which("Input.Title" == names(mturk.df))] <- "title"
names(mturk.df)[which("Answer.wage" == names(mturk.df))] <- "predicted.wage"
# short names for job titles (which tend to be long)
SHORT.TITLE.LENGTH <- 30
mturk.df$short.title <- with(mturk.df, abbreviate(title, SHORT.TITLE.LENGTH))
getNationalMeasures <- function(field.name, title.name = "title"){
    "Function for mapping national measures to MTurk dataset"
    l <- national.df[, field.name]
    names(l) <- national.df[, "OCC_TITLE"]
    l <- as.list(l)
    as.numeric(as.character(l[ mturk.df[,title.name] ]))
}
mturk.df <- within(mturk.df, {
  actual.mean.wage <- getNationalMeasures("H_MEAN")
  actual.median.wage <- getNationalMeasures("H_MEDIAN")
  tot.emp <- getNationalMeasures("TOT_EMP")
  actual.v.trend <- getNationalMeasures("v.trend")
  v.trend.error <- abs(actual.v.trend - v.trend)
  pct.mean.error <- abs((actual.mean.wage - predicted.wage)/actual.mean.wage)
  prediction.delta <- log(actual.mean.wage) - log(predicted.wage)
  know.someone <- I(!is.na(social.knowledge) & social.knowledge != "0")
  error <- abs( log(actual.mean.wage) - log(predicted.wage) )
})
mturk.df <- na.omit(mturk.df)
saveRDS(mturk.df, "../../data/mturk_cooked.rds")


############################
# Knowledge of jobs by title
############################

df.wage <- ddply(mturk.df, .(title), summarise,
                 mturk.w = mean(predicted.wage),
                 oes.w = mean(actual.mean.wage),
                 tot.emp = mean(tot.emp),
                 know.mu = mean(know),
                 social.mu = mean(social),
                 error = log(oes.w) - log(mturk.w))

df.know <- na.omit(subset(df.wage, log(tot.emp) > 13))
df.know$title <- as.factor(df.know$title)
df.know$title <- with(df.know, reorder(title, know.mu, mean))

g.know <- ggplot(data = df.know,
                 aes(y = title, x = know.mu)) +
    geom_point() +
    xlab("Mean of (Do you know what this job is? -1 = No, 0 = Maybe, 1 = Yes") +
    ylab("") +
    theme_bw()

pdf("../../writeup/plots/knowledge_by_occupation.pdf", width = 7, height = 10)
print(g.know)
dev.off()

#######################
# Know someone by title 
#######################

df.wage$title <- with(df.wage, reorder(title, social.mu, mean))

g.social <- ggplot(data = na.omit(subset(df.wage, log(tot.emp) > 13)),
                 aes(y = title, x = social.mu)) + geom_point() +
    xlab("Mean of social index") +
    ylab("") +
    theme_bw()

# print(g.social)

pdf("../../writeup/plots/social_by_occupation.pdf", width = 7, height = 10)
print(g.social)
dev.off()

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))


# mturk.df <- data.table(mturk.df)
#mturk.df.raw <- mturk.df
#mturk.df <- subset(mturk.df, know.job == "Yes")

# Store parameters

system("rm ../../writeup/parameters.tex")
addParam <- genParamAdder("../../writeup/parameters.tex")
addParam("numRaters", formatC(length(unique(mturk.df$WorkerId)), big.mark = ","))
addParam("numOccupations", formatC(length(unique(mturk.df$title)), big.mark = ","))

#################
# Restrict Sample
#################

df.tmp <- data.table(mturk.df)
df.tmp.by.occ <- df.tmp[, list(num.ratings = .N, know.job = sum(know.job=="Yes")), by = list(title)]
known.jobs <- subset(df.tmp.by.occ, know.job >= 20)$title


mturk.df <- subset(mturk.df, know.job == "Yes" & title %in% known.jobs)


# Predictive Accuracy 

m.emp <- felm(actual.v.trend ~ v.trend + G(WorkerId), clustervar = "WorkerId", data = mturk.df)
m.wage <- felm(log(actual.mean.wage) ~ log(predicted.wage) + G(WorkerId), clustervar = "WorkerId", data = mturk.df)

out.file <- "../../writeup/tables/predictions.tex" 
sink("/dev/null")
s <- stargazer(m.wage, m.emp, 
          title = "Predictions about wages and employment trends",
          label = "tab:predictions",
          column.labels = c("Log Mean Occupational wage", "Pred. Employment change"),
          model.names = FALSE,
          font.size = "small",     
               dep.var.labels.include = FALSE,
               star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels = c("Estimated log wage", "Estimated employment change"),
          add.lines = list(c("Respondent FE", "Yes", "Yes")),
          omit.stat = c("aic", "f", "ll", "bic", "ser"), 
          type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{0.95 \\textwidth}",
          "{\\footnotesize \\emph{Notes:}
           The sample for both regressions reported in this table are all the responses where the respondent said they knew what an occupation consisted of.
In Column~(1), the dependent variable is the actual log mean wage for that occupation (from the May 2013 OES estimates from the BLS), while in Column~(2) the dependent variable is an index for whether the BLS predicts that by 2020 the fraction employed in that occupation will grow by more than 5\\% (outcome = 1), fall by more than 5\\% (outcome = -1) or will not change by more than 5\\%, positive or negative (outcome = 0).
The important independent variable in both regressions is the respondent's prediction.
Both regressions include a respondent-specific standard error. 
Standard errors are clustered at the level of the individual respondent. 
\\starlanguage }",
"\\end{minipage}")
AddTableNote(s, out.file, note)

###############
# Summary Stats
###############

## cont.vars <- data.frame(mturk.df[, c("predicted.wage")])
## names(cont.vars) <- "Predicted Wage"
## titles <- mturk.df[, "short.title"]

## sink("../../writeup/tables/continuous_vars.tex")
## tableContinuous(cont.vars, group = titles)
## sink()

## m <- felm(know ~ log(tot.emp) + G(WorkerId), data = mturk.df)
## m <- felm(social ~ log(tot.emp) + G(WorkerId), data = mturk.df)

###########################
# Defining prediction error
###########################


mturk.df$error.pct <- with(mturk.df,
                           scale(abs((log(actual.mean.wage) - log(predicted.wage))/log(actual.mean.wage))))
mturk.df$error <- with(mturk.df, scale(abs(log(actual.mean.wage) - log(predicted.wage))  ))


####################################
# Worker and Occupation-Specific FEs
####################################

m.wage <-felm(error ~ social + G(WorkerId) + G(title), clustervar = "WorkerId", data = mturk.df)
m.v.trend <- felm(v.trend.error ~ social + G(WorkerId) + G(title), clustervar = "WorkerId",  data = mturk.df)
out.file <- "../../writeup/tables/error_fe_estimate.tex" 
sink("/dev/null")
s <- stargazer(m.wage, m.v.trend, 
          title = "Social and errors in occupational wage and employment trajectories",
          label = "tab:fe_errors",
          column.labels = c("Wage Error", "Employment Trend Error"),
          model.names = FALSE, 
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels = c("Social Knowledge Index"),
          add.lines = list(c("Respondent FE", "Yes", "Yes"), c("Occupation FE", "Yes", "Yes")),
          omit.stat = c("aic", "f", "ll", "bic", "ser"), 
          type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{0.95 \\textwidth}",
          "{\\footnotesize \\emph{Notes:} This table reports the effects of social and general knowledge on a
respondent's prediction error, while controlling for worker and
occupation specfic effects. In Columns (1) the dependent
variable is MSE in wage prediction, while in Column (2), the
dependent variable is an indicator for incorrectly predicting the
direction of employment growth in that occupation. \\starlanguage }",
          "\\end{minipage}")
AddTableNote(s, out.file, note)

##############################
# Occupation-specific approach 
##############################

df.wage <- ddply(mturk.df, .(title), summarise,
                 mturk.w = mean(predicted.wage),
                 oes.w = mean(actual.mean.wage),
                 tot.emp = mean(tot.emp),
                 know.mu = mean(know),
                 social.mu = mean(social),
                 error = log(oes.w) - log(mturk.w))

df.wage.outliers <- subset(df.wage, abs(log(oes.w) - log(mturk.w)) > .50)
axes.breaks <- c(7, 15, 30, 60, 100)
g.predicted.v.actual <- ggplot(data = df.wage,
                               aes(y = mturk.w,
                                   x = oes.w)) +
    geom_point(shape = 1, alpha = .5) +
    geom_smooth() +
    geom_abline(a = 1, b = 0, linetype = "dashed") +
    geom_text(data = df.wage.outliers, aes(label = title), size = 1.75, vjust = 0, angle = 0) +
    theme_bw() +
    ylab("Respondent mean log wage for occupation") +
    xlab("Actual mean log wage for occupation") +
    scale_x_log10(breaks = axes.breaks, limits = c(5, 100)) +
    scale_y_log10(breaks = axes.breaks, limits = c(5, 100)) 

# print(g.predicted.v.actual)

writeImage(g.predicted.v.actual, "predicted_v_actual_RAW", height = 6, width = 6)

##################
# OUTLIER RUN-DOWN
##################


#############################
# Box plots of wage knowledge 
#############################

df.overlay <- rbind(ddply(mturk.df, .(title), summarise, sum.wage = actual.mean.wage[1], obs.type = "mean"),
                ddply(mturk.df, .(title), summarise, sum.wage = actual.median.wage[1], obs.type = "median"))


mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

g.box.plot <- ggplot(data = mturk.df,
                     aes(x = title,
                         y = predicted.wage)) +
    geom_boxplot(outlier.size = 0, alpha = .5) +
    xlab("BLS Occupation") + ylab("Log hourly wage") +
    scale_y_log10(breaks = c(7, 15, 30, 60, 100)) +
        geom_point(data = df.overlay,
               aes(x = title,
                   y = sum.wage,
                   colour = obs.type,
                   shape = obs.type)
               ) + 
    coord_flip() +
    theme_bw() +
    theme(text=element_text(size = 10)) +
    scale_colour_manual(name = "Actual BLS Occupational Statistics",
                        breaks = c("median", "mean"),
                        labels = c("Median", "Mean"),
                        values = c("red", "black")) +
            scale_shape_discrete("Actual BLS Occupational Statistics",
                                 breaks=c("median", "mean"),
                                 labels=c("Median", "Mean")) +    
   theme(legend.position = "top")
    

#print(g.box.plot)

writeImage(g.box.plot, "box_plots_by_occupation", width = 7, height = 10)

