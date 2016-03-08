#! /usr/bin/Rscript --vanilla

##########################
# Author: John J. Horton 
# Project: kwow 
##########################

library(devtools)
devtools::install("kwowR")
library(kwowR)
devtools::install_github("johnjosephhorton/JJHmisc")
library(JJHmisc)

mturk.df <- kwowR::Get_mturk_df()

tables.path <- "../writeup/tables/"
plots.path <- "../writeup/plots/"

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

pdf(paste0(plots.path, "knowledge_by_occupation.pdf"), width = 7, height = 10)
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

pdf(paste0(plots.path, "social_by_occupation.pdf"), width = 7, height = 10)
print(g.social)
dev.off()

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))


if (file.exists("../writeup/parameters.tex")){
    system("rm ../writeup/parameters.tex")
}

addParam <- genParamAdder("../writeup/parameters.tex")
addParam("\\numRaters", formatC(length(unique(mturk.df$WorkerId)), big.mark = ","))
addParam("\\numOccupations", formatC(length(unique(mturk.df$title)), big.mark = ","))

#################
# Restrict Sample
#################

df.tmp <- data.table(mturk.df)
df.tmp.by.occ <- df.tmp[, list(num.ratings = .N, know.job = sum(know.job=="Yes")), by = list(title)]
known.jobs <- subset(df.tmp.by.occ, know.job >= 20)$title

mturk.df <- subset(mturk.df, know.job == "Yes" & title %in% known.jobs)

# Predictive Accuracy 

m.emp <- felm(actual.v.trend ~ v.trend  | WorkerId | 0 | WorkerId, data = mturk.df)
m.wage <- felm(log(actual.mean.wage) ~ log(predicted.wage)| WorkerId | 0 | WorkerId, data = mturk.df)

out.file <- paste0(tables.path, "predictions.tex")
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

###########################
# Defining prediction error
###########################

mturk.df$error.pct <- with(mturk.df,
                           scale(abs((log(actual.mean.wage) - log(predicted.wage))/log(actual.mean.wage))))
mturk.df$error <- with(mturk.df, scale(abs(log(actual.mean.wage) - log(predicted.wage))  ))

####################################
# Worker and Occupation-Specific FEs
####################################

m.wage <-felm(error ~ social | WorkerId + title | 0 | WorkerId, data = mturk.df)
m.v.trend <- felm(v.trend.error ~ social | WorkerId + title | 0 | WorkerId,  data = mturk.df)
out.file <- paste0(tables.path, "error_fe_estimate.tex")
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
JJHmisc::AddTableNote(s, out.file, note)

##############################
# Occupation-specific approach 
##############################

df.wage <- ddply(mturk.df, .(title), summarise,
                 mturk.w = mean(predicted.wage),
                 se.mturk.w = sd(predicted.wage)/sqrt(length(predicted.wage)), 
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
    geom_errorbar(aes(x = oes.w, y = mturk.w, ymin = mturk.w - 2*se.mturk.w, ymax = mturk.w + 2*se.mturk.w),
                  colour = "grey") + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_text(data = df.wage.outliers, aes(label = title), size = 1.75, vjust = 0, angle = 0) +
    theme_bw() +
    ylab("Respondent mean log wage for occupation") +
    xlab("Actual mean log wage for occupation") +
    scale_x_log10(breaks = axes.breaks, limits = c(5, 100)) +
    scale_y_log10(breaks = axes.breaks, limits = c(5, 100)) 

# print(g.predicted.v.actual)

JJHmisc::writeImage(g.predicted.v.actual, "predicted_v_actual_RAW", height = 6, width = 6, path = plots.path)
JJHmisc::writeImage(g.predicted.v.actual, "predicted_v_actual", height = 6, width = 6, path = plots.path)

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

JJHmisc::writeImage(g.box.plot, "box_plots_by_occupation", width = 7, height = 10, path = plots.path)

