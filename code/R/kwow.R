#! /usr/bin/Rscript --vanilla

##########################
# Author: John J. Horton 
# Created Date: 2013-11-07 
# Project: kwow 
##########################

library(foreign)
#library(gdata)
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


<<<<<<< HEAD
#  library(devtools)
#  install_github("JJHmisc", "johnjosephhorton")
library(JJHmisc)

source("construct_datasets.R")
=======
# If JJHmisc is missing, run: 
# library(devtools)
# install_github("JJHmisc", "johnjosephhorton")
# library(JJHmisc)

#
# Can't install JJHmisc 
# * installing *source* package 'JJHmisc' ...
# ** R
# Error in .install_package_code_files(".", instdir) : 
#   files in '/tmp/Rtmp9DOoip/devtoolse7b3365db19/JJHmisc-master/R' missing from 'Collate' field:
#   wasItUsed.R
# writeImage.R
# ERROR: unable to collate and parse R files for package 'JJHmisc'
>>>>>>> 737e11b304a92d4a6abb10010efc2635c2896602

## Clearing the workspace
# rm(list=ls(all=TRUE))
# gc(reset=TRUE)
set.seed(12345)

#@ Function to import Data from Bureau of Labor Statistics, Department of Labor
## Imports TOP.N jobs from specified OCC.CAT
##
## ARGS:
## fname - path to CSV file with original data
## TOP.N - top jobs to import
## OCC.CAT - occupation categories
##
###############################################

# Data from Bureau of Labor Statistics, Department of Labor
national.df <- import.top.national("../../data/national_M2012_dl.csv", 99)

# Impute volume trend estimations
national.df <- import.occupation.trend("../../data/occupation.table.1.2.csv", national.df)

# Impute education requirements
national.df <- import.education("../../data/education.categories.csv", national.df)

nrow(national.df)
head(national.df)
sapply(national.df, class)

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
#names(mturk.df)[which("Answer.volume_trend" == names(mturk.df))] <- "v.trend"

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

###############
# Summary Stats
###############

cont.vars <- data.frame(mturk.df[, c("predicted.wage")])
names(cont.vars) <- "Predicted Wage"
titles <- mturk.df[, "short.title"]

sink("../../writeup/tables/continuous_vars.tex")
tableContinuous(cont.vars, group = titles)
sink()

## discrete.vars <- data.frame(mturk.df[, c("v.trend")])

<<<<<<< HEAD
## names(discrete.vars) <- "Predicted Wage"
## titles <- mturk.df[, "short.title"]

## sink("../../writeup/tables/continuous_vars.tex")
## tableContinuous(cont.vars, group = titles)
## sink()
=======
################################################
#
# Volume Trend Error
#
################################################

ex.indx <- which((!mturk.df$v.trend %in% c(-1:1)) | (mturk.df$know.job==""))

df.1 <- ddply(mturk.df[-ex.indx,], .(v.trend, know.job), summarise, predicted=length(WorkerId))
df.2 <- ddply(mturk.df[-ex.indx,], .(actual.v.trend), summarise, actual=length(WorkerId))

# calculate portion instead of sum
tots <- ddply(df.1, .(know.job), summarise, sum(predicted))
df.1 <- ddply(df.1, .(know.job, v.trend), summarise, predicted=predicted/tots[,2][tots$know.job==know.job])
df.2$actual <- df.2$actual/sum(df.2$actual)

df.trend <- merge(df.1, df.2, by.x="v.trend", by.y="actual.v.trend")
df.trend <- melt(data=df.trend, id.vars = c("v.trend","know.job"))
rm(df.1, df.2)

q <- ggplot(data=df.trend, aes(x=v.trend, y=value, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_x_continuous(labels=c("GoDown","StayTheSame","GoUp"), breaks=-1:1) +
  facet_grid(~know.job) + 
  ylab("Portion of responses") + xlab("Trend change") + ggtitle("Trends prediction w/social knowledge")

q
ggsave("../../writeup/plots/trends_prediction.png", q, width=8, height=5)

with(mturk.df, table(v.trend, v.trend.error))

# What explains error in trend.change prediction?

m.1 <- glm(I(v.trend.error>0) ~ social + know + log(tot.emp) + actual.mean.wage + v.trend, data = mturk.df, family="binomial")
m.2 <- lmer(I(v.trend.error>0) ~ social + know + (1|title), data = mturk.df)
m.3 <- lmer(I(v.trend.error>0) ~ social + know + (1|title) + (1|WorkerId), data = mturk.df)

screenreg(list(m.1, m.2, m.3))
>>>>>>> 737e11b304a92d4a6abb10010efc2635c2896602


##########################################
# Knowledge of what a job is, by wage band
##########################################
parameters <- list()
parameters[["N"]] <- length(unique(mturk.df$WorkerId))


# better cuts
mturk.df <- na.omit(mturk.df)

N.cuts <- 10
breaks <- ceiling(c(0, exp(with(mturk.df, quantile(log(actual.mean.wage), (1:N.cuts)/N.cuts)))))

mturk.df$band <- with(mturk.df, cut(actual.mean.wage, breaks))

df.know   <- ddply(mturk.df, .(band), summarise,
                   mu = mean(know, na.rm = TRUE),
                   se = sd(know, na.rm = TRUE)/sqrt(length(band)),
                   obs.type = "general")

df.social <- ddply(mturk.df, .(band), summarise,
                   mu = mean(know.someone, na.rm = TRUE),
                   se = sd(know.someone, na.rm = TRUE)/sqrt(length(band)),
                   obs.type = "social")

df.combined <- rbind(df.know, df.social)
df.combined$obs.type <- with(df.combined, factor(obs.type))

pos_dodge <- position_dodge(width = 0.3)

g.knowledge_by_wage <- ggplot(data = df.combined,
                              aes(x = band,
                                  y = mu,
                                  colour = obs.type,
                                  shape = obs.type,
                                  linetype = obs.type),
                              position = position_dodge()) +
    geom_point(position = pos_dodge) +
    geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.1, position = pos_dodge) +
    geom_line(aes(group = obs.type), position = pos_dodge) +
    theme_bw() +
    theme(legend.position = "top") +  
    xlab("Hourly wage bands (USD/Hour)") +
    ylab("Fraction of respondents") +
    scale_colour_manual(name = "Knowledge Type",
                        breaks = c("general", "social"),
                        labels = c("Knows what Job is", "Knows Someone \nwith that Job"),
                        values = c("red", "black")) +
            scale_shape_discrete(
                name = "Knowledge Type",
                breaks = c("general", "social"),
                labels = c("Knows what Job is", "Knows Someone \nwith that Job")
                ) +
         scale_linetype_discrete(
             name = "Knowledge Type",
             breaks = c("general", "social"),
             labels = c("Knows what Job is", "Knows Someone \nwith that Job")
             )
   
print(g.knowledge_by_wage)

pdf("../../writeup/plots/knowledge_by_wage.pdf", width = 8, height = 5)
 print(g.knowledge_by_wage)
dev.off()

###########################
# Defining prediction error
###########################

mturk.df$error.pct <- with(mturk.df,
                           scale(abs((log(actual.mean.wage) - log(predicted.wage))/log(actual.mean.wage))))
mturk.df$error <- with(mturk.df, scale(abs(log(actual.mean.wage) - log(predicted.wage))  ))

################################################
# Characterizing error in individual predictions 
################################################

# predictors of error

df.summary <- ddply(mturk.df, .(title), summarise,
                    mse.wage = mean(error),
                    mse.v.trend = mean(v.trend.error), 
                    mean.social = mean(social),
                    mean.know = mean(know),
                    tot.emp = mean(tot.emp),
                    avg.wage = mean(actual.mean.wage))

m.wage.pop <- lm(mse.wage ~ log(tot.emp), data = df.summary)
m.wage <- lm(mse.wage ~ log(tot.emp) + log(avg.wage), data = df.summary)
m.wage.i <- lm(mse.wage ~ log(tot.emp)*log(avg.wage), data = df.summary)
m.trend.pop <- lm(mse.v.trend ~ log(tot.emp), data = df.summary)
m.trend <- lm(mse.v.trend ~ log(tot.emp) + log(avg.wage), data = df.summary)
m.trend.i <- lm(mse.v.trend ~ log(tot.emp)*log(avg.wage), data = df.summary)

out.file <- "../../writeup/tables/occupation_error.tex"
sink(file = out.file)
stargazer(m.wage.pop, m.wage, m.wage.i, m.trend.pop, m.trend, m.trend.i,  
          title = "Occupation attributes and worker accuracy",
          label = "tab:occupation_accuracy",
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("adj.rsq", "res.dev", "f", "ser"),
          type = "latex") 
sink()
kill.lines(32, 33, out.file)
table.note <- "\\multicolumn{5}{p{0.80 \\linewidth}}{
\\emph{Notes:} This table reports descriptive regressions where the
depdendent variable is the MSE in respondent log wage prediction and the
 independent variables are the effects of social and general knowledge on a
respondent's prediction error, while controlling for worker and
occupation specfic effects. In Columns (1) and (2), the dependent
variable is MSE in wage prediction, while in Columns (3) and (4), the
dependent variable is an indicator for incorrectly predicting the
direction of employment growth in that occupation. Columns (1) and (3)
use worker and occupation fixed effects, with standard errors
clustered at the level of the respondent (the more converative
clustering choice), while in Columns (2) and (3), a multi-level model
with respondent and title random effects are used. \\starlanguage 
}"
insert.note(32, table.note, out.file)


## #  trend error 
## m.pop <- lm(v.trend.error ~ log(tot.emp), data = mturk.df)
## m.pop.know <- lm(v.trend.error ~ log(tot.emp) + know, data = mturk.df)
## m.pop.know.soc <- lm(v.trend.error ~ log(tot.emp) + know + social, data = mturk.df)
## m.ks <- lm(v.trend.error ~ log(tot.emp) + know + social + log(actual.mean.wage), data = mturk.df)

## out.file <- "../../writeup/tables/occupation_error_trend.tex"
## sink(file = out.file)
## stargazer(m.pop, m.pop.know, m.pop.know.soc, m.ks, 
##           title = "Occupation attributes and respondent accuracy about employment growth",
##           label = "tab:occupation_accuracy_v",
##           model.names = FALSE,
##           covariate.labels = c("Log total employment", "General Knowledge Index",
##               "Social Knowledge Index", "Log mean occupational wage"),  
##           dep.var.labels = c("Respondent Trend Error (1/0)"),
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           omit.stat = c("adj.rsq", "res.dev", "f", "ser"),
##           type = "latex") 
## sink()
## kill.lines(35, 36, out.file)
## table.note <- "\\multicolumn{5}{p{0.80 \\linewidth}}{
## \\emph{Notes:} TK This table reports the effects of social and general knowledge on a
## respondent's prediction error, while controlling for worker and
## occupation specfic effects. In Columns (1) and (2), the dependent
## variable is MSE in wage prediction, while in Columns (3) and (4), the
## dependent variable is an indicator for incorrectly predicting the
## direction of employment growth in that occupation. Columns (1) and (3)
## use worker and occupation fixed effects, with standard errors
## clustered at the level of the respondent (the more converative
## clustering choice), while in Columns (2) and (3), a multi-level model
## with respondent and title random effects are used. \\starlanguage 
## }"
## insert.note(35, table.note, out.file)




####################################################
# Correlation of Social Knowledge Across Disciplines 
####################################################


# For each occupation, what fraction of respondents know other occupatoin 

well.known <- unique(subset(ddply(mturk.df, .(title), summarise, mean.known = mean(know)), mean.known > 0.90)$title)
length(well.known)
df.cluster <- drop.levels(subset(mturk.df, title %in% well.known))
X <- xtabs(know.someone ~ WorkerId + title, data = df.cluster)

dim(X)

#sum(ddply(mturk.df, .(title), summarise, know.totals = sum(know.someone))$know.totals)
#sum(diag(X.dis))
#diag(X.dis)

X.dist.raw <- (t(X) %*% X)
X.dist <- X.dist.raw/diag(X.dist.raw)
d <- dist(X.dist)

hc <- hclust(d, members = NULL)

library(ape)
# plot basic tree

plot(as.phylo(hc), cex=0.9, type = "fan")

svg("../../writeup/plots/dendrogram.svg", width = 20, height = 20)
plot(as.phylo(hc), cex=0.9, type = "fan")
dev.off() 

pdf("../../writeup/plots/dendrogram.pdf", width = 20, height = 20)
plot(as.phylo(hc), cex=0.9, type = "fan")
dev.off()



#   o1 o2 o3 
# o1
# o2
# o3 

## exposed <- with(subset(mturk.df, title %in% well.known), table(WorkerId,title))

## X[exposed == 0] <- NA


## cor.X <- cor(X, use = "complete.obs")
## upperTriangle(cor.X, diag = TRUE) <- FALSE
## df.cor <- melt(cor.X)

## qplot(x=Var1, y=Var2, data=df.cor, fill=value, geom="tile") +
##     theme(axis.text.x = element_text(angle = 90, hjust = 1))

## # Worker | Occupation 1 | Occupation 2 | Occupation 3|
## #              1               0              1

####################################
# Worker and Occupation-Specific FEs
####################################

# write to stata to check VCE

## RE.READ <- TRUE
## if(RE.READ){
##     mturk.df <- readRDS("../../data/mturk_cooked.rds")
## }

clusterSE <- function(cluster.name, model, df){
    X <- model.matrix(model)
    df[, cluster.name] <- as.numeric(as.factor(df[,cluster.name]))
    clus <- cbind(X,df[,cluster.name],resid(model))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster.name,"resid")
    m <- dim(table(clus[,cluster.name])) # number of clusters
    n <- dim(X)[1]
    k <- dim(X)[2]
    dfc <- (m/(m-1))*((n-1)/(n-k)) # dof adjustment
    uclust <- matrix(NA, nrow = m, ncol = k) # uj matrix
    gs <- names(table(df[,cluster.name]))
    for(i in 1:m){
        uclust[i,] <- t(matrix(clus[clus[,cluster.name]==gs[i],k+2])) %*% clus[clus[,cluster.name]==gs[i],1:k]
    }
    se <- sqrt(diag(solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X)))*dfc)
    se
}

# "skeleton" regressions that will be plugged in w/ the correct coefs & se's from the FE regression 
m.skel.wage <- lm(error ~ social + know - 1, data = mturk.df)
m.skel.v.trend <- lm(v.trend.error ~ social + know - 1, data = mturk.df)

# FE estimates 
m.wage <- lm(error ~ social + know + factor(WorkerId) + factor(title) - 1, data = mturk.df)
#beta.wage = coef(m.wage)[1:2]
beta.wage <- coef(m.wage)
se.wage = sqrt(diag(vcov(m.wage)))[1:2]

m.v.trend <- lm(v.trend.error ~ social + know + factor(WorkerId) + factor(title) - 1, data = mturk.df)
#beta.v.trend = coef(m.v.trend)[1:2]
beta.v.trend = coef(m.v.trend)
se.v.trend = sqrt(diag(vcov(m.v.trend)))[1:2]

se.wage.cluster.worker <- clusterSE("WorkerId", m.wage, mturk.df)[1:2]
se.wage.cluster.title <- clusterSE("title", m.wage, mturk.df)[1:2]
se.v.trend.cluster.worker <- clusterSE("WorkerId", m.v.trend, mturk.df)[1:2]
se.v.trend.cluster.title <- clusterSE("title", m.v.trend, mturk.df)[1:2]

m.wage.re <- lmer(error ~ social + know -1 + (1|WorkerId) +(1|title),
                  data = mturk.df[, c("social", "error", "know", "WorkerId", "title")])
se.wage.re <- sqrt(diag(vcov(m.wage.re)))
beta.wage.re <- fixef(m.wage.re)

m.v.trend.re <- lmer(v.trend.error ~ social + know -1 + (1|WorkerId) +(1|title),
                  data = mturk.df)
se.v.trend.re <- sqrt(diag(vcov(m.v.trend.re)))

beta.v.trend.re <- fixef(m.v.trend.re)
old.names <- names(beta.v.trend.re)
beta.v.trend.re <- c(beta.v.trend.re, c(0, 0))
names(beta.v.trend.re) <- c(old.names, c("RE_WorkerId", "RE_title"))

# hack to get RE's to show up

# Try using the "keep" function 
out.file <- "../../writeup/tables/error_fe_estimate.tex" 
sink(out.file)
stargazer(m.wage, m.wage.re, m.v.trend, m.v.trend.re,
          title = "Effects of social and general knowledge on occupational wage and trajectory estimate errors",
          label = "tab:fe_errors",
          float.env = "table", 
          column.labels = c("MSE Wage Error", "Trend Error (1/0)"),
          model.names = FALSE, 
          column.separate = c(2,2),
          dep.var.labels.include = FALSE, 
          coef = list(beta.wage, beta.wage.re, beta.v.trend, beta.v.trend.re),
          se = list(se.wage.cluster.worker, se.wage.re, se.v.trend.cluster.worker, se.v.trend.re),
          covariate.labels = c("Social Knowledge Index", "General Knowledge Index"),
          omit = c("WorkerID*", "title*"),
          omit.labels = c("Worker FE", "Occupation FE"),
          omit.stat = c("aic", "f", "adj.rsq", "ll", "bic", "ser"), 
          type = "latex")
sink()

insert.note(24, note = "Worker RE & No & Yes & No & Yes \\\\ \n", file = out.file)
insert.note(25, note = "Title RE & No & Yes & No & Yes \\\\ \n", file = out.file)
kill.lines(31, 32, out.file)
table.note <- "\\multicolumn{5}{p{0.80 \\linewidth}}{
\\emph{Notes:} This table reports the effects of social and general knowledge on a
respondent's prediction error, while controlling for worker and
occupation specfic effects. In Columns (1) and (2), the dependent
variable is MSE in wage prediction, while in Columns (3) and (4), the
dependent variable is an indicator for incorrectly predicting the
direction of employment growth in that occupation. Columns (1) and (3)
use worker and occupation fixed effects, with standard errors
clustered at the level of the respondent (the more converative
clustering choice), while in Columns (2) and (3), a multi-level model
with respondent and title random effects are used. \\starlanguage 
}"

insert.note(31, table.note, out.file)

mturk.df$title <- with(mturk.df, reorder(as.character(title), actual.mean.wage, mean))
qplot(title, actual.mean.wage, data = mturk.df) + coord_flip()

## # First, do SE's "by hand"
## u <- m.v.trend$resid 
## X <- model.matrix(m.v.trend)
## Sigma <- u %*% t(u)
## dim(Sigma)
## n <- dim(X)[1]
## k <- dim(X)[2]
## sigma <- sqrt( sum(diag(Sigma))/(n - k) ) # dof adjustment 
## V <- solve(t(X) %*% X)*(sigma**2)
## se <- sqrt(diag(V))[1:2]
## # Robust Standard Errors
## u <- matrix(resid(m.v.trend))
## # meat part Sigma is a diagonal with u^2 as elements
## meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
## dfc <- n/(n-k)   # degrees of freedom adjust
## se.robust <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))[1:2]

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

writeImage(g.predicted.v.actual, "predicted_v_actual_RAW", height = 6, width = 6)

##################
# OUTLIER RUN-DOWN
##################

## df.wage$title <- with(df.wage, reorder(title, error, mean))
## df.wage$mistake.type <- with(df.wage, factor(ifelse(error > 0, "Underestimate", "Overestimate")))

## g.error.type <- ggplot(data = df.wage, aes(x = error, y = title)) + geom_point(aes(colour = mistake.type)) 

## #print(g.error.type)

## pdf("../../writeup/plots/error_type.pdf", width = 7, height = 10)
##  print(g.error.type)
## dev.off()

############################
# Knowledge of jobs by title
############################

df.know <- na.omit(subset(df.wage, log(tot.emp) > 13))
df.know$title <- as.factor(df.know$title)

df.know$title <- with(df.know, reorder(title, know.mu, mean))

g.know <- ggplot(data = df.know,
                 aes(y = title, x = know.mu)) +
    geom_point() +
    xlab("Mean of (Do you know what this job is? -1 = No, 0 = Maybe, 1 = Yes") +
    ylab("") +
    theme_bw()

print(g.know)

pdf("../../writeup/plots/knowledge_by_occupation.pdf", width = 7, height = 10)
print(g.know)
dev.off()

#######################
# Know someone by title 
#######################

df.wage$title <- with(df.wage, reorder(title, social.mu, mean))

g.social <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),
                 aes(y = title, x = social.mu)) + geom_point() +
    xlab("Mean of social index") +
    ylab("") +
    theme_bw()

print(g.social)

pdf("../../writeup/plots/social_by_occupation.pdf", width = 7, height = 10)
print(g.social)
dev.off()

#################
# WAGE PREDICTION 
#################

## shortenName <- function(x, n){
##    abbreviate(x, n)
## }

## g.scatter <- ggplot(data = mturk.df, aes(x = log(actual.mean.wage), log(predicted.wage))) +
##     geom_point() +
##     geom_smooth() +
##     theme_bw() + geom_abline(a = 1, b = 0) +
##     xlab("Actual log hourly wage") +
##     ylab("Predicted log hourly wage")

## print(g.scatter)

## pdf("../../writeup/plots/prediction_scatter_RAW.pdf", width = 12, heigh = 7)
## print(g.scatter)
## dev.off()

## #mturk.df$title <- with(mturk.df, sapply(as.character(title), function(x) shortenName(x, 20)))

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

#############################
# Box plots of wage knowledge 
#############################

## getNationalMeasures <- function(field.name, title.name = "title"){
##   l <- national.df[, field.name]
##   names(l) <- national.df[, "OCC_TITLE"]
##   l <- as.list(l)
##   as.numeric(as.character(l[ mturk.df[,title.name] ]))
## }

## mturk.df <- within(mturk.df, {
##   actual.mean.wage <- getNationalMeasures("H_MEAN")
##   actual.median.wage <- getNationalMeasures("H_MEDIAN")
##   tot.emp <- getNationalMeasures("TOT_EMP")
##   actual.v.trend <- getNationalMeasures("v.trend")
##   v.trend.error <- abs(actual.v.trend - v.trend)
##   pct.mean.error <- abs((actual.mean.wage - predicted.wage)/actual.mean.wage)
##   prediction.delta <- log(actual.mean.wage) - log(predicted.wage)
##   know.someone <- I(!is.na(social.knowledge) & social.knowledge != "0")
##   error <- abs( log(actual.mean.wage) - log(predicted.wage) )
## })


###############################################
## Save data for knitr report
###############################################

save(national.df, file="../../knitr/bls.data")
save(mturk.df, file="../../knitr/mturk.data")

###############################################


################################################
#
# Volume Trend Error
#
################################################
## with(mturk.df, table(v.trend, v.trend.error))

## # ggplot(mturk.df, aes(x=v.trend.error, y=error)) + geom_point()
## #   geom_histogram(position="dodge", binwidth=0.5)

## m.1 <- glm(I(v.trend.error>0) ~ social + know + log(tot.emp) + actual.mean.wage, data = mturk.df, family="binomial")
## m.2 <- lmer(I(v.trend.error>0) ~ social + know + (1|title), data = mturk.df)
## m.3 <- lmer(I(v.trend.error>0) ~ social + know + (1|title) + (1|WorkerId), data = mturk.df)

## stargazer(m.1, type = "text")

#screenreg(list(m.1, m.2, m.3))


##########################################
# Knowledge of what a job is, by wage band
##########################################

## mturk.df$band <- with(mturk.df, cut(actual.mean.wage, 10))
## df.know   <- ddply(mturk.df, .(band), summarise,
##                    mu = mean(know, na.rm = TRUE),
##                    se = sd(know, na.rm = TRUE)/sqrt(length(band)),
##                    obs.type = "Knows what Job is")
## df.social <- ddply(mturk.df, .(band), summarise,
##                    mu = mean(know.someone, na.rm = TRUE),
##                    se = sd(know.someone, na.rm = TRUE)/sqrt(length(band)),
##                    obs.type = "Knows Someone \nwith that Job")
## df.combined <- rbind(df.know, df.social)

## pos_dodge <- position_dodge(width = 0.3)
## g.knowledge_by_wage <- ggplot(data = df.combined, aes(x = band, y = mu, colour = factor(obs.type)), position = position_dodge()) +
##   geom_point(position = pos_dodge) +
##   geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.1, position = pos_dodge) +
##   geom_line(aes(group = obs.type), position = pos_dodge) + 
##   theme_bw() +
##   xlab("Hourly wage bands") +
##   ylab("Fraction of respondents") 

## pdf("../../writeup/plots/knowledge_by_wage.pdf", width = 8, height = 5)
## print(g.knowledge_by_wage)
## dev.off()



df.overlay <- rbind(ddply(mturk.df, .(title), summarise, sum.wage = actual.mean.wage[1], obs.type = "mean"),
                ddply(mturk.df, .(title), summarise, sum.wage = actual.median.wage[1], obs.type = "median"))


#df.overlay$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))



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
    

print(g.box.plot)

writeImage(g.box.plot, "box_plots_by_occupation", width = 7, height = 10)

# Estimated trends in wages 
g.wage.trends <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),  aes(y = title, x = w.trend.mu)) +
  geom_point() +
  theme_bw() 

########################################
# Is social knowledge clustered by wage? 
########################################

# For each observation, calculate the mean wage for other jobs evaluated by that worker where they knew someone with the job.
# Then see how the mean wage of the jobs they knew about is predictive.
# What jobs the worker got exposed to is a random draw. 

mturk.df$know.someone <- with(mturk.df, social.knowledge != "0")

by.worker.df <- ddply(mturk.df, .(WorkerId), transform,
                      num.known = sum(know.someone),
                      num.obs = length(WorkerId),
                      z = sum(log(actual.mean.wage[know.someone])))

by.worker.df$mean.wage.others.social <- with(by.worker.df,
                                             ifelse(know.someone,
                                                    (z - log(actual.mean.wage))/(num.known - 1), z/(num.known)))

# Fake version for placebo test 
mturk.df$fake.know.someone <- with(mturk.df, sample(know.someone, size = length(know.someone)))

by.worker.df.fake <- ddply(mturk.df, .(WorkerId), transform,
                      num.known = sum(fake.know.someone),
                      num.obs = length(WorkerId),
                      z = sum(log(actual.mean.wage[fake.know.someone])))

by.worker.df.fake$mean.wage.others.social <- with(by.worker.df.fake,
                                                  ifelse(fake.know.someone, (z - log(actual.mean.wage))/(num.known - 1),
                                                         z/(num.known)))


m.real <- lm(know.someone ~ log(actual.mean.wage) * log(mean.wage.others.social) + num.obs, data = by.worker.df)
m.fake <- lm(fake.know.someone ~ log(actual.mean.wage) * log(mean.wage.others.social) + num.obs, data = by.worker.df.fake)

out.file <- "../../writeup/tables/clustering.tex"
sink(out.file)
stargazer(m.real, m.fake, 
          title = "General Knowledge Index, conditioned upon wages of other known occupations",
          label = "tab:clustering",
          covariate.labels = c("Actual occupation wage, $\\log \\bar{w}_i$",
              "Mean log wage of other known occupations, $\\log \\bar{w}^S_{-i}$",
              "Number of observations by worker, $\\sum_i 1$", 
              "Interaction term: $\\log \\bar{w}_i \\times \\log \\bar{w}^S_{-i}$"
              ),
          type = "latex"
          )
sink()
kill.lines(38, 39, out.file)
table.note <- "\\multicolumn{3}{p{0.95 \\linewidth}}{
\\emph{Notes:} TK This table reports the effects of social and general knowledge on a
respondent's prediction error, while controlling for worker and
occupation specfic effects. In Columns (1) and (2), the dependent
variable is MSE in wage prediction, while in Columns (3) and (4), the
dependent variable is an indicator for incorrectly predicting the
direction of employment growth in that occupation. Columns (1) and (3)
use worker and occupation fixed effects, with standard errors
clustered at the level of the respondent (the more converative
clustering choice), while in Columns (2) and (3), a multi-level model
with respondent and title random effects are used. \\starlanguage 
}"
insert.note(38, table.note, out.file)

############################
# Knowledge of jobs by title
############################

df.wage$title <- with(df.wage, reorder(title, know.mu, mean))

g.know <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),
                 aes(y = title, x = know.mu)) +
  geom_point() +
  xlab("Mean of (Do you know what this job is? -1 = No, 0 = Maybe, 1 = Yes") +
  ylab("") +
  theme_bw()

writeImage(g.know, "knowedge_by_occupation", width = 7, height = 10)

#######################
# Know someone by title 
#######################

df.wage$title <- with(df.wage, reorder(title, social.mu, mean))

g.social <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),
                   aes(y = title, x = social.mu)) + geom_point() +
  xlab("Mean of social index") +
  ylab("") +
  theme_bw()

writeImage(g.know, "social_by_occupation", width = 7, height = 10)

createLaTeXparameters(parameters, "../../writeup/parameters.tex")

##########################
# NOT SURE ABOUT THIS PART
##########################

## ## Function returns basic stats for variable
## ## Statistics are comparable to OES data

## national.stats <- function(x){
##   c(h.mean=mean(x),
##     h.prse=100*sd(x)/mean(x),
##     h.pct=quantile(x,c(0.10,0.25,0.50,0.75,0.90)))
## }


## mturk.df <- import.mturk("../../data/mturk_output.csv")

## mturk.df[, "WorkerId"] <- NULL

## #agg <- aggregate(.~title, data = mturk.df[,c("title", "Answer.wage")], FUN=function(x)national.stats(x))


## agg <- ddply(mturk.df, .(title), summarise,
##              c(h.mean = mean(Answer.wage),
##                h.prse=100*sd(Answer.wage)/mean(Answer.wage))
## )


## agg <- ddply(mturk.df, .(title), summarise, 
##              H_MEAN = mean(Answer.wage), 
##              MEAN_PRSE = 100 * sd(Answer.wage)/mean(Answer.wage),
##              H_PCT10 = quantile(Answer.wage, 0.10),
##              H_PCT25 = quantile(Answer.wage, 0.25),
##              H_PCT_MEDIAN = quantile(Answer.wage, 0.50),
##              H_PCT75 = quantile(Answer.wage, 0.75),
##              H_PCT90 = quantile(Answer.wage, 0.90))

## #df1 <- cbind(agg$title, as.data.frame(agg$h.mean))
## df1 <- agg
## df2 <- national.df[,c(-1,-3,-4)]

## names(df1) <- names(df2)

## l.err <- aggregate(.~title, data=mturk.df[,c(1,12)], mean)[,2]
## names(l.err) <- sort(national.df$OCC_TITLE)
## l.err <- as.list(l.err)

## df1$error <- as.numeric(l.err[df1$OCC_TITLE])
## df2$error <- as.numeric(l.err[df2$OCC_TITLE])

## df1$var <- "MTOS"
## df2$var <- "OES"

## df <- rbind(df1, df2)
## ## Set order for datasources
## df$var <- factor(df$var, levels=c("OES", "MTOS"))
## rm(df1,df2,agg,l.err)

## ##
## ## Function to shorten strings
## ##
## shorten.str <- function(x, N=20){
##   x <- as.character(x)
##   res <- paste0(substr(x,1,N), ifelse(nchar(x)>N,"...",""))
##   res
## }

## ##
## ## Function to create custom boxplots from merged dataset
## ##
## wage.boxplots <- function(df, all.jobs){
  
##   ## Filtering data
##   df.f <- df[df$OCC_TITLE %in% all.jobs, ]
  
##   ## Shorten string
##   df.f$OCC_TITLE <- shorten.str(df.f$OCC_TITLE)
  
##   ## Ordering facets on median OES wage
##   df.f <- df.f[with(df.f, order(var, -H_MEDIAN)), ]
##   df.f$OCC_TITLE <- factor(df.f$OCC_TITLE, levels=df.f$OCC_TITLE[df.f$var=="OES"])
  
##   ## Plotting
##   p <- ggplot(df.f, aes(x=OCC_TITLE, ymin=`H_PCT10`, lower=`H_PCT25`, middle=`H_MEDIAN`, 
##                         upper=`H_PCT75`, ymax=`H_PCT90`)) 
##   p <- p + geom_boxplot(aes(fill=var), stat="identity") 
##   p <- p + theme(axis.text.x=element_blank())
##   p <- p + facet_wrap( ~ OCC_TITLE, scales="free_x")
##   p <- p + xlab("Boxplots are constructed based on 10%,25%,50%,75% and 90% percentiles") + 
##     ggtitle("OES and MTOS Wages")
##   p
## }

## ## Filter some top-paid jobs
## all.jobs <- national.df[with(national.df, order(-H_MEAN)), ]$OCC_TITLE[1:9]
## top.paid <- wage.boxplots(df, all.jobs)
## top.paid
## ggsave("../../writeup/plots/top.paid.png", top.paid, width=8, height=5)

## ## Filter som low-paid jobs
## all.jobs <- national.df[with(national.df, order(H_MEAN)), ]$OCC_TITLE[1:16]
## low.paid <- wage.boxplots(df, all.jobs)
## low.paid
## ggsave("../../writeup/plots/low.paid.png", low.paid, width=8, height=5)

## ## Filter selected jobs
## all.jobs <- national.df$OCC_TITLE[str_detect(national.df$OCC_TITLE, ignore.case("computer"))]
## computer.jobs <- wage.boxplots(df, all.jobs)
## computer.jobs
## ggsave("../../writeup/plots/computer.jobs.png", computer.jobs, width=8, height=5)


## ## Find jobs with lowest/highest MSE
## df.ord <- df[with(df, order(error)), ]
## df.ord <- (df.ord[df.ord$var=="OES", ])[1:10, c("OCC_TITLE", "H_MEAN", "error")]

## low.error.jobs <- wage.boxplots(df, df.ord$OCC_TITLE[1:9])
## low.error.jobs
## ggsave("../../writeup/plots/low.error.jobs.png", low.error.jobs, width=8, height=5)


## df.ord <- df[with(df, order(-error)), ]
## df.ord <- (df.ord[df.ord$var=="OES", ])[1:10, c("OCC_TITLE", "H_MEAN", "error")]

## high.error.jobs <- wage.boxplots(df, df.ord$OCC_TITLE[1:9])
## high.error.jobs
## ggsave("../../writeup/plots/high.error.jobs.png", high.error.jobs, width=8, height=5)


## # Export to LaTeX parameters.tex file for by-name inclusion 

          
