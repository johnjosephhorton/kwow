#! /usr/bin/Rscript --vanilla

###############################################
# Author: John J. Horton 
# Created Date: 2013-11-07 
# Project: kwow 
###############################################

##
## !!! NOTE: Comment this line when building project !!!
##
# setwd("./code/R")

##
## Clearing the workspace
##
rm(list=ls(all=TRUE))
gc(reset=TRUE)
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

import.top.national <- function(fname, TOP.N=100, OCC.CAT="broad"){
  
  if(!file.exists(fname)){
    print(sprintf("Sorry! The file '%s' does not exist!", fname))
    return(NULL)
  }
  
  ## Replaces '*' and '**' with NA
  df.raw <- read.table(fname, header=T, sep=",", fill=T, na.strings=c("*", "**"))
  ## Filter for category(-ies)
  df <- df.raw[df.raw$OCC_GROUP %in% OCC.CAT,]
  
  ## Masks all columns with numerical values
  convert.cols <- colnames(df)[c(-1,-2,-3,-19,-20)]
  ## Masks columns with hourly and annual data respectively
  cols.hourly <- colnames(df)[c(6,9:13)]
  cols.annual <- colnames(df)[c(7,14:18)]
  ## Replaces comma in numbers
  df[,convert.cols] <- as.data.frame(sapply(df[,convert.cols], gsub, pattern=",", replacement=""))
  ## Replaces '#' with Inf
  ## df[,convert.cols] <- as.data.frame(sapply(df[,convert.cols], gsub, pattern="#", replacement=Inf))
  
  ## Replace '#' with 90.00 for hourly data
  df[,cols.hourly] <- as.data.frame(sapply(df[,cols.hourly], gsub, pattern="#", replacement="90.00"))
  ## Replace '#' with 187'200 for annual data
  df[,cols.annual] <- as.data.frame(sapply(df[,cols.annual], gsub, pattern="#", replacement="187200"))
  
  ## Double convert factor-to-numeric
  df[,convert.cols] <- sapply(df[,convert.cols], as.character)
  df[,convert.cols] <- sapply(df[,convert.cols], as.numeric)
  ## Convert first 3 columns to characters
  df[,1:3] <- sapply(df[,1:3], as.character)
  
  ## For annually-paid jobs converts annual wages to hourly
  df[!is.na(df$ANNUAL) & df$ANNUAL==T, cols.hourly] <-
    df[!is.na(df$ANNUAL) & df$ANNUAL==T, cols.annual]/2080

  df$imputed.hourly <- with(df, !is.na(df$ANNUAL) & df$ANNUAL==T)
  
  ## Order by Total employment (NOTE: original data is already ordered)
  df <- df[with(df, order(-TOT_EMP)), ]
  ## Select TOP.N records
  df <- df[1:ifelse(nrow(df)>TOP.N, TOP.N, nrow(df)), ]
  rownames(df) <- 1:nrow(df)

  print(colnames(df))
  df[,c("OCC_CODE", "OCC_TITLE", "TOT_EMP", "EMP_PRSE", "H_MEAN", "MEAN_PRSE", 
"H_PCT10", "H_PCT25", "H_MEDIAN", "H_PCT75", "H_PCT90", "imputed.hourly")]
  
}


###############################################
## Function to import Data from MTurk Survey
##
## ARGS:
## fname - path to CSV file with original data
##
###############################################

import.mturk <- function(fname){
  
  if(!file.exists(fname)){
    print(sprintf("Sorry! The file '%s' does not exist!", fname))
    return(NULL)
  }
  df.raw <- read.table(fname, header=T, sep=",", fill=T)
  df <- df.raw[, c("Input.Title",
                   "Answer.know_job",
                   "Answer.know_anyone",
                   "Answer.wage",
                   "Answer.volume_trend",
                   "Answer.wage_trend",
                   "Answer.comment",
                   "WorkerId")]
  
  df[,"Answer.wage"] <- as.data.frame(sapply(df[,"Answer.wage"], gsub, pattern="++", replacement="", fixed=T))
  df[,"Answer.wage"] <- sapply(df[,"Answer.wage"], as.character)
  df[,"Answer.wage"] <- sapply(df[,"Answer.wage"], as.numeric)
  
  char.cols <- c("Input.Title","Answer.comment")
  df[,char.cols] <- sapply(df[,char.cols], as.character)
  
  # Reformulate anyone not as factor
  df$know <- as.numeric(as.character(with(df, factor(Answer.know_job,
                                                       levels = c("No", "Maybe", "Yes"),
                                                       labels = c("-1","0","1")))))
  
  # Reformulate social not as factor
  df$social <- as.numeric(as.character(with(df, factor(Answer.know_anyone,
                                                         levels = c("0", "1", "2", "3_10", "10_plus"),
                                                         labels = c("-2","-1","0","1","2")))))

  df$w.trend <- as.numeric(as.character(with(df,
                                              factor(Answer.wage_trend,
                                                     levels = c("GoDown", "StayTheSame", "GoUp"),
                                                     labels = c("-1","0","1")))))

    
  df
}

national.df <- import.top.national("../../data/national_M2012_dl.csv", 99)

## Libraries
library(xtable)
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(lme4)
library(memisc)
library(plyr)

#  If JJHmisc is missing, run: 
#  library(devtools)
#  install_github("JJHmisc", "johnjosephhorton")
library(JJHmisc)

##
## Data from Bureau of Labor Statistics, Department of Labor
##

national.df <- import.top.national("../../data/national_M2012_dl.csv", 99)

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

SHORT.TITLE.LENGTH <- 30
mturk.df$short.title <- with(mturk.df, abbreviate(title, SHORT.TITLE.LENGTH))

getNationalMeasures <- function(field.name, title.name = "title"){
    l <- national.df[, field.name]
    names(l) <- national.df[, "OCC_TITLE"]
    l <- as.list(l)
    as.numeric(as.character(l[ mturk.df[,title.name] ]))
}

mturk.df <- within(mturk.df, {
    actual.mean.wage <- getNationalMeasures("H_MEAN")
    actual.median.wage <- getNationalMeasures("H_MEDIAN")
    tot.emp <- getNationalMeasures("TOT_EMP")
    pct.mean.error <- abs((actual.mean.wage - predicted.wage)/actual.mean.wage)
    prediction.delta <- log(actual.mean.wage) - log(predicted.wage)
    know.someone <- I(!is.na(social.knowledge) & social.knowledge != "0")
})

##########################################
# Knowledge of what a job is, by wage band
##########################################

mturk.df$band <- with(mturk.df, cut(actual.mean.wage, 10))
df.know   <- ddply(mturk.df, .(band), summarise,
                   mu = mean(know, na.rm = TRUE),
                   se = sd(know, na.rm = TRUE)/sqrt(length(band)),
                   obs.type = "Knows what Job is")
df.social <- ddply(mturk.df, .(band), summarise,
                   mu = mean(know.someone, na.rm = TRUE),
                   se = sd(know.someone, na.rm = TRUE)/sqrt(length(band)),
                   obs.type = "Knows Someone \nwith that Job")
df.combined <- rbind(df.know, df.social)

pos_dodge <- position_dodge(width = 0.3)
g.knowledge_by_wage <- ggplot(data = df.combined, aes(x = band, y = mu, colour = factor(obs.type)), position = position_dodge()) +
    geom_point(position = pos_dodge) +
    geom_errorbar(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), width = 0.1, position = pos_dodge) +
    geom_line(aes(group = obs.type), position = pos_dodge) + 
    theme_bw() +
    xlab("Hourly wage bands") +
    ylab("Fraction of respondents") 

pdf("../../writeup/plots/knowledge_by_wage.pdf", width = 8, height = 5)
 print(g.knowledge_by_wage)
dev.off()

################################################
# Characterizing error in individual predictions 
################################################

qplot(pct.error, prediction.delta, data = mturk.df)

mturk.df$error <- with(mturk.df, abs( log(actual.mean.wage) - log(predicted.wage) ))

m.1 <- lm(error ~ social + know + log(tot.emp), data = mturk.df)
m.2 <- lmer(error ~ social + know + (1|title), data = mturk.df)
m.3 <- lmer(error ~ social + know + (1|title) + (1|WorkerId), data = mturk.df)

mtable(m.1, m.2, m.3)

models <- list()
renames <- list()
renames[["log(tot.emp)"]] <- "Log total employment"

renames[["know"]] <- "Knows what job consists of"
renames[["social"]] <- "Knows someone with the job"

regression.table(list("(1)" = m.1, "(2)" = m.2, "(3)" = m.3), renames, "../../writeup/tables/error_prediction.tex")

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
                           
g.predicted.v.actual <- ggplot(data = df.wage, aes(x = log(mturk.w), y = log(oes.w))) +
    geom_point() +
    geom_smooth() +
    geom_abline(a = 1, b = 0, linetype = "dashed") +
    geom_text(data = df.wage.outliers, aes(label = title), size = 2, vjust = -1) + theme_bw()

pdf("../../writeup/plots/predicted_v_actual.pdf")
print(g.predicted.v.actual)
dev.off()

##################
# OUTLIER RUN-DOWN
##################

df.wage$title <- with(df.wage, reorder(title, error, mean))
df.wage$mistake.type <- with(df.wage, factor(ifelse(error > 0, "Underestimate", "Overestimate")))

g.error.type <- ggplot(data = df.wage, aes(x = error, y = title)) + geom_point(aes(colour = mistake.type)) 
pdf("../../writeup/plots/error_type.pdf", width = 7, height = 10)
 print(g.error.type)
dev.off()

# Trends
qplot(oes.w, w.trend.mu, data = df.wage)

df.wage$Input.Title <- with(df.wage, reorder(Input.Title, w.trend.mu, mean))

# Estimated trends in wages 
g.wage.trends <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),  aes(y = Input.Title, x = w.trend.mu)) +
    geom_point() +
    theme_bw() 

ggsave("../../writeup/plots/wage_trends.png", g.wage.trends, width=7, height=10)

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

shortenName <- function(x, n){
   abbreviate(x, n)
}

g.scatter <- ggplot(data = mturk.df, aes(x = log(actual.mean.wage), log(Answer.wage))) +
    geom_point() +
    geom_smooth() +
    theme_bw() + geom_abline(a = 1, b = 0) +
    xlab("Actual log hourly wage") +
    ylab("Predicted log hourly wage")

print(g.scatter)

pdf("../../writeup/plots/prediction_scatter.pdf", width = 12, heigh = 7)
print(g.scatter)
dev.off()

mturk.df$title <- with(mturk.df, sapply(as.character(Input.Title), function(x) shortenName(x, 20)))

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

#############################
# Box plots of wage knowledge 
#############################

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

g.box.plot <- ggplot(data = mturk.df,
                     aes(x = title, y = log(predicted.wage))) +
    geom_boxplot(outlier.size = 0) +
    geom_point(aes(y = log(actual.mean.wage), colour = "red")) +
    geom_point(aes(y = log(actual.median.wage), colour = "blue")) +
    ylab("BLS Occupation") + xlab("Log hourly wage") +
    coord_flip() + theme_bw() +
    theme(text=element_text(size = 10))

print(g.box.plot)

pdf("../../writeup/plots/box_plots_by_occupation.pdf", width = 7, heigh = 10)
print(g.box.plot)
dev.off()

##########################################
# Regression approach to quantifying error
##########################################


m.lm <- lm(error ~ log(TOT_EMP) + know + social + log(actual.mean.wage), data = mturk.df)


m.lmer <- lmer(error ~ log(TOT_EMP) + know + social + log(actual.mean.wage) + (1|WorkerId), data = mturk.df)
mtable(m.lm, m.lmer)

m.lmer <- lmer(error ~ log(TOT_EMP) + know + social + (1|WorkerId), data = mturk.df)



########################################
# Is social knowledge clustered by wage? 
########################################

# For each observation, calculate the mean wage for other jobs evaluated by that worker where they knew someone with the job.
# Then see how the mean wage of 


by.worker.df <- ddply(mturk.df, .(WorkerId), transform,
                      num.known = sum(social > -2), 
                      num.obs = length(WorkerId),
                      z = sum(log(actual.mean.wage[social > -2])))

by.worker.df$mean.wage.others.social <- with(by.worker.df, ifelse(social > -2, (z - log(actual.mean.wage))/(num.known - 1), z/(num.known)))

m.1 <- lm(I(social > -2) ~ log(actual.mean.wage) * mean.wage.others.social + log(tot.emp), data = by.worker.df)
m.2 <- lmer(I(social > -2) ~ log(actual.mean.wage) * mean.wage.others.social + log(tot.emp) + (1|WorkerId), data = by.worker.df)
m.3 <- lmer(I(social > -2) ~ log(actual.mean.wage) * mean.wage.others.social + (1|WorkerId) + (1|title), data = by.worker.df)

models <- list("(1)" = m.1, "(2)" = m.2, "(3)"  = m.3)
renames <- list() 
regression.table(models, renames, "../../writeup/tables/clustering.tex")

      
###########################
# SCATTER PLOT - NEEDS WORK
###########################

df.goofs <- subset(df.wage, log(tot.emp) > 13 & abs(log(mturk.w) - log(oes.w)) > .40)

ggplot(data = df.wage, aes(
           x = log(mturk.w),
           y = log(oes.w))) + geom_point(aes(size = log(tot.emp))) +
                                            geom_smooth(method = "lm") + geom_abline(a = 1, b = 0) +
    geom_text(data = df.goofs,
              aes(label = Input.Title))

# what predicts error rate? FE approach
m <- lm(error ~ Answer.know_anyone + Answer.know_job + log(TOT_EMP), data = mturk.df)
summary(m)


# What job titles do people find confusing?

m <- lmer(know ~ (1|Input.Title) + log(TOT_EMP), data = mturk.df)

df.resid <- data.frame(ranef(m)$Input.Title)

colnames(df.resid) <- 'Residual'

df.resid$job <- factor(rownames(df.resid))
                                        
df.resid$job <- with(df.resid, reorder(job, Residual, mean))

ggplot(data = df.resid, aes(x = Residual, y = job)) + geom_point() 

qplot(log(TOT_EMP), social, data = mturk.df) + geom_smooth() 

# How knowledge about job (ratio of awared respondents) is related to TOT_EMP
tb <- with(mturk.df, table(cut_interval(TOT_EMP, 10), I(know==1)))
tb <- data.frame(dont=tb[,1], know=tb[,2], ratio=tb[,2]/(tb[,1]+tb[,2]))
q <- ggplot(data=tb, aes(x=1:10, y=ratio)) + geom_smooth() + geom_point() + xlab("TOT_EMP") +
        scale_x_discrete(limits=levels(cut_interval(log(mturk.df$TOT_EMP),10))) +
    theme_bw() 
q
ggsave("../../writeup/plots/knowledge_emp.png", q, width=8, height=5)


# Additional var for both know and social
mturk.df$qualified <- with(mturk.df, know>0 | social>-2)

m.error.emp <- lm(error ~ know*social + log(TOT_EMP), data = mturk.df)
m.error <- lm(error ~ know*social, data = mturk.df)

mtable(m.error, m.error.emp)

##
## Error of prediction models
##
m <- lm(error ~ Answer.know_anyone + Answer.know_job + log(TOT_EMP) + actual.mean.wage, data = mturk.df)
summary(m)

m1 <- glm(error ~ know*social + log(TOT_EMP) + log(actual.mean.wage), data = mturk.df)
m2 <- glm(error ~ know*social + log(actual.mean.wage), data = mturk.df)
m3 <- glm(error ~ social + log(actual.mean.wage) + log(TOT_EMP), data=mturk.df)
m4 <- lmer(error ~ know*social + log(TOT_EMP) + log(actual.mean.wage) + (1|Input.Title) , data = mturk.df)
m5 <- glm(error ~ know*social + log(TOT_EMP) + log(actual.mean.wage) + I(log(Answer.wage)>3), data = mturk.df)

models <- list("1" = m1,
               "2" = m2,
               "3" = m3,
               "4" = m4,
               "5" = m5)

regression.table(models, list(), "../../writeup/tables/models_error.tex")

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) + geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) 

# What about when they know the job? 
ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_job, ncol = 3)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_anyone, ncol = 3)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
  geom_point() +
     geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~qualified, ncol = 3)


m1 <- lm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df)
m2 <- glm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df, family="binomial")

regression.table(list("1" = m1, "2" = m2), list(), "../../writeup/tables/social_knowledge.tex")


qplot(Answer.know_anyone, TOT_EMP, data = mturk.df) + geom_boxplot()


# MTurks know jobs with lower wages and mass employment?
m1 <- glm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df, family="binomial")
m2 <- glm(I(Answer.know_anyone != "0") ~ log(actual.mean.wage), data = mturk.df, family="binomial")
m3 <- glm(I(Answer.know_anyone != "0") ~ log(actual.mean.wage) + TOT_EMP, data = mturk.df, family="binomial")

regression.table(list("1" = m1, "2" = m2, "3" = m3), list(), "../../writeup/tables/models_know_anyone.tex")

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~know, ncol=2)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) 


##
## Function returns basic stats for variable
## Statistics are comparable to OES data
##
national.stats <- function(x){
  c(h.mean=mean(x),
    h.prse=100*sd(x)/mean(x),
    h.pct=quantile(x,c(0.10,0.25,0.50,0.75,0.90)))
}

##
## Merging of two datasets (OES & MTSO)
##


mturk.df <- import.mturk("../../data/mturk_output.csv")

mturk.df[, "WorkerId"] <- NULL

#agg <- aggregate(.~Input.Title, data = mturk.df[,c("Input.Title", "Answer.wage")], FUN=function(x)national.stats(x))


agg <- ddply(mturk.df, .(Input.Title), summarise,
             c(h.mean = mean(Answer.wage),
               h.prse=100*sd(Answer.wage)/mean(Answer.wage))
)


agg <- ddply(mturk.df, .(Input.Title), summarise, 
            H_MEAN = mean(Answer.wage), 
            MEAN_PRSE = 100 * sd(Answer.wage)/mean(Answer.wage),
            H_PCT10 = quantile(Answer.wage, 0.10),
            H_PCT25 = quantile(Answer.wage, 0.25),
            H_PCT_MEDIAN = quantile(Answer.wage, 0.50),
            H_PCT75 = quantile(Answer.wage, 0.75),
            H_PCT90 = quantile(Answer.wage, 0.90))

#df1 <- cbind(agg$Input.Title, as.data.frame(agg$h.mean))
df1 <- agg
df2 <- national.df[,c(-1,-3,-4)]

names(df1) <- names(df2)

l.err <- aggregate(.~Input.Title, data=mturk.df[,c(1,12)], mean)[,2]
names(l.err) <- sort(national.df$OCC_TITLE)
l.err <- as.list(l.err)

df1$error <- as.numeric(l.err[df1$OCC_TITLE])
df2$error <- as.numeric(l.err[df2$OCC_TITLE])

df1$var <- "MTOS"
df2$var <- "OES"

df <- rbind(df1, df2)
## Set order for datasources
df$var <- factor(df$var, levels=c("OES", "MTOS"))
rm(df1,df2,agg,l.err)

##
## Function to shorten strings
##
shorten.str <- function(x, N=20){
  x <- as.character(x)
  res <- paste0(substr(x,1,N), ifelse(nchar(x)>N,"...",""))
  res
}

##
## Function to create custom boxplots from merged dataset
##
wage.boxplots <- function(df, all.jobs){
  
  ## Filtering data
  df.f <- df[df$OCC_TITLE %in% all.jobs, ]
  
  ## Shorten string
  df.f$OCC_TITLE <- shorten.str(df.f$OCC_TITLE)
  
  ## Ordering facets on median OES wage
  df.f <- df.f[with(df.f, order(var, -H_MEDIAN)), ]
  df.f$OCC_TITLE <- factor(df.f$OCC_TITLE, levels=df.f$OCC_TITLE[df.f$var=="OES"])
  
  ## Plotting
  p <- ggplot(df.f, aes(x=OCC_TITLE, ymin=`H_PCT10`, lower=`H_PCT25`, middle=`H_MEDIAN`, 
                        upper=`H_PCT75`, ymax=`H_PCT90`)) 
  p <- p + geom_boxplot(aes(fill=var), stat="identity") 
  p <- p + theme(axis.text.x=element_blank())
  p <- p + facet_wrap( ~ OCC_TITLE, scales="free_x")
  p <- p + xlab("Boxplots are constructed based on 10%,25%,50%,75% and 90% percentiles") + 
    ggtitle("OES and MTOS Wages")
  p
}

## Filter some top-paid jobs
all.jobs <- national.df[with(national.df, order(-H_MEAN)), ]$OCC_TITLE[1:9]
top.paid <- wage.boxplots(df, all.jobs)
top.paid
ggsave("../../writeup/plots/top.paid.png", top.paid, width=8, height=5)

## Filter som low-paid jobs
all.jobs <- national.df[with(national.df, order(H_MEAN)), ]$OCC_TITLE[1:16]
low.paid <- wage.boxplots(df, all.jobs)
low.paid
ggsave("../../writeup/plots/low.paid.png", low.paid, width=8, height=5)

## Filter selected jobs
all.jobs <- national.df$OCC_TITLE[str_detect(national.df$OCC_TITLE, ignore.case("computer"))]
computer.jobs <- wage.boxplots(df, all.jobs)
computer.jobs
ggsave("../../writeup/plots/computer.jobs.png", computer.jobs, width=8, height=5)


## Find jobs with lowest/highest MSE
df.ord <- df[with(df, order(error)), ]
df.ord <- (df.ord[df.ord$var=="OES", ])[1:10, c("OCC_TITLE", "H_MEAN", "error")]

low.error.jobs <- wage.boxplots(df, df.ord$OCC_TITLE[1:9])
low.error.jobs
ggsave("../../writeup/plots/low.error.jobs.png", low.error.jobs, width=8, height=5)


df.ord <- df[with(df, order(-error)), ]
df.ord <- (df.ord[df.ord$var=="OES", ])[1:10, c("OCC_TITLE", "H_MEAN", "error")]

high.error.jobs <- wage.boxplots(df, df.ord$OCC_TITLE[1:9])
high.error.jobs
ggsave("../../writeup/plots/high.error.jobs.png", high.error.jobs, width=8, height=5)

