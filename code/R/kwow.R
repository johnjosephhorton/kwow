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

##
## Data utils
##
source("utils.R")

##
## Libraries
##
library(xtable)
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(lme4)


##
## Data from Bureau of Labor Statistics, Department of Labor
##
national.df <- import.top.national("../../data/national_M2012_dl.csv", 99)

nrow(national.df)
head(national.df)
sapply(national.df, class)


##
## Data from MTurk
##
mturk.df <- import.mturk("../../data/mturk_output.csv")

nrow(mturk.df)
head(mturk.df)

sapply(mturk.df, class)


# Hacky way of getting some of the national data on the mturk.df 
l.emp <- national.df[, "TOT_EMP"]
names(l.emp) <- national.df[, "OCC_TITLE"]
l.emp <- as.list(l.emp)

l.wage <- national.df[, "H_MEAN"]
names(l.wage) <- national.df[, "OCC_TITLE"]
l.wage <- as.list(l.wage)

mturk.df$TOT_EMP <- as.numeric(as.character(l.emp[ mturk.df$Input.Title ]))
mturk.df$H_WAGE <- as.numeric(as.character(l.wage[ mturk.df$Input.Title ]))

# mse of wage prediction 
mturk.df$error <- with(mturk.df, sqrt((log(H_WAGE) - log(Answer.wage))**2))

# what predicts error rate? 
m <- lm(error ~ Answer.know_anyone + Answer.know_job + log(TOT_EMP), data = mturk.df)
summary(m)

# reformulate anyone not as factor
mturk.df$know <- as.numeric(as.character(with(mturk.df,
                                              factor(Answer.know_job,
                                                     levels = c("No", "Maybe", "Yes"),
                                                     labels = c("-1","0","1")))))

mturk.df$social <- as.numeric(as.character(with(mturk.df,
                                              factor(Answer.know_anyone,
                                                     levels = c("0", "1", "2", "3-10", "10_plus"),
                                                     labels = c("-2","-1","0","1","2")))))

qplot(log(TOT_EMP), social, data = mturk.df) + geom_smooth() 

qplot(log(TOT_EMP), know, data = mturk.df) + geom_smooth() 

m <- lm(error ~ know*social + log(TOT_EMP), data = mturk.df)

m <- lm(error ~ know*social, data = mturk.df)

m <- lmer(error ~ know*social + log(TOT_EMP) + (1|Input.Title), data = mturk.df)

summary(m)


ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) + geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) 

# What about when they know the job? 
ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_job, ncol = 3)

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_anyone, ncol = 3)



m <- lm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df)
summary(m)

qplot(Answer.know_anyone, TOT_EMP, data = mturk.df) + geom_boxplot()


# MTurks know jobs with lower wages?
m <- lm(know ~ log(H_WAGE), data = mturk.df)
summary(m)

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~know, ncol=2)

m <- lm(error ~ know*social + log(H_WAGE), data = mturk.df)
summary(m)

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = error, size = TOT_EMP)) + geom_point() +
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
agg <- aggregate(.~Input.Title, data = mturk.df[,c(1,4)], FUN=national.stats)
df1 <- cbind(agg$Input.Title, as.data.frame(agg$Answer.wage))
df2 <- national.df[,c(-1,-3,-4)]

names(df1) <- names(df2)
df1$var <- "MTOS"
df2$var <- "OES"

df <- rbind(df1, df2)
## Set order for datasources
df$var <- factor(df$var, levels=c("OES", "MTOS"))
rm(df1,df2,agg)

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







#
# CODE BELOW IS DEPRECIATED 
#




## Comparation of RSE's
stat <- summary(data.frame(OES=df1$EMP_PRSE, MTSO=df$Answer.wage[,3]))
tab <- xtable(stat, caption="RSE for Hourly Wages in OES and MTSO datasets (all obs.)",
              label="tab:rse_oes_mtso1")

# Output to a file 
sink("../../writeup/tables/rse1.tex", append=FALSE, split=FALSE)
tab
sink()

##
## Filter only informed respondents
## Who a) know the job; b) know any people in the job
##
mturk.df.knowledge <- with(mturk.df, mturk.df[Answer.know_job=="Yes" & Answer.know_anyone!=0, ])

agg <- aggregate(.~Input.Title, data = mturk.df.knowledge[,c(1,4)], FUN=national.stats)
df2 <- merge(national.df, agg, by.x="OCC_TITLE", by.y="Input.Title")

## Comparation of RSE's
stat <- summary(data.frame(OES=df2$EMP_PRSE, MTSO=df2$Answer.wage[,3]))
tab <- xtable(stat, caption="RSE for Hourly Wages in OES and MTSO datasets (filtered)",
              label="tab:rse_oes_mtso2")

# Output to a file 
sink("../../writeup/tables/rse2.tex", append=FALSE, split=FALSE)
tab
sink()

## Jobs with no informed respondents
national.df$OCC_TITLE[!(unique(national.df$OCC_TITLE) %in% unique(df2$OCC_TITLE))]


##
## Basic plots
##



# head(df1)
# plots.df <- data.frame(job=sapply(df1$OCC_TITLE,shorten.str), h.mean.oes=df1$H_MEAN, h.mean.mtos=df1$Answer.wage[,1]) #[1:20,]

plots.df <- data.frame(job=df$OCC_CODE, h.mean.oes=df$H_MEAN, h.mean.mtos=df$Answer.wage[,1]) #[1:20,]

plots.df <- plots.df[with(plots.df, order(-h.mean.oes)), ]
plots.df <- within(plots.df, job<-factor(job, levels=job))
plots.df <- melt(data=plots.df, id.vars="job")

plot.mean.h <- 
  ggplot(plots.df, aes(x=job, y=value, fill=variable)) + 
  geom_bar(alpha=.5, position="dodge", stat="identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) + 
  ggtitle("Mean hourly wage")

plot.mean.h

ggsave("../../writeup/plots/mean.h.wage.png", plot.mean.h, width=8, height=5)
