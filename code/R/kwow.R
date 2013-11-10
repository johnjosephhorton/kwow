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


##
## Function returns basic stats for variable
## Statistics are comparable to OES data
##
national.stats <- function(x){
  c(h.mean=mean(x),
    h.median=median(x),
    h.prse=100*sd(x)/mean(x),
    h.pct=quantile(x,c(0.10,0.25,0.75,0.90)))
}

##
## Aggregation of two datasets (OES & MTSO)
##
agg <- aggregate(.~Input.Title, data = mturk.df[,c(1,4)], FUN=national.stats)
df1 <- merge(national.df, agg, by.x="OCC_TITLE", by.y="Input.Title")

## Comparation of RSE's
stat <- summary(data.frame(OES=df1$EMP_PRSE, MTSO=df1$Answer.wage[,3]))
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
national.df$OCC_TITLE[!(unique(national.df$OCC_TITLE) %in% unique(df$OCC_TITLE))]


##
## Basic plots
##

shorten.str <- function(x,N=15){
  paste0(substr(x,1,N), ifelse(nchar(x)>N,"...",""))
}

# head(df1)
# plots.df <- data.frame(job=sapply(df1$OCC_TITLE,shorten.str), h.mean.oes=df1$H_MEAN, h.mean.mtos=df1$Answer.wage[,1]) #[1:20,]

plots.df <- data.frame(job=df1$OCC_CODE, h.mean.oes=df1$H_MEAN, h.mean.mtos=df1$Answer.wage[,1]) #[1:20,]

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

library(ggplot2)
library(scales)
library(testthat)

N <- 1000
df.raw <- data.frame(x = runif(N), y = runif(N))

pdf("../../writeup/plots/hist.pdf")
 qplot(x, y, data = df.raw)
dev.off() 