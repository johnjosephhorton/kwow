#! /usr/bin/Rscript --vanilla

###############################################
# Author: John J. Horton 
# Created Date: 2013-11-07 
# Project: kwow 
###############################################

##
## !!! NOTE: Comment this line when building project !!!
##
setwd("./code/R")

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
df <- merge(national.df, agg, by.x="OCC_TITLE", by.y="Input.Title")

## Comparation of RSE's
summary(data.frame(OES=df$EMP_PRSE, MTSO=df$Answer.wage[,3]))


head(with(mturk.df, mturk.df[Answer.know_job=="Yes" & Answer.know_anyone!=0, ]))


library(ggplot2)
library(scales)
library(testthat)

N <- 1000
df.raw <- data.frame(x = runif(N), y = runif(N))

pdf("../../writeup/plots/hist.pdf")
 qplot(x, y, data = df.raw)
dev.off() 