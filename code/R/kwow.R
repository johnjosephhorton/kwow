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


# tb <- table(mturk.df$Input.Title)
# jtitle <- names(tb[tb<30])
# 
# mturk.df[mturk.df$Input.Title==jtitle,1]
# # Licensed Practical and Licensed Vocational Nurses




library(ggplot2)
library(scales)
library(testthat)

N <- 1000
df.raw <- data.frame(x = runif(N), y = runif(N))

pdf("../../writeup/plots/hist.pdf")
 qplot(x, y, data = df.raw)
dev.off() 