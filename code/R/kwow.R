#! /usr/bin/Rscript --vanilla

###############################################
# Author: John J. Horton 
# Created Date: 2013-11-07 
# Project: kwow 
###############################################

setwd("./code/R")

mturk.file <- "../../data/mturk_output.csv"
mturk.df.raw <- read.table(mturk.file, header=T, sep=",", fill=T)
mturk.df <- mturk.df.raw[, c("Input.Title",
                             "Answer.know_job",
                             "Answer.know_anyone",
                             "Answer.wage",
                             "Answer.volume_trend",
                             "Answer.wage_trend",
                             "Answer.comment")]

tb <- table(mturk.df$Input.Title)
jtitle <- names(tb[tb<30])

mturk.df[mturk.df$Input.Title==jtitle,1]
# Licensed Practical and Licensed Vocational Nurses

library(ggplot2)
library(scales)
library(testthat)

N <- 1000
df.raw <- data.frame(x = runif(N), y = runif(N))

pdf("../../writeup/plots/hist.pdf")
 qplot(x, y, data = df.raw)
dev.off() 