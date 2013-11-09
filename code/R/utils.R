###############################################
# Author: Alexander B. Gedranovich
# Created Date: 2013-11-09 
# Project: kwow 
# Description: Data utils
###############################################


###############################################
## Function to import Data from Bureau of Labor Statistics, Department of Labor
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
  ## Replaces comma in numbers
  df[,convert.cols] <- as.data.frame(sapply(df[,convert.cols], gsub, pattern=",", replacement=""))
  ## Replaces '#' with Inf
  df[,convert.cols] <- as.data.frame(sapply(df[,convert.cols], gsub, pattern="#", replacement=Inf))
  
  ## Double convert factor-to-numeric
  df[,convert.cols] <- sapply(df[,convert.cols], as.character)
  df[,convert.cols] <- sapply(df[,convert.cols], as.numeric)
  ## Convert first 3 columns to characters
  df[,1:3] <- sapply(df[,1:3], as.character)
  
  ## Order by Total employment (NOTE: original data is already ordered)
  df <- df[with(df, order(-TOT_EMP)), ]
  ## Select TOP.N records
  df <- df[1:ifelse(nrow(df)>TOP.N, TOP.N, nrow(df)), ]
  rownames(df) <- 1:nrow(df)
  
  df[,c(1,2,4,5,6,8,9:13)]
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
                   "Answer.comment")]
  
  df[,"Answer.wage"] <- as.data.frame(sapply(df[,"Answer.wage"], gsub, pattern="++", replacement="", fixed=T))
  df[,"Answer.wage"] <- sapply(df[,"Answer.wage"], as.character)
  df[,"Answer.wage"] <- sapply(df[,"Answer.wage"], as.numeric)
  
  char.cols <- c("Input.Title","Answer.comment")
  df[,char.cols] <- sapply(df[,char.cols], as.character)
  
  df
}


