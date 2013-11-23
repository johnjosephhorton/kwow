#! /usr/bin/Rscript --vanilla

###############################################
# Author: Alexander B. Gedranovich
# Created Date: 2013-11-21
# Project: kwow 
###############################################


## Function to import Data from Bureau of Labor Statistics, Department of Labor
## Imports TOP.N jobs from specified OCC.CAT
##
## ARGS:
## fname - path to CSV file with original data
## TOP.N - top jobs to import
## OCC.CAT - occupation categories
##

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

## Function to import occupation data from BLS
##
## ARGS:
## fname - path to CSV file with occupation data
## n.df - data.frame, constructed with 'import.top.national'
##

import.occupation.trend <- function(fname, n.df){
  
  if(!file.exists(fname)){
    print(sprintf("Sorry! The file '%s' does not exist!", fname))
    return(NULL)
  }
  
  df <- na.omit(read.table(fname, header=F, skip=5, sep=","))
  df <- df[, c(-5,-6,-8,-9)]
  names(df) <- c("OCC_TITLE", "OCC_CODE", "TOT_EMP2010", "TOT_EMP2020", "EMP_CHANGE")
  
  # replaces comma in numbers
  df[,c(3:5)] <- as.data.frame(sapply(df[,c(3:5)], gsub, pattern=",", replacement=""))
  # columns conversion
  df[,c(1:5)] <- sapply(df[,c(1:5)], as.character)  
  df[,c(3:5)] <- sapply(df[,c(3:5)], as.numeric)
  
  # as far as in 'occupation.csv' OCC_CODE is 'detailed' we need to convert it to 'broad'
  df$OCC_BROAD <- sapply(df$OCC_CODE, function(s) paste0(substr(s,1,6),"0"))
  
  # jobs not in broad category
  n.df[which(!(n.df$OCC_CODE %in% df$OCC_BROAD)),]
  
  ##
  ## NOTE: There are codes mismatch!!!
  ##
  
  # 1) 'Registered Nurses': 29-1140 (national), 29-1111 (occupation)
  # 2) 'Miscellaneous Teachers and Instructors' 25-3090 (national)
  #    'Teachers and Instructors, All Other' 25-3999 (ocupation)
  # 3) 'Miscellaneous Postsecondary Teachers' 25-1190 (national)
  #    'Postsecondary Teachers' 25-1000 (occupation, minor group)
  
  # change codes just for estimation purposes
  df[which(df$OCC_CODE %in% c("29-1111", "25-3999", "25-1000")), "OCC_BROAD"] <- 
    c("29-1140","25-3090","25-1190")
  
  # cummulative change in employment by broad category (estimated)
  df.broad <- ddply(df, .(OCC_BROAD), summarise,
                    tot.emp2020 = 1000*sum(TOT_EMP2020))
  
  df.broad <- subset(df.broad, df.broad$OCC_BROAD %in% n.df$OCC_CODE)
  
  n.df <- n.df[with(n.df, order(OCC_CODE)), ]
  n.df$TOT_EMP2020 <- df.broad$tot.emp2020
  n.df <- n.df[with(n.df, order(-TOT_EMP)), ]
  
  # Let's conisder +/-5% change as 'StayTheSame'
  n.df$EMP_CHANGE <- with(n.df, (TOT_EMP2020-TOT_EMP)/TOT_EMP)
  n.df$volume.trend <- 
    cut(n.df$EMP_CHANGE, breaks=c(-Inf, -0.05, 0.05, Inf), labels=c("GoDown","StayTheSame","GoUp"))
    
  n.df
}


## Function to import occupation data from BLS
##
## ARGS:
## fname - path to CSV file with education data by category
## n.df - data.frame, constructed with 'import.top.national'
##

import.education <- function(fname, n.df){
  
  if(!file.exists(fname)){
    print(sprintf("Sorry! The file '%s' does not exist!", fname))
    return(NULL)
  }
  
  df <- na.omit(read.table(fname, header=F, skip=3, sep=","))
  # Only first three columns, last line is comment
  df <- df[-nrow(df), c(1:3)]
  names(df) <- c("OCC_TITLE", "OCC_CODE", "ENTRY_EDUCATION")
  
  # Convert entry eduction to factor
  education.levels <- c("Less than high school",
                        "High school diploma or equivalent",
                        "Postsecondary non-degree award",
                        "Some college, no degree",
                        "Associate's degree",
                        "Bachelor's degree",
                        "Master's degree",
                        "Doctoral or professional degree")
  df$ENTRY_EDUCATION  <- factor(df$ENTRY_EDUCATION, levels=education.levels,labels=education.levels)
  
  # as far as in 'education.csv' OCC_CODE is 'detailed' we need to convert it to 'broad'
  df$OCC_BROAD <- sapply(df$OCC_CODE, function(s) paste0(substr(s,1,6),"0"))
  
  # jobs not in broad category
  n.df[which(!(n.df$OCC_CODE %in% df$OCC_BROAD)),]
  
  ##
  ## NOTE: There are codes mismatch!!!
  ##
  
  # 1) 'Registered Nurses': 29-1140 (national), 29-1111 (occupation)
  # 2) 'Miscellaneous Teachers and Instructors' 25-3090 (national)
  #    'Teachers and Instructors, All Other' 25-3999 (ocupation)
  # 3) 'Miscellaneous Postsecondary Teachers' 25-1190 (national)
  #    'Postsecondary Teachers' 25-1000 (occupation, minor group)
  
  # change codes just for estimation purposes
  df[which(df$OCC_CODE %in% c("29-1111", "25-3999", "25-1000")), "OCC_BROAD"] <- 
    c("29-1140","25-3090","25-1190")
  
  # Minimal education requirements within detailed category
  df.broad <- ddply(df, .(OCC_BROAD), summarise,
                    entry.edu = sort(ENTRY_EDUCATION)[1])
  
  df.broad <- subset(df.broad, df.broad$OCC_BROAD %in% n.df$OCC_CODE)
  
  n.df <- n.df[with(n.df, order(OCC_CODE)), ]
  n.df$ENTRY_EDUCATION <- df.broad$entry.edu
  n.df <- n.df[with(n.df, order(-TOT_EMP)), ]
    
  n.df
}


## Function to import Data from MTurk Survey
##
## ARGS:
## fname - path to CSV file with original data
##

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
  
  # rename weird columns 
  names(df)[which("Answer.know_job" == names(df))] <- "know.job"
  names(df)[which("Answer.know_anyone" == names(df))] <- "social.knowledge"
  names(df)[which("Input.Title" == names(df))] <- "title"
  names(df)[which("Answer.wage" == names(df))] <- "predicted.wage"
  
  print(colnames(df))
  df
}