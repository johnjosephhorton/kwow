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
library(memisc)
library(plyr)
library(texreg)

# If JJHmisc is missing, run: 
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


##
## Data from MTurk
##
mturk.df <- import.mturk("../../data/mturk_output.csv")

nrow(mturk.df)
head(mturk.df)
sapply(mturk.df, class)


l.emp <- national.df[, "TOT_EMP"]
names(l.emp) <- national.df[, "OCC_TITLE"]
l.emp <- as.list(l.emp)

l.wage <- national.df[, "H_MEAN"]
names(l.wage) <- national.df[, "OCC_TITLE"]
l.wage <- as.list(l.wage)

mturk.df$TOT_EMP <- as.numeric(as.character(l.emp[ mturk.df$Input.Title ]))
mturk.df$H_WAGE <- as.numeric(as.character(l.wage[ mturk.df$Input.Title ]))

mturk.df$know <- as.numeric(as.character(with(mturk.df,
                                              factor(Answer.know_job,
                                                     levels = c("No", "Maybe", "Yes"),
                                                     labels = c("-1","0","1")))))

# mse of wage prediction 
mturk.df$error <- with(mturk.df, sqrt((log(H_WAGE) - log(Answer.wage))**2))

qplot(log(TOT_EMP), social, data = mturk.df) + geom_smooth() 

qplot(log(TOT_EMP), know, data = mturk.df) + geom_smooth() 

# what predicts error rate? 

table(mturk.df[,c("know","social")])

cor(mturk.df[,c("error","TOT_EMP","H_WAGE","Answer.wage")])


# How knowledge about job (ratio of awared respondents) is related to H_WAGE
tb <- with(mturk.df, table(cut_interval(log(H_WAGE), 10), I(know==1)))
tb <- data.frame(dont=tb[,1], know=tb[,2], ratio=tb[,2]/(tb[,1]+tb[,2]))

g.knowledge_by_wage <- ggplot(data=tb,
                              aes(x=1:10, y=ratio)) +
    geom_smooth() +
    geom_point() +
    xlab("log(Hourly Wage)") +
        scale_x_discrete(limits=levels(cut_interval(log(mturk.df$H_WAGE),10))) +
    theme_bw() 

print(q)

ggsave("../../writeup/plots/knowledge_wage.png", g.knowledge_by_wage, width=8, height=5)

mturk.df$w.trend <- as.numeric(as.character(with(mturk.df,
                                              factor(Answer.wage_trend,
                                                     levels = c("GoDown", "StayTheSame", "GoUp"),
                                                     labels = c("-1","0","1")))))

df.wage <- ddply(mturk.df, .(Input.Title), summarise,
                 mturk.w = mean(Answer.wage),
                 mturk.w.weighted = mean(Answer.wage, weight = know),
                 oes.w = mean(H_WAGE),
                 tot.emp = mean(TOT_EMP),
                 know.mu = mean(know),
                 w.trend.mu = mean(w.trend))

df.wage$error <- with(df.wage, log(oes.w) - log(mturk.w))


# Worker wage historgrams
# qplot(log(Answer.wage), data = mturk.df) + facet_wrap(~WorkerId, ncol = 12)

# mse of wage prediction 
mturk.df$error <- with(mturk.df, sqrt((log(H_WAGE) - log(Answer.wage))**2))

m.1 <- lm(error ~ social, data = mturk.df)
m.2 <- lmer(error ~ social + (1|Input.Title), data = mturk.df)

# Does social predict know?
m <- lm(know ~ social + log(TOT_EMP), data = mturk.df)

# Within a worker, is knowledge of a job independent of its wage?
#m <- lmer(Answer.wage ~ 1 + (H_WAGE | 

# What's the occupational breakdown of "know"?


# Are higher wage occupations less well-known? 
m <- lm(error ~ oes.w, data = df.wage)

# How much variation in wages does worker mean predictions explain? 
m <- lm(log(oes.w) ~ log(mturk.w), data = df.wage)

m <- lm(log(oes.w) ~ log(mturk.w.weighted), data = df.wage)

m <- lm(log(oes.w) ~ log(mturk.w), data = df.wage, weights = tot.emp)

# Trends
qplot(oes.w, w.trend.mu, data = df.wage)

df.wage$Input.Title <- with(df.wage, reorder(Input.Title, w.trend.mu, mean))

# Estimated trends in wages 
g.wage.trends <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),  aes(y = Input.Title, x = w.trend.mu)) +
    geom_point() +
    theme_bw() 

ggsave("../../writeup/plots/wage_trends.png", g.wage.trends, width=7, height=10)

# Self-reported knowledge about jobs 
df.wage$Input.Title <- with(df.wage, reorder(Input.Title, know.mu, mean))

g.know <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),  aes(y = Input.Title, x = know.mu)) + geom_point() +
    xlab("Mean of (Do you know what this job is? -1 = No, 0 = Maybe, 1 = Yes") +
    ylab("Job Title") + theme_bw()

pdf("../../writeup/plots/knowledge_by_occupation.pdf")
print(g.know)
dev.off()

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


# What job titles do people find confusion?

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
m <- lm(error ~ Answer.know_anyone + Answer.know_job + log(TOT_EMP) + H_WAGE, data = mturk.df)
summary(m)

m1 <- glm(error ~ know*social + log(TOT_EMP) + log(H_WAGE), data = mturk.df)
m2 <- glm(error ~ know*social + log(H_WAGE), data = mturk.df)
m3 <- glm(error ~ social + log(H_WAGE) + log(TOT_EMP), data=mturk.df)
m4 <- lmer(error ~ know*social + log(TOT_EMP) + log(H_WAGE) + (1|Input.Title) , data = mturk.df)
m5 <- glm(error ~ know*social + log(TOT_EMP) + log(H_WAGE) + I(log(Answer.wage)>3), data = mturk.df)

models <- list("1" = m1,
               "2" = m2,
               "3" = m3,
               "4" = m4,
               "5" = m5)

regression.table(models, list(), "../../writeup/tables/models_error.tex")

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) + geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) 

# What about when they know the job? 
ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_job, ncol = 3)

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_anyone, ncol = 3)

ggplot(data = mturk.df, aes(x = log(H_WAGE), y = log(Answer.wage), size = TOT_EMP)) +
  geom_point() +
     geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~qualified, ncol = 3)


m1 <- lm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df)
m2 <- glm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df, family="binomial")
screenreg(list(m1,m2))

qplot(Answer.know_anyone, TOT_EMP, data = mturk.df) + geom_boxplot()


# MTurks know jobs with lower wages and mass employment?
m1 <- glm(I(Answer.know_anyone != "0") ~ TOT_EMP, data = mturk.df, family="binomial")
m2 <- glm(I(Answer.know_anyone != "0") ~ log(H_WAGE), data = mturk.df, family="binomial")
m3 <- glm(I(Answer.know_anyone != "0") ~ log(H_WAGE) + TOT_EMP, data = mturk.df, family="binomial")
screenreg(list(m1,m2,m3))

sink("../../writeup/tables/models_know_anyone.tex", append=FALSE, split=FALSE)
texreg(list(m1,m2,m3), booktabs=T)
sink()


ggplot(data = mturk.df, aes(x = log(H_WAGE), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~know, ncol=2)

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

#
# CODE BELOW IS DEPRECIATED 
#




## ## Comparation of RSE's
## stat <- summary(data.frame(OES=df1$EMP_PRSE, MTSO=df$Answer.wage[,3]))
## tab <- xtable(stat, caption="RSE for Hourly Wages in OES and MTSO datasets (all obs.)",
##               label="tab:rse_oes_mtso1")

## # Output to a file 
## sink("../../writeup/tables/rse1.tex", append=FALSE, split=FALSE)
## tab
## sink()

## ##
## ## Filter only informed respondents
## ## Who a) know the job; b) know any people in the job
## ##
## mturk.df.knowledge <- with(mturk.df, mturk.df[Answer.know_job=="Yes" & Answer.know_anyone!=0, ])

## agg <- aggregate(.~Input.Title, data = mturk.df.knowledge[,c(1,4)], FUN=national.stats)
## df2 <- merge(national.df, agg, by.x="OCC_TITLE", by.y="Input.Title")

## ## Comparation of RSE's
## stat <- summary(data.frame(OES=df2$EMP_PRSE, MTSO=df2$Answer.wage[,3]))
## tab <- xtable(stat, caption="RSE for Hourly Wages in OES and MTSO datasets (filtered)",
##               label="tab:rse_oes_mtso2")

## # Output to a file 
## sink("../../writeup/tables/rse2.tex", append=FALSE, split=FALSE)
## tab
## sink()

## ## Jobs with no informed respondents
## national.df$OCC_TITLE[!(unique(national.df$OCC_TITLE) %in% unique(df2$OCC_TITLE))]


## ##
## ## Basic plots
## ##



## # head(df1)
## # plots.df <- data.frame(job=sapply(df1$OCC_TITLE,shorten.str), h.mean.oes=df1$H_MEAN, h.mean.mtos=df1$Answer.wage[,1]) #[1:20,]

## plots.df <- data.frame(job=df$OCC_CODE, h.mean.oes=df$H_MEAN, h.mean.mtos=df$Answer.wage[,1]) #[1:20,]

## plots.df <- plots.df[with(plots.df, order(-h.mean.oes)), ]
## plots.df <- within(plots.df, job<-factor(job, levels=job))
## plots.df <- melt(data=plots.df, id.vars="job")

## plot.mean.h <- 
##   ggplot(plots.df, aes(x=job, y=value, fill=variable)) + 
##   geom_bar(alpha=.5, position="dodge", stat="identity") + 
##   theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) + 
##   ggtitle("Mean hourly wage")

## plot.mean.h

## ggsave("../../writeup/plots/mean.h.wage.png", plot.mean.h, width=8, height=5)
