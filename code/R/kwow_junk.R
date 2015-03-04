# Restrict sample to people who know what an occupation consists of

m <- lm(error ~ social, data = subset(mturk.df, know == 1))
m <- lmer(error ~ social + (1|title), data = subset(mturk.df, know == 1))
m <- lmer(error ~ social*log(actual.mean.wage) + (1|title),
          data = subset(mturk.df, know == 1))
mturk.df$managerial <- with(mturk.df, grepl("[Mm]anag", title))
m <- lm(error ~ social + know + managerial + log(actual.mean.wage), data = mturk.df)

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


###########################
# SCATTER PLOT - NEEDS WORK
###########################

df.goofs <- subset(df.wage, log(tot.emp) > 13 & abs(log(mturk.w) - log(oes.w)) > .40)

ggplot(data = df.wage, aes(
  x = log(mturk.w),
  y = log(oes.w))) + geom_point(aes(size = log(tot.emp))) +
  geom_smooth(method = "lm") + geom_abline(a = 1, b = 0) +
  geom_text(data = df.goofs,
            aes(label = shortenName(title,20)))

# what predicts error rate? FE approach
m <- lm(error ~ Answer.know_anyone + Answer.know_job + log(TOT_EMP), data = mturk.df)
summary(m)


# What job titles do people find confusing?

m <- lmer(know ~ (1|title) + log(tot.emp), data = mturk.df)

df.resid <- data.frame(ranef(m)$title)

colnames(df.resid) <- 'Residual'

df.resid$job <- factor(rownames(df.resid))

df.resid$job <- with(df.resid, reorder(job, Residual, mean))

ggplot(data = df.resid, aes(x = Residual, y = job)) + geom_point() 

qplot(log(TOT_EMP), social, data = mturk.df) + geom_smooth() 

# How knowledge about job (ratio of awared respondents) is related to TOT_EMP
tb <- with(mturk.df, table(cut_interval(tot.emp, 10), I(know==1)))
tb <- data.frame(dont=tb[,1], know=tb[,2], ratio=tb[,2]/(tb[,1]+tb[,2]))
q <- ggplot(data=tb, aes(x=1:10, y=ratio)) + geom_smooth() + geom_point() + xlab("TOT_EMP") +
  scale_x_discrete(limits=levels(cut_interval(log(mturk.df$tot.emp),10))) +
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
<<<<<<< HEAD
       geom_smooth() + geom_abline(a = 1, b = 0) 

# What about when they know the job? 
ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_job, ncol = 3)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
    geom_point() +
       geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_anyone, ncol = 3)
=======
  geom_smooth() + geom_abline(a = 1, b = 0) 

# What about when they know the job? 
ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
  geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_job, ncol = 3)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = log(Answer.wage), size = TOT_EMP)) +
  geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~Answer.know_anyone, ncol = 3)
>>>>>>> 4ef2184035c55d616b30a218da961d1772a2a7bf

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
<<<<<<< HEAD

regression.table(list("1" = m1, "2" = m2, "3" = m3), list(), "../../writeup/tables/models_know_anyone.tex")

=======

regression.table(list("1" = m1, "2" = m2, "3" = m3), list(), "../../writeup/tables/models_know_anyone.tex")

>>>>>>> 4ef2184035c55d616b30a218da961d1772a2a7bf
ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) + facet_wrap(~know, ncol=2)

ggplot(data = mturk.df, aes(x = log(actual.mean.wage), y = error, size = TOT_EMP)) + geom_point() +
  geom_smooth() + geom_abline(a = 1, b = 0) 



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

m.real <- felm(know.someone ~ log(actual.mean.wage) * log(mean.wage.others.social) + G(WorkerId),
               clustervar = "WorkerId", 
               data = by.worker.df)

m.fake <- felm(fake.know.someone ~ log(actual.mean.wage) * log(mean.wage.others.social) +
                 G(WorkerId),
               clustervar = "WorkerId", 
             , data = by.worker.df.fake)

out.file <- "../../writeup/tables/clustering.tex"
sink("/dev/null")
s <- stargazer(m.real, m.fake, 
          title = "Knowing someone in an occupation, conditioned upon wages of other known occupations",
          label = "tab:clustering",
          add.lines = list(c("Worker FE", "Yes", "Yes")), 
          dep.var.labels = c("Know Someone?", "Pseudo-Know Someone?"), 
          covariate.labels = c("Actual occupation wage, $\\log \\bar{w}_j$",
               "Mean log wage of other known occupations, $\\log \\bar{w}^S_{-j}$",          
               "Interaction term: $\\log \\bar{w}_i \\times \\log \\bar{w}^S_{-i}$"
          ),
          type = "latex"
          )
sink()
table.note <- note <- c("\\\\",
          "\\begin{minipage}{0.95 \\textwidth}",
          "{\\footnotesize \\emph{Notes:}
           

           \\starlanguage }",
          "\\end{minipage}")
AddTableNote(s, out.file, table.note)


# Estimated trends in wages 
g.wage.trends <- ggplot(data = na.omit(subset(df.wage,log(tot.emp) > 13)),  aes(y = title, x = w.trend.mu)) +
  geom_point() +
  theme_bw() 


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

#createLaTeXparameters(parameters, "../../writeup/parameters.tex")

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

          
