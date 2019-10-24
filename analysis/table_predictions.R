#! /usr/bin/Rscript --vanilla

##########################
# Author: John J. Horton 
# Project: kwow 
##########################

suppressPackageStartupMessages({
    library(data.table)
    library(lfe)
    library(stargazer)
    library(JJHmisc)
})

mturk.df <- readRDS("../etl/mturk_cooked.rds")

tables.path <- "../writeup/tables/"
plots.path <- "../writeup/plots/"


mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

df.tmp <- data.table(mturk.df)

df.tmp.by.occ <- df.tmp[,
                        list(num.ratings = .N,
                             know.job = sum(know.job=="Yes")), by = list(title)]

known.jobs <- subset(df.tmp.by.occ, know.job >= 20)$title

mturk.df <- subset(mturk.df, know.job == "Yes" & title %in% known.jobs)

# Predictive Accuracy 

m.emp <- felm(actual.v.trend ~ v.trend  | WorkerId | 0 | WorkerId, data = mturk.df)
m.wage <- felm(log(actual.mean.wage) ~ log(predicted.wage)| WorkerId | 0 | WorkerId, data = mturk.df)

out.file <- paste0(tables.path, "predictions.tex")
sink("/dev/null")
s <- stargazer(m.wage, m.emp, 
          title = "Predictions about wages and employment trends",
          label = "tab:predictions",
          column.labels = c("Log Mean Occupational wage", "Pred. Employment change"),
          model.names = FALSE,
          font.size = "small",     
               dep.var.labels.include = FALSE,
               star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels = c("Estimated log wage", "Estimated employment change"),
          add.lines = list(c("Respondent FE", "Yes", "Yes")),
          omit.stat = c("aic", "f", "ll", "bic", "ser"), 
          type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{0.95 \\textwidth}",
          "{\\footnotesize \\emph{Notes:}
           The sample for both regressions reported in this table are all the responses where the respondent said they knew what an occupation consisted of.
In Column~(1), the dependent variable is the actual log mean wage for that occupation (from the May 2013 OES estimates from the BLS), while in Column~(2) the dependent variable is an index for whether the BLS predicts that by 2020 the fraction employed in that occupation will grow by more than 5\\% (outcome = 1), fall by more than 5\\% (outcome = -1) or will not change by more than 5\\%, positive or negative (outcome = 0).
The important independent variable in both regressions is the respondent's prediction.
Both regressions include a respondent-specific standard error. 
Standard errors are clustered at the level of the individual respondent. 
\\starlanguage }",
"\\end{minipage}")
AddTableNote(s, out.file, note)

