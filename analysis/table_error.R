#! /usr/bin/Rscript --vanilla

suppressPackageStartupMessages({
    library(data.table)
    library(lfe)
    library(stargazer)
    library(JJHmisc)
    library(lme4)
})

mturk.df <- readRDS("../etl/mturk_cooked.rds")

tables.path <- "../writeup/tables/"
plots.path <- "../writeup/plots/"


mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

m.wage <-felm(error ~ social | WorkerId + title | 0 | WorkerId,
              data = mturk.df)

m.v.trend <- felm(v.trend.error ~ social | WorkerId + title | 0 | WorkerId,
                  data = mturk.df)

m.wage <- lmer(error ~ social + (1|WorkerId) + (1|title),
              data = mturk.df)

m.v.trend <- lmer(v.trend.error ~ social + (1|WorkerId) + (1|title),
                  data = mturk.df)

out.file <- paste0(tables.path, "error.tex")
sink("/dev/null")
s <- stargazer(m.wage, m.v.trend, 
          title = "Social and errors in occupational wage and employment trajectories",
          label = "tab:fe_errors",
          column.labels = c("Wage Error", "Employment Trend Error"),
          model.names = FALSE, 
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          covariate.labels = c("Social Knowledge Index"),
          add.lines = list(c("Respondent RE", "Yes", "Yes"), c("Occupation RE", "Yes", "Yes")),
          omit.stat = c("aic", "f", "ll", "bic", "ser"), 
          type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{0.95 \\textwidth}",
          "{\\footnotesize \\emph{Notes:} This table reports the effects of social and general knowledge on a
respondent's prediction error, while controlling for worker and
occupation specfic effects. In Columns (1) the dependent
variable is MSE in wage prediction, while in Column (2), the
dependent variable is an indicator for incorrectly predicting the
direction of employment growth in that occupation. \\starlanguage }",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)

