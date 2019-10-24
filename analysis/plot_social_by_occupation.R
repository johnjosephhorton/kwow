#! /usr/bin/Rscript --vanilla

suppressPackageStartupMessages({
    library(data.table)
    library(lfe)
    library(stargazer)
    library(JJHmisc)
    library(magrittr)
    library(dplyr)
    library(ggplot2)
})

mturk.df <- readRDS("../etl/mturk_cooked.rds")

tables.path <- "../writeup/tables/"
plots.path <- "../writeup/plots/"

df.wage <- mturk.df %>% group_by(title) %>%
    summarise(
        mturk.w = mean(predicted.wage),
        oes.w = mean(actual.mean.wage),
        tot.emp = mean(tot.emp),
        know.mu = mean(know),
        social.mu = mean(social),
        error = log(oes.w) - log(mturk.w)
    )

df.wage$title <- with(df.wage, reorder(title, social.mu, mean))

g.social <- ggplot(data = na.omit(subset(df.wage, log(tot.emp) > 13)),
                 aes(y = title, x = social.mu)) + geom_point() +
    xlab("Mean of social index") +
    ylab("") +
    theme_bw()

pdf(paste0(plots.path, "social_by_occupation.pdf"), width = 7, height = 10)
print(g.social)
dev.off()

