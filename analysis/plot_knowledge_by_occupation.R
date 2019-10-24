#! /usr/bin/Rscript --vanilla

suppressPackageStartupMessages({
    library(JJHmisc)
    library(dplyr)
    library(magrittr)
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

df.know <- na.omit(subset(df.wage, log(tot.emp) > 13))
df.know$title <- as.factor(df.know$title)
df.know$title <- with(df.know, reorder(title, know.mu, mean))

g.know <- ggplot(data = df.know,
                 aes(y = title, x = know.mu)) +
    geom_point() +
    xlab("Mean of (Do you know what this job is? -1 = No, 0 = Maybe, 1 = Yes") +
    ylab("") +
    theme_bw()

pdf(paste0(plots.path, "knowledge_by_occupation.pdf"), width = 7, height = 10)
print(g.know)
dev.off()
