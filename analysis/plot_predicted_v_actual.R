#! /usr/bin/Rscript --vanilla

suppressPackageStartupMessages({
    library(dplyr)
    library(JJHmisc)
    library(ggplot2)
    library(ggrepel)
})

mturk.df <- readRDS("../etl/mturk_cooked.rds")

tables.path <- "../writeup/tables/"
plots.path <- "../writeup/plots/"

df.wage <- mturk.df %>% group_by(title) %>%
    summarise(
        mturk.w = mean(predicted.wage),
        se.mturk.w = sd(predicted.wage)/sqrt(length(predicted.wage)), 
        oes.w = mean(actual.mean.wage),
        tot.emp = mean(tot.emp),
        know.mu = mean(know),
        social.mu = mean(social),
        error = log(oes.w) - log(mturk.w)
    ) %>%
    mutate(outlier = I(abs(log(oes.w) - log(mturk.w)) > .5))

axes.breaks <- c(7, 15, 30, 60, 100)

g.predicted.v.actual <- ggplot(data = df.wage,
                               aes(y = mturk.w,
                                   x = oes.w)) +
    geom_point(shape = 1, alpha = 0.2) +
    geom_smooth() +
    geom_errorbar(aes(x = oes.w, y = mturk.w, ymin = mturk.w - 2*se.mturk.w,
                      ymax = mturk.w + 2*se.mturk.w),
                  colour = "grey") + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
   # geom_text(data = df.wage.outliers, aes(label = title), size = 1.75,
                                        #           vjust = 0, angle = 0) +
    geom_text_repel(aes(label = ifelse(outlier, title, ""))) + 
    theme_bw() +
    ylab("Respondent mean wage prediction for occupation (log scale)") +
    xlab("Actual mean wage for occupation (log scale)") +
    scale_x_log10(breaks = axes.breaks, limits = c(5, 100)) +
    scale_y_log10(breaks = axes.breaks, limits = c(5, 100)) 


print(g.predicted.v.actual)

JJHmisc::writeImage(g.predicted.v.actual,
                    "predicted_v_actual",
                    height = 5,
                    width = 5,
                    path = plots.path)

