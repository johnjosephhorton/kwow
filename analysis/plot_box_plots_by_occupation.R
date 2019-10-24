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

#############################
# Box plots of wage knowledge 
#############################

df.overlay <- rbind(mturk.df %>%
                    group_by(title) %>% summarise(sum.wage = actual.mean.wage[1]) %>% mutate(obs.type = "mean"),
                    mturk.df %>% group_by(title) %>% summarise(sum.wage = actual.median.wage[1]) %>% mutate(obs.type = "median"))

mturk.df$title <- with(mturk.df, reorder(title, actual.mean.wage, mean))

g.box.plot <- ggplot(data = mturk.df,
                     aes(x = title,
                         y = predicted.wage)) +
    geom_boxplot(outlier.size = 0, alpha = .5) +
    xlab("BLS Occupation") + ylab("Log hourly wage") +
    scale_y_log10(breaks = c(7, 15, 30, 60, 100)) +
        geom_point(data = df.overlay,
               aes(x = title,
                   y = sum.wage,
                   colour = obs.type,
                   shape = obs.type)
               ) + 
    coord_flip() +
    theme_bw() +
    theme(text=element_text(size = 10)) +
    scale_colour_manual(name = "Actual BLS Occupational Statistics",
                        breaks = c("median", "mean"),
                        labels = c("Median", "Mean"),
                        values = c("red", "black")) +
            scale_shape_discrete("Actual BLS Occupational Statistics",
                                 breaks=c("median", "mean"),
                                 labels=c("Median", "Mean")) +    
   theme(legend.position = "top")
    

#print(g.box.plot)

JJHmisc::writeImage(g.box.plot, "box_plots_by_occupation", width = 7, height = 10, path = plots.path)

