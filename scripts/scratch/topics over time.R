library(tidyverse)
theme_set(theme_minimal())
library(tmfast)

library(arrow)
library(here)
library(assertthat)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

model = here(tm_dir, '04_md_tmfast.Rds') |>
    read_rds()
meta_ar = open_dataset(here(data_dir, '01_metadata'))

gamma_df = tidy_all(model, matrix = 'gamma')

gamma_df |> 
    filter(k == 30, 
           topic %in% c('V05', 'V07', 'V19')) |> 
    left_join(collect(meta_ar), 
              by = c('document' = 'article_id')) |> 
    ggplot(aes(container.title, gamma, 
               color = container.title)) +
    # geom_boxplot() +
    geom_violin(draw_quantiles = .5, 
                scale = 'width') +
    scale_color_discrete(guide = 'none') +
    facet_wrap(vars(topic), 
               ncol = 1)

gamma_df |> 
    filter(k == 30, 
           topic %in% c('V05', 'V07', 'V19')) |> 
    left_join(collect(meta_ar), 
              by = c('document' = 'article_id')) |> 
    ggplot(aes(year, gamma, 
               color = container.title)) +
    # geom_point(alpha = .2) +
    stat_summary(fun = median, geom = 'line') +
    facet_wrap(vars(topic), 
               ncol = 1)

threshold = .1

## TODO: properly handle missing values
gamma_df |> 
    filter(k == 30, 
           topic %in% c('V05', 'V07', 'V19')) |> 
    left_join(collect(meta_ar), 
              by = c('document' = 'article_id')) |> 
    filter(gamma > threshold) |> 
    count(k, topic, year, container.title) |> 
    group_by(k, topic, container.title) |>
    mutate(smooth = slider::slide_index_dbl(n, year, 
                                            mean, 
                                            .before = 2, 
                                            .after = 2, 
                                            .complete = TRUE)) |> 
    ungroup() |> 
    ggplot(aes(year, 
               color = container.title, group = container.title)) +
    geom_line(aes(y = n), alpha = .2) +
    geom_line(aes(y = smooth), size = 1) +
    facet_wrap(vars(topic), 
               scales = 'free_y',
               ncol = 1) +
    labs(caption = glue('Count of documents w/ gamma > {threshold}'))

gamma_df |> 
    filter(k == 30, 
           topic %in% c('V05', 'V07', 'V19')) |> 
    left_join(collect(meta_ar), 
              by = c('document' = 'article_id')) |> 
    group_by(k, topic, year, container.title) |> 
    count(thresh = gamma > threshold) |> 
    mutate(share = n/sum(n)) |> 
    filter(thresh) |> 
    group_by(k, topic, container.title) |> 
    mutate(smooth = slider::slide_index_dbl(share, year, 
                                            mean, 
                                            .before = 2, 
                                            .after = 2)) |> 
    ungroup() |> 
    ggplot(aes(year, 
               color = container.title, group = container.title)) +
    geom_line(aes(y = share), alpha = .2) +
    geom_line(aes(y = smooth), size = 1) +
    facet_wrap(vars(topic), 
               # scales = 'free', 
               ncol = 1) +
    labs(caption = glue('Share of documents w/ gamma > {threshold}'))
