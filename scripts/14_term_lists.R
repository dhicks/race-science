library(tidyverse)
theme_set(theme_minimal())
library(tmfast)

library(gt)

library(arrow)
library(here)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')

model = read_rds(here(tm_dir, '04_md_tmfast.Rds'))

k = 40
topics_of_interest = c('V07', 'V22', 'V24')

phrases_ar = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv')

## Extract word-topic distributions ----
## Top 250 words for each topic, ranked
top_words = tidy(model, k, matrix = 'beta') |> 
    filter(topic %in% topics_of_interest) |> 
    group_by(topic) |> 
    top_n(250, wt = beta) |> 
    arrange(topic, desc(beta)) |> 
    mutate(rank = row_number()) |> 
    ungroup() |> 
    group_split(topic)

make_edges = function(x, y) {
    inner_join(x, y, by = 'token')
}

edges = bind_rows(
    make_edges(top_words[[1]], top_words[[2]]), 
    make_edges(top_words[[2]], top_words[[3]]), 
    {make_edges(top_words[[3]], top_words[[1]]) |> 
            mutate(topic.y = str_c(topic.y, ' '))}) |> 
    mutate(delta = rank.x - rank.y)

ggplot(edges, aes(x = topic.x, xend = topic.y, 
                      y = rank.x, yend = rank.y, 
                      text = token)) +
    geom_segment(aes(color = token, group = token)) +
    scale_x_discrete(limits = c('V07', 'V22', 'V24', 'V07 '), 
                     name = 'topic') +
    scale_y_reverse(name = 'rank') +
    scale_color_viridis_d(guide = 'none')

## Beta rather than rank scale; harder to parse
last_plot() + 
    aes(y = beta.x, yend = beta.y) +
    scale_y_continuous()

## Table ----
bind_rows(
    make_edges(top_words[[1]], top_words[[2]]), 
    make_edges(top_words[[2]], top_words[[3]]), 
    make_edges(top_words[[3]], top_words[[1]])) |> 
    group_by(topic.x, topic.y) |> 
    summarize(n()) |> 
    ungroup()
