library(tidyverse)
theme_set(theme_bw())
library(tmfast)

library(arrow)
library(here)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')

## Load data ----
source(here('R', 'pf_authors.R'))

meta_df = open_dataset(here(data_dir, '01_metadata')) |> 
    mutate(article_id = tolower(article_id)) |> 
    inner_join(pf_authors, by = 'article_id') |>
    collect()

## Available documents by author
count(meta_df, pf_author)

## Load topic models ----
model_files = list.files(tm_dir, '*_tmfast.Rds') %>%
    here(tm_dir, .) |> 
    set_names('lg', 'md', 'sm')
exp_files = list.files(data_dir, '*_exponents.Rds') %>%
    here(data_dir, .) |> 
    set_names('lg', 'md', 'sm')
## Helper to read exponents as a named num
read_exp = function(file) {
    file |> 
        read_rds() |> 
        pull(exponent, name = k)
}


this_vocab = 'md'

this_model_file = model_files[this_vocab]
this_model = read_rds(this_model_file)
this_exp = exp_files[this_vocab] |> 
    read_exp()

gamma_df = tidy_all(this_model, 
                    matrix = 'gamma', 
                    exponent = this_exp) |> 
    inner_join(meta_df, 
               by = c('document' = 'article_id'), 
               multiple = 'all')

## Tile plot ----
## Difficult to read bc so many authors
ggplot(gamma_df, aes(topic, document, fill = gamma)) +
    geom_raster() +
    geom_vline(xintercept = c(10, 20, 30, 40) + .5,
               color = 'black') +
    facet_grid(rows = vars(pf_author), 
               cols = vars(k),
               scale = 'free', 
               switch = 'y',
               labeller = label_wrap_gen(width = 15), 
               # space = 'free_y'
               ) +
    scale_x_discrete(breaks = c('V10', 'V20', 'V30', 'V40')) +
    scale_y_discrete(breaks = NULL,
                     name = '') +
    scale_fill_viridis_c(option = 'viridis', direction = 1) +
    theme_minimal() +
    theme(axis.ticks.x = element_line(),
          legend.position = 'bottom',
          plot.margin = margin(r = 10))

gamma_df |> 
    filter(k == 40) |> 
    ggplot(aes(topic, gamma, group = document)) +
    geom_vline(xintercept = c('V07', 'V22', 'V24'), 
               color = 'blue', alpha = .5) +
    geom_line(alpha = .2) +
    facet_wrap(vars(pf_author)) +
    scale_x_discrete(breaks = c('V10', 'V20', 'V30'))

## Document max count ----
gamma_df |> 
    group_by(k, pf_author, document) |> 
    filter(gamma == max(gamma)) |> 
    group_by(k, pf_author) |> 
    count(topic) |> 
    ggplot(aes(topic, n)) +
    geom_col() +
    facet_grid(rows = vars(pf_author), 
               cols = vars(k), 
               scales = 'free') +
    scale_x_discrete(breaks = 'none')

gamma_df |> 
    filter(k == 40) |> 
    group_by(pf_author, document) |> 
    filter(gamma == max(gamma)) |> 
    group_by(pf_author) |> 
    count(topic) |> 
    complete(topic = {1:40 |> 
            str_pad(2, pad = '0') %>% 
            str_c('V', .)}, 
            fill = list(n = 0L)) |> 
    ggplot(aes(topic, n)) +
    geom_vline(xintercept = c('V07', 'V22', 'V24'), 
               color = 'blue', alpha = .5) +
    geom_col() +
    facet_wrap(vars(pf_author), 
               scales = 'free_y') +
    scale_x_discrete(breaks = c('V10', 'V20', 'V30', 'V40'))
