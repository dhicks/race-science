## Fit topic models using tmfast
## TODO: abstract process of loading vocab and fitting, to facilitate fitting multiple models (vocab size x k)
library(tidyverse)
theme_set(theme_bw())
library(tmfast)

library(arrow)
library(here)
library(tictoc)


data_dir = here('data')

## Load data ----
vocab = read_rds(here(data_dir, '03-vocab-md.Rds'))
meta_df = open_dataset(here(data_dir, '01_metadata')) |> 
    collect()

phrases_df = open_dataset(here(data_dir, '00_phrases')) |> 
    select(-year) |> 
    filter(phrase %in% vocab$phrase) |> 
    mutate(n = log1p(n)) |> 
    collect()

## What's dropped with this vocabulary? 
anti_join(meta_df, phrases_df, by = c('article_id')) |> 
    count(container.title)

## Fit topic models ----
k = c(5, seq(10, 50, by = 5))
## Like 20 sec
tic()
topic_models = tmfast(phrases_df, 
                      n = k,
                      article_id, 
                      phrase, 
                      n)
toc()


## Explore topic models ----
screeplot(topic_models, npcs = max(k))

data.frame(PC = 1:length(topic_models$sdev),
           cum_var = cumsum(topic_models$sdev^2) / topic_models$totalvar) |> 
    ggplot(aes(PC, cum_var)) +
    geom_line() +
    geom_point()

gamma_df = tidy(topic_models, k = 5, matrix = 'gamma')
gamma_df |> 
    left_join(meta_df, by = c('document' = 'article_id')) |> 
    ggplot(aes(topic, gamma,
               group = document, 
               color = container.title)) +
    geom_line(alpha = .01) +
    facet_wrap(vars(container.title)) +
    scale_color_discrete(guide = 'none')
