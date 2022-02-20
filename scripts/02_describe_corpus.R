library(tidyverse)
theme_set(theme_minimal())
library(arrow)

library(here)

data_dir = here('data')

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

nrow(meta_ar)

## Approach 1:  Define separate dataset factories and combine
# phrases_mq = dataset_factory(here(data_dir, '00_phrases_mq'))
# phrases_ms = dataset_factory(here(data_dir, '00_phrases_mainstr'))
# phrases_ar = open_dataset(c(phrases_mq, phrases_ms))
## Approach 2:  Dataset combined in script 01
# phrases_ar = open_dataset(here(data_dir, '01_phrases'))
## Approach 3:  Manually combined parquet folders
phrases_ar = open_dataset(here(data_dir, '00_phrases'))

## Descriptive plots ----
meta_ar |>
    count(container.title, year) |>
    collect() |>
    ggplot(aes(year, n, color = container.title, group = container.title)) +
    geom_line() +
    geom_point()

phrases_ar |>
    head(500) |>
    arrange(desc(n)) |>
    collect()




phrases_ar |>
    count(article_id) |>
    anti_join(meta_ar, by = 'article_id') |>
    collect()

## NB Do summaries on phrases_ar *before* joining
# phrases_ar |> 
#     select(-year) |> 
#     left_join(meta_ar, by = 'article_id') |>
#     # head(5000) |>
#     collect()
# inner_join(meta_ar, phrases_ar, by = 'article_id') |>
#     group_by(year) |>
#     summarize(total_phrases = sum(n)) |>
#     collect()
phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    group_by(container.title, year) |>
    summarize(total_phrases = sum(total_phrases)) |> 
    collect()

