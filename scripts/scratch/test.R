library(tidyverse)
theme_set(theme_minimal())
library(arrow)

library(here)

data_dir = here('data')

meta_ar = open_dataset(here(data_dir, '01_metadata'))

# phrases_mq = dataset_factory(here(data_dir, '00_phrases_mq'))
# phrases_ms = dataset_factory(here(data_dir, '00_phrases_mainstr'))
# phrases_ar = open_dataset(c(phrases_mq, phrases_ms))

# phrases_ar = open_dataset(here(data_dir, '00_phrases_mainstr'))

phrases_ar = open_dataset(here(data_dir, 'phrase_alt')) |> 
    select(-prefix, -year)

# phrases_ar |>
#     count(year) |>
#     collect()

phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    group_by(container.title, year) |>
    summarize(total_phrases = sum(total_phrases)) |> 
    collect()


