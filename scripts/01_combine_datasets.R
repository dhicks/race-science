library(dplyr)
library(arrow)

library(here)

data_dir = here('data')

write_csv = FALSE  ## Write phrases out to a single CSV? 

## Noun phrases ----
# phrases_mq = open_dataset(here(data_dir, '00_phrases_mq'))
# phrases_mq = dataset_factory(here(data_dir, '00_phrases_mq'))

# phrases_ms = open_dataset(here(data_dir, '00_phrases_mainstr'))
# phrases_ms = dataset_factory(here(data_dir, '00_phrases_mainstr'))

## This is very twitchy; manually combined parquet folders seems more stable
# phrases_comb = open_dataset(c(phrases_mq, phrases_ms))
# write_dataset(phrases_comb, here(data_dir, '01_phrases'))

phrases_comb = open_dataset(here(data_dir, '00_phrases'))

if (write_csv) {
    phrases_comb |> 
        select(-year) |> 
        compute() |>
        write_csv_arrow(file = here(data_dir, '01_phrases.csv'))
}

## Metadata ----
meta_mq = open_dataset(here(data_dir, '00_meta_mq.csv'), format = 'csv') |> 
    mutate(container.title = 'Mankind Quarterly') |> 
    select(article_id, container.title, year, title, volume, issue) |> 
    collect() |> 
    mutate(across(.cols = c(volume, issue), as.character))

meta_ms = open_dataset(here(data_dir, '00_meta_mainstr')) |> 
    rename(article_id = doi) |> 
    select(article_id, container.title, year, title, volume, issue) |> 
    collect()

meta_comb = bind_rows(meta_ms, meta_mq)

meta_comb |> 
    group_by(container.title, year) |> 
    write_dataset(here(data_dir, '01_metadata'))

