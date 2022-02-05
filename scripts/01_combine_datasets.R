library(dplyr)
library(arrow)

library(here)

data_dir = here('data')

## Noun phrases ----
phrases_comb = open_dataset(c(here(data_dir, 'phrases_mq', 'part-0.parquet'),
                              here(data_dir, 'phrases_mainstr', 'part-0.parquet')),
                            format = 'parquet')

## This crashes
# write_dataset(phrases_comb, here(data_dir, '01_phrases'))
## Uses ~4 GB memory, but doesn't crash
phrases_comb |> 
    head(nrow(phrases_comb)) |> 
    write_dataset(here(data_dir, '01_phrases'))


## Metadata ----
meta_mq = open_dataset(here(data_dir, 'meta_mq.csv'), format = 'csv') |> 
    mutate(container.title = 'Mankind Quarterly') |> 
    select(article_id, container.title, year, title, volume, issue) |> 
    collect() |> 
    mutate(across(.cols = c(volume, issue), as.character))

meta_ms = open_dataset(here(data_dir, 'meta_mainstr')) |> 
    rename(article_id = doi) |> 
    select(article_id, container.title, year, title, volume, issue) |> 
    collect()

meta_comb = bind_rows(meta_ms, meta_mq)

meta_comb |> 
    group_by(container.title, year) |> 
    write_dataset(here(data_dir, '01_metadata'))

