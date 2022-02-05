library(tidyverse)
theme_set(theme_minimal())
library(arrow)

library(here)

data_dir = here('data')

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

phrases_ar = open_dataset(here(data_dir, '01_phrases'))

## Descriptive plots ----
meta_ar |> 
    count(container.title, year) |> 
    collect() |> 
    ggplot(aes(year, n, color = container.title, group = container.title)) +
    geom_line() +
    geom_point()





## In theory we can join metadata and phrases
## But these all crash R when passed to collect()
## May be a known issue, but mwe didn't have problems on my Mac:  
##    <https://issues.apache.org/jira/browse/ARROW-14908>
## Possibly fixed in 6.0.2; not released as of 2022-02-05
# left_join(meta_ar, phrases_ar, by = 'article_id') |> 
#     head() |> 
#     collect()
# inner_join(meta_ar, phrases_ar, by = 'article_id') |>
#     group_by(year) |>
#     summarize(total_phrases = sum(n)) |>
#     collect()
# phrases_ar |> 
#     group_by(article_id) |> 
#     summarize(total_phrases = sum(n)) |> 
#     inner_join(meta_ar, by = 'article_id') |> 
#     group_by(container.title, year) |> 
#     summarize(total_phrases = sum(total_phrases))
# 

## Workaround
## NB This seems to create objects in memory, so significantly bumps up memory use
# inner_join(to_duckdb(meta_ar, auto_disconnect = FALSE),
#            to_duckdb(phrases_ar, auto_disconnect = FALSE),
#            by = 'article_id') |>
#     group_by(container.title, year) |>
#     summarize(total_phrases = sum(n)) |>
#     collect()
## Can use gc() to purge these
## auto_disconnected = TRUE seems to cause gc() to raise an error
# gc()
