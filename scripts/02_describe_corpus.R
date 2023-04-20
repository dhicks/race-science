renv::load(here::here())
 library(tidyverse)
theme_set(theme_minimal())
library(arrow)

library(here)

data_dir = here('data')

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))
    # collect()

nrow(meta_ar)

## Approach 1:  Define separate dataset factories and combine
# phrases_mq = dataset_factory(here(data_dir, '00_phrases_mq'))
# phrases_ms = dataset_factory(here(data_dir, '00_phrases_mainstr'))
# phrases_ar = open_dataset(c(phrases_mq, phrases_ms))
## Approach 2:  Dataset combined in script 01
# phrases_ar = open_dataset(here(data_dir, '01_phrases'))
## Approach 3:  Manually combined parquet folders
## This seems to be the most stable option
source(here('R', 'phrases.R'))
phrases_ar = phrases()

# phrases_ar = read_csv(here(data_dir, '01_phrases.csv'))

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

## Any docs with phrases but not metadata? 
phrases_ar |>
    count(article_id) |>
    anti_join(meta_ar, by = 'article_id') |>
    collect() |> 
    nrow()

## Metadata but not phrases?  
phrase_ids = phrases_ar |> 
    count(article_id) |> 
    pull(article_id)

meta_ar |> 
    filter(!article_id %in% phrase_ids) |> 
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
combined = phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    group_by(container.title, year) |>
    summarize(total_phrases = sum(total_phrases)) |> 
    collect()

ggplot(combined, 
       aes(year, total_phrases, 
               color = container.title, group = container.title)) +
    geom_line() +
    scale_y_log10()

#created a joined df of everything rather than just total phrases by journal-year
full = inner_join(phrases_ar, meta_ar, 
                  by = c('article_id'), 
                  multiple = 'all') |> 
    collect()

#identifying missing values
n_distinct(full$article_id)
sum(is.na(full$article_id)) #none
sum(is.na(full$title)) #lots 441,751
sum(is.na(full$volume)) #lots 928,591
sum(is.na(full$issue)) #1,375
sum(is.na(full$container.title)) #none
sum(is.na(full$n)) #none

#' Matching checks
#articles only in meta_ar
#pdfs for these articles are corrupt
anti_join(meta_ar, phrases_ar, by = 'article_id') |> 
    collect()

# articles only in phrases_ar
#none
anti_join(phrases_ar, meta_ar, by = 'article_id') |> 
    collect()


#' plot total phrases by journal-year
options(scipen = 999) #changes scientific notation to standard notation for y-axis
combined |> 
    ggplot(aes(x = year, y = total_phrases, color = container.title)) +
    geom_line() +
    geom_point() +
    facet_grid(. ~ container.title)

#distribution by journal of total phrases by article
# takes several minutes to run if x-axis labels are left in
full |> 
    group_by(container.title, article_id) |> 
    summarize(total_phrases = sum(n)) |> 
    ggplot(aes(x = article_id, y = total_phrases, color = container.title)) +
    geom_point() +
    scale_x_discrete(breaks = NULL) +
    facet_grid(. ~ container.title, scales = "free")

#' number of distinct phrases
n_distinct(full$phrase) #5,437,430

#' unique phrases
##count number of rows per noun phrase, filter to just the noun phrases that only show up in one doc
unique = phrases_ar |> 
    count(article_id, phrase) |> 
    count(phrase) |> 
    filter(n == 1L) |> 
    collect()
# 4,826,771
nrow(unique)
