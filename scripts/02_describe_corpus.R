renv::load(here::here())
library(tidyverse)
theme_set(theme_minimal())
library(ggrepel)
library(arrow)

library(here)

data_dir = here('data')
out_dir = here('out')

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
## Counts by journal ----
count_df = meta_ar |>
    count(container.title, year) |>
    collect()

label_df = count_df |> 
    group_by(container.title) |> 
    slice_max(n, n = 1) |> 
    mutate(container.title = str_wrap(container.title, width = 25))

ggplot(count_df, 
       aes(year, n, color = container.title, group = container.title)) +
    geom_line(size = 1.25, color = 'black') +
    geom_line(size = 1) +
    geom_label_repel(
        data = label_df, 
        mapping = aes(fill = container.title, 
                      label = container.title), 
        hjust = 0, nudge_y = 20,
        xlim = c(1960, 2100), direction = 'x',
        color = 'black', alpha = .9) +
    scale_color_viridis_d(guide = 'none', 
                          aesthetics = c('fill', 'color')) +
    # scale_y_sqrt() +
    coord_cartesian(clip = 'off') +
    theme(plot.margin = margin(t = 10, r = 50, b = 10, l = 10))

ggsave(here(out_dir, '02_count.png'), 
       width = 4.76, height = 3/4 * 4.76, scale = 1.75,
       bg = 'white')

meta_ar |> 
    group_by(container.title) |> 
    summarize(n = n(), 
          start = min(year), 
          end = max(year)) |> 
    rename(journal = container.title) |> 
    collect() |>
    knitr::kable(caption = '(\\#tab:counts) Document counts, by journal, and years included in the corpus.') |> 
    write_lines(here(out_dir, '02_tab_counts.md'))


## Phrases ----
## Total corpus size: 43M tokens
phrases_ar |> 
    pull(n) |> 
    sum() |> 
    format(big.mark = ',')

## Distinct phrases: 6M
phrases_ar |> 
    select(phrase) |> 
    distinct() |> 
    collect() |> 
    nrow() |> 
    format(big.mark = ',')

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
