## This script uses length-proportional conditional entropy to select the analysis vocabulary
library(tidyverse)
theme_set(theme_bw())
library(tmfast)

## Since the datasets are huge by laptop standards, we use arrow
library(arrow)
library(here)
library(tictoc)
library(assertthat)

data_dir = here('data')

## Desired docs:vocab ratios
## We use 3 since this is a common RDF
vocab_ratio = c(1/5, 1, 5)

## Load data ----
message('Loading data')
meta_ar = open_dataset(here(data_dir, '01_metadata'))
phrases_ar = open_dataset(here(data_dir, '00_phrases')) |> 
    select(-year)

## 6.3M distinct phrases
## ~16 sec
tic()
total_phrases = phrases_ar |> 
    pull(phrase) |> 
    n_distinct()
toc()
total_phrases

## 34.9k documents
ndocs = nrow(meta_ar)
ndocs
assert_that(identical(ndocs, 
                      meta_ar |> 
                          pull(article_id) |> 
                          n_distinct()), 
            msg = 'Metadata does not have 1 row per article')

## Target vocabulary sizes
## 0.1%, 0.6%, and 2.8% of all phrases
vocab_size = ndocs * vocab_ratio
vocab_size
vocab_size / total_phrases

## Conditional entropy ----
message('Calculating conditional entropy')
## To save time and memory, we discard everything beyond the largest vocabulary
## ~33 sec
tic()
R_df = ndR(phrases_ar, article_id, phrase, n) |> 
    arrange(desc(ndR)) |> 
    head(max(vocab_size)) |> 
    collect()
toc()

## Pull out small, medium, and large vocabularies
vocabs = vocab_size |> 
    set_names(c('sm', 'md', 'lg')) |> 
    map(~slice(R_df, 1:.x))

## Write out ----
message('Writing vocabulary to disk')
iwalk(vocabs, 
     ~ write_rds(.x, 
                 file = here(data_dir, 
                             str_c('03-vocab-', .y, '.Rds'))))

## Vocab visualization ----
if (!interactive()) quit()

ggplot(vocabs$lg, aes(n, dR)) +
    geom_point() +
    scale_x_log10()

vocab_sample = sample(vocabs$sm$phrase, 30)
phrases_ar |> 
    filter(phrase %in% vocab_sample) |> 
    left_join(meta_ar, by = 'article_id') |> 
    collect() |> 
    ggplot(aes(article_id, n, fill = container.title)) +
    geom_col() +
    facet_wrap(vars(phrase), scales = 'free') +
    scale_x_discrete(breaks = NULL) +
    scale_y_sqrt() +
    coord_flip() +
    theme(axis.text.y=element_blank())
