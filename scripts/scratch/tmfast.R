## This is super promising.  Relatively fast to fit, and the one really expensive operation (SVD) only needs to happen once to fit models across different values of k.  Looks like we'll need to go further than k = 10.  Try up through 50? 
library(tidyverse)
theme_set(theme_minimal())
library(arrow)
library(irlba)
library(tidytext)

library(tmfast)

library(tictoc)
library(here)

data_dir = here('data')

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

set.seed(2022-02-14)
subset_meta = meta_ar |> 
    group_by(container.title) |> 
    collect() |> 
    slice_sample(n = 1000)

subset_phr = open_dataset(here(data_dir, '01_phrases.csv'),
                          format = 'csv') |>
    filter(article_id %in% subset_meta$article_id)

## Vocabulary selection ----
## R(doc) draws docs inversely proportional to their length
r_df = subset_phr |> 
    group_by(article_id) |> 
    summarize(len = sum(n)) |> 
    collect() |> 
    mutate(alpha = sum(len), 
           r = len/alpha)

## Information gain from R to tokens
d_df = subset_phr |> 
    left_join(r_df, by = 'article_id') |>
    collect() |>
    group_by(phrase) |> 
    mutate(p = n/sum(n), 
           H_term = - p * log2(p),
           d_term = p * log2(p / r)) |> 
    summarize(n = sum(n), 
              H = sum(H_term),
              d = sum(d_term)) |> 
    mutate(nd = log2(n)*d) |> 
    arrange(desc(nd)) # |> view()

terms = d_df |> 
    filter(H > 0) |> 
    top_n(10*nrow(subset_meta), nd) |> 
    pull(phrase)

subset_phr |> 
    filter(phrase %in% terms) |> 
    count(article_id) |> 
    inner_join(subset_meta, by = 'article_id') |> 
    count(container.title) |> 
    collect()


## PCA ----
dtm = subset_phr |> 
    filter(phrase %in% terms) |> 
    mutate(n = log1p(n)) |> 
    collect() |> 
    cast_sparse(row = article_id, col = phrase, value = n)

## ~200 sec
tic()
fitted = tmfast(dtm, c(6, 12))
toc()

## 12 factors doesn't have a notable drop in the screeplot and only 16% of cumulative variation
screeplot(fitted)
cumsum(fitted$sdev^2) / fitted$totalvar

## Topic-doc distributions ----
gamma_df = tidy(fitted, k = 12, matrix = 'gamma')
gamma_df |> 
    left_join(subset_meta, by = c('doc' = 'article_id')) |> 
    ggplot(aes(topic, gamma,
                      group = doc, color = container.title)) +
    geom_line(alpha = .05) +
    facet_wrap(vars(container.title)) +
    scale_color_discrete(guide = 'none')

## Top terms ----
beta_df = tidy(fitted, k = 12, matrix = 'beta')

beta_df |> 
    group_by(topic) |> 
    top_n(n = 5, wt = beta) |> 
    arrange(topic, desc(beta)) |> 
    view()
    