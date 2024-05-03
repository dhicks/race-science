library(tidyverse)
theme_set(theme_minimal())
library(arrow)

library(here)

data_dir = here('data')

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

set.seed(2022-02-14)
subset_meta = meta_ar |> 
    group_by(container.title) |> 
    collect() |> 
    slice_sample(n = 250)

subset_phr = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv') |> 
    filter(article_id %in% subset_meta$article_id) |> 
    collect()

## Entropy ----
## Pr(document | phrase)
# subset_phr |> 
#     group_by(phrase) |> 
#     mutate(p = n/sum(n))
# 
# ## Conditional entropy
# subset_phr |> 
#     group_by(phrase) |> 
#     mutate(p = n/sum(n),
#            H_term = -p*log2(p)) |> 
#     summarize(H = sum(H_term)) |> 
#     arrange(H)
# 
# ## Information gain
# max_H = log2(nrow(subset_meta))
# 
# subset_phr |> 
#     group_by(phrase) |> 
#     mutate(p = n/sum(n),
#            H_term = -p*log2(p)) |> 
#     summarize(H = sum(H_term)) |> 
#     mutate(delta_H = max_H - H) |> 
#     arrange(desc(delta_H))
# 
# ## ndH
# H_df = subset_phr |> 
#     group_by(article_id) |> 
#     mutate(doc_len = sum(n)) |> 
#     group_by(phrase) |> 
#     mutate(p = n/sum(n), 
#            H_term = -p*log2(p)) |> 
#     summarize(n = sum(n), 
#               H = sum(H_term)) |> 
#     mutate(delta_H = max_H - H, 
#            ndH = log10(n)*delta_H) |> 
#     arrange(desc(ndH))
# 
# # terms = c('sleep', 'dreams', 'rem_sleep', 'creativity')
# terms = H_df |> 
#     head(24) |> 
#     pull(phrase)
# 
# subset_phr |> 
#     filter(phrase %in% terms) |> 
#     #view() |> 
#     left_join(subset_meta, by = 'article_id') |> 
#     ggplot(aes(article_id, n, fill = container.title)) +
#     geom_col() +
#     facet_wrap(vars(phrase), scales = 'free_x') +
#     coord_flip()


## Alternative approach ----
## R(doc) draws docs inversely proportional to their length
r_df = subset_phr |> 
    group_by(article_id) |> 
    summarize(len = sum(n)) |> 
    mutate(alpha = sum(len), 
           r = len/alpha)

## Information gain from R to tokens
d_df = subset_phr |> 
    left_join(r_df, by = 'article_id') |> 
    group_by(phrase) |> 
    mutate(p = n/sum(n), 
           H_term = - p * log2(p),
           d_term = p * log2(p / r)) |> 
    summarize(n = sum(n), 
              H = sum(H_term),
              d = sum(d_term)) |> 
    mutate(nd = log2(n)*d) |> 
    arrange(desc(nd)) # |> view()

## Drop unique phrases in vocab selection
terms = d_df |> 
    filter(H > 0) |> 
    top_n(25, nd) |> 
    pull(phrase)

subset_phr |> 
    filter(phrase %in% terms) |> 
    #view() |> 
    left_join(subset_meta, by = 'article_id') |> 
    ggplot(aes(article_id, n, fill = container.title)) +
    geom_col() +
    facet_wrap(vars(phrase), scales = 'free_x') +
    scale_x_discrete(labels = NULL) +
    scale_y_sqrt() +
    coord_flip()


## Compare w/ TF-IDF ----
# tf_idf = subset_phr |> 
#     tidytext::bind_tf_idf(phrase, article_id, n) |> 
#     arrange(desc(tf_idf))
# 
# tf_idf |> 
#     pull(phrase) |> 
#     head(24)
# 
# H_df |> 
#     filter(H > 0) |> 
#     inner_join(tf_idf, by = c('phrase')) |> 
#     arrange(phrase) |> 
#     view()
