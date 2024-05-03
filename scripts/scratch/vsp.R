library(tidyverse)
theme_set(theme_minimal())
library(vsp)

library(arrow)

library(here)

data_dir = here('data')

## Two helper functions ----
trim_negative = function(x) {
    x[x < 0] = 0
    return(x)
}

normalize = function(x) {
    if (sum(x) != 0) {
        x = x / sum(x)
    }
    return(x)
}

## Load metadata ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

set.seed(2022-02-14)
subset_meta = meta_ar |> 
    group_by(container.title) |> 
    collect() |> 
    slice_sample(n = 1000)

subset_phr = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv') |> 
    filter(article_id %in% subset_meta$article_id) |> 
    collect()


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

## Using vsp ----
## <https://rss.org.uk/RSS/media/File-library/Events/Discussion%20meetings/Preprint_Rohe_Zeng_11-May-2022.pdf>
tic()
vsp_fit = subset_phr |> 
    filter(phrase %in% terms) |> 
    collect() |> 
    tidytext::cast_sparse(article_id, phrase, n) |> #str()
    log1p() |>
    # sqrt() |>
    vsp(rank = 6, center = TRUE, recenter = TRUE, 
        degree_normalize = FALSE, renormalize = FALSE)
toc()

head(vsp_fit$Z)

screeplot(vsp_fit)
# plot_mixing_matrix(vsp_fit)

## Topic-document distributions ----
gamma = vsp_fit$Z |> 
    trim_negative() |> 
    apply(1, normalize) |> 
    t() |> 
    # magrittr::set_rownames(doc_ids) |> 
    as_tibble(rownames = 'article_id') |> 
    left_join(subset_meta, by = 'article_id')

gamma |> 
    select(article_id, container.title, starts_with('z')) |> 
    pivot_longer(starts_with('z'), 
                 names_to = 'topic', 
                 values_to = 'gamma') |> 
    ggplot(aes(topic, gamma, color = container.title)) +
    geom_line(aes(group = article_id), alpha = .05) +
    geom_line(aes(group = container.title), stat = 'summary') +
    scale_color_discrete(guide = 'none') +
    facet_wrap(vars(container.title))


## Word-topic distributions ----
beta = {vsp_fit$Y %*% vsp_fit$B} |> 
    trim_negative() |> 
    # head() |>
    apply(2, normalize) |> 
    as_tibble(rownames = 'term')

beta |> 
    pivot_longer(starts_with('y'), 
                 names_to = 'topic', 
                 values_to = 'beta') |> 
    filter(beta > 0) |> 
    group_by(topic) |> 
    top_n(10, wt = beta) |> 
    arrange(topic, desc(beta)) |> view()

