library(tidyverse)
library(tmfast)
theme_set(theme_bw())
library(patchwork)
library(tinytable)

library(here)
library(arrow)

## Used for simulation draws
library(furrr)
plan(multisession, workers = availableCores() - 2)


## Load data ----
data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

meta_ar = open_dataset(here(data_dir, '01_metadata'))

source(here('R', 'author_data.R'))

model_files = list.files(tm_dir, '*_tmfast.Rds') %>%
    here(tm_dir, .) |> 
    set_names('lg', 'md', 'sm')

## med vocab, k = 40, topics 7, 22, 24 ----
doc_gamma = read_rds(model_files['md']) |> 
    tidy(k = 40, matrix = 'gamma') |> 
    filter(gamma == max(gamma) & topic %in% c('V07', 'V22', 'V24'), 
           .by = document)

## Link metadata to documents of interest
docs_df = inner_join(meta_ar, doc_gamma, by = c('article_id' = 'document')) |> 
    ## And then to authors
    left_join(author_data(), by = 'article_id') |> 
    collect() |> 
    mutate(family = {str_split(author, ' ') |> 
            map_chr(last)})


## Diversity as number of authors, irrespective of pub count
n_authors_df = docs_df |> 
    count(topic, year, family, name = 'nn') |> 
    count(topic, year) |> 
    complete(topic, year, fill = list(n = 0L))

n_authors_gg = ggplot(n_authors_df, 
                      aes(year, n, color = topic, group = topic)) +
    geom_line(linewidth = 1.5, color = 'black') +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = 'Set2', guide = 'none') +
    labs(y = 'authors')
n_authors_gg

## Diversity as perplexity (exp(entropy))
## Basically the same as author count
# docs_df |> 
#     count(topic, year, family) |> 
#     group_by(topic, year) |> 
#     mutate(share = n / sum(n)) |> 
#     summarize(H = sum(-share * log2(share))) |> 
#     mutate(perplexity = 2^H) |> 
#     ungroup() |> 
#     complete(topic, year, fill = list(H = 0)) |> 
#     ggplot(aes(year, perplexity, color = topic)) +
#     geom_line()

## What share of papers have a PF-funded author? 
share_papers_df = docs_df |> 
    mutate(pf = author %in% mq$author) |> 
    summarize(pf = any(pf), authors = list(author),
              .by = c(article_id, year, topic)) |> 
    count(topic, year, pf) |> 
    complete(topic, year, pf, fill = list(n = 0L)) |> 
    mutate(share_papers = n / sum(n), .by = c(topic, year)) |> 
    filter(pf) |> 
    mutate(share_sm = slider::slide_index_dbl(share_papers, 
                                              year, 
                                              mean, 
                                              .complete = TRUE, 
                                              .before = 2, 
                                              .after = 2), 
           .by = c(topic))
share_papers_gg = ggplot(share_papers_df, aes(year, 
                                              color = topic, 
                                              group = topic)) +
    geom_line(aes(y = share_papers), alpha = .5) +
    geom_line(aes(y = share_sm), linewidth = 1.5, color = 'black') +
    geom_line(aes(y = share_sm), linewidth = 1) +
    scale_color_brewer(palette = 'Set2', guide = 'none') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'paper share')
share_papers_gg


## % of authors PF-funded
share_authors_df = docs_df |> 
    count(topic, year, author) |> 
    mutate(pf = author %in% mq$author) |> 
    summarize(share_authors = mean(pf), .by = c(topic, year)) |> 
    complete(topic, year, fill = list(share_authors = 0)) |> 
    mutate(share_sm = slider::slide_index_dbl(share_authors, 
                                              year, 
                                              mean, 
                                              .complete = TRUE, 
                                              .before = 2, 
                                              .after = 2), 
           .by = c(topic))

share_authors_gg = ggplot(share_authors_df, aes(year, 
                                                share_sm, 
                                                color = topic, 
                                                group = topic)) +
    geom_line(aes(y = share_authors), alpha = .5) +
    geom_line(linewidth = 1.5, color = 'black') +
    geom_line(linewidth = 1.25) +
    scale_color_brewer(palette = 'Set2', guide = 'none') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'author share')
share_authors_gg

## Lots of missing values here, presum. div by 0? 
# full_join(share_papers_df, share_authors_df, 
#           by = c('topic', 'year')) |> 
#     mutate(ratio = share_papers / share_authors) |> 
#     # pull(ratio) |> summary()
#     ggplot(aes(year, ratio, color = topic)) +
#     geom_line()

papers_authors_gg = full_join(share_papers_df, share_authors_df, 
                              by = c('topic', 'year')) |> 
    filter(share_authors > 0) |>
    ggplot(aes(share_authors, share_papers, 
               color = topic, group = topic)) +
    geom_linerange(aes(ymin = share_papers, 
                       ymax = share_authors), 
                   linewidth = .5,
                   alpha = .5) +
    geom_point(size = 2.5, color = 'black') +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0) +
    scale_color_brewer(palette = 'Set2') +
    scale_y_continuous(name = 'paper share', 
                       labels = scales::percent_format()) +
    scale_x_continuous(name = 'author share', 
                       labels = scales::percent_format())
papers_authors_gg



## Median difference ----
## (length of line in plot), when share_authors > 0
# # A tibble: 3 × 2
# topic     diff
# <chr>    <dbl>
# 1 V07   -0.00100
# 2 V22    0.0418 
# 3 V24    0.117  
diff_df = full_join(share_papers_df, share_authors_df, 
                    by = c('topic', 'year')) |> 
    mutate(diff = share_papers - share_authors) |> 
    filter(share_authors > 0) |>
    group_by(topic) |> 
    summarize(diff = mean(diff, na.rm = TRUE))
diff_df

## Drawing sets of 15 authors randomly, 
## across 1000 iterations 99% CI is 
# # A tibble: 3 × 4
# topic     n     low   high
# <chr> <int>   <dbl>  <dbl>
# 1 V07    1000 -0.0238 0.213 
# 2 V22    1000  0.0148 0.0845
# 3 V24    1000  0.0217 0.107 

set.seed(2025-04-07)
sim_df = future_map(1:1000, 
                    .progress = TRUE,
                    .options = furrr_options(seed = 2025-04-07),
                    ~ {
                        ## Random sample of 15 authors
                        authors = docs_df |> 
                            group_by(topic) |> 
                            distinct(family) |> 
                            slice_sample(n = 15) |> 
                            mutate(in_sample = TRUE) |> 
                            ungroup()
                        
                        ## Share of papers w/ at least one author from `authors`
                        papers = docs_df |> 
                            left_join(authors, by = c('topic', 'family')) |> 
                            replace_na(list(in_sample = FALSE)) |> 
                            summarize(in_sample = any(in_sample), authors = list(author),
                                      .by = c(article_id, year, topic)) |> 
                            count(topic, year, in_sample) |> 
                            complete(topic, year, in_sample, fill = list(n = 0L)) |>
                            mutate(share_papers = n / sum(n), .by = c(topic, year)) |> 
                            filter(in_sample)
                        ## Share of authors
                        docs = docs_df |> 
                            count(topic, year, family) |> 
                            left_join(authors, by = c('topic', 'family')) |> 
                            replace_na(list(in_sample = FALSE)) |> 
                            summarize(share_authors = mean(in_sample), .by = c(topic, year)) |> 
                            complete(topic, year, fill = list(share_authors = 0))
                        full_join(papers, docs, 
                                  by = c('topic', 'year')) |> 
                            mutate(diff = share_papers - share_authors) |> 
                            filter(share_authors > 0) |> 
                            group_by(topic) |> 
                            summarize(diff = mean(diff,
                                                  na.rm = TRUE))
                    }) |> 
    bind_rows(.id = 'iter')

## ECDF of "resamples"
ecdf_gg = ggplot(sim_df, aes(diff, color = topic)) +
    # geom_density()
    geom_hline(yintercept = c(.05, .5, .95), 
               linetype = 'solid', alpha = .5) +
    stat_ecdf(linewidth = 1) +
    geom_vline(aes(xintercept = diff, color = topic), 
               linewidth = 1,
               data = diff_df) +
    scale_color_brewer(palette = 'Set2', guide = 'none') +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1), 
                       expand = c(0, 0)) +
    facet_grid(cols = vars(topic), scales = 'fixed')
ecdf_gg

sim_df |> 
    group_by(topic) |> 
    summarize(n = n(),
              low = quantile(diff, .005),
              high = quantile(diff, .995))

sim_df |> 
    group_by(topic) |> 
    summarize(#n = n(),
        low = quantile(diff, .005),
        high = quantile(diff, .995)) %>%
    left_join(diff_df, ., by = 'topic') |> 
    tt() |> 
    format_tt(j = c(2:4), 
              digits = 3, 
              num_fmt = 'decimal', 
              num_zero = TRUE) |> 
    style_tt(j = c(2:4), 
             align = 'd')

## Density plot of "resamples"
density_gg = ggplot(sim_df, aes(diff, color = topic)) +
    geom_density() +
    geom_rug(alpha = .25) +
    # geom_hline(yintercept = c(.05, .5, .95), 
    #            linetype = 'solid', alpha = .5) +
    # stat_ecdf(linewidth = 1) +
    geom_vline(aes(xintercept = diff, color = topic), 
               linewidth = 1,
               data = diff_df) +
    scale_color_brewer(palette = 'Set2', guide = 'none') +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3), 
                       name = 'mean(paper share - author share)') +
    labs(y = '') +
    facet_wrap(vars(topic), scales = 'free') +
    theme(
        strip.text = element_blank(),
        strip.background = element_blank()
    )
density_gg



## Big combined visualization ----
# design = 
# 'AD
#  BC'
design = 'AD
          BD
          CE'

list(A = n_authors_gg, 
     B = share_authors_gg, 
     C = share_papers_gg, 
     D = papers_authors_gg + coord_equal(), 
     E = density_gg) |> 
    wrap_plots(guides = 'collect', 
               design = design) +
    plot_annotation(tag_levels = 'A')

ggsave(here(out_dir, '15_presence.png'), 
       width = 6, height = 4, scale = 1.75)

"Disproportionate presence of Pioneer-funded authors in three focal topics.  
A: Number of authors (distinguished by family name) for three focal topics from the medium vocabulary, k= 40 model, by year. Mainstream intelligence (topic 22) shows exponential-like increase while MQ race science (topic 7) and race-and-intelligence research (topic 24) remain flat. 
B: Pioneer-funded authors as a share (%) of all authors publishing in a given topic, by year (thin lines) and 5-year running mean (thick lines). Usually fewer than 15% of authors in a topic are Pioneer-funded. 
C: Share of papers (%) with at least one Pioneer-funded author, by year (thin lines) and 5-year running mean (thick lines). After about 1985, more than 15% of papers in topic 24 have at least one Pioneer-funded author. 
D: Comparison of author share (panel B) to paper share (panel C). Each point represents one year. If Pioneer-funded authors had publishing profiles (y-axis) proportionate to their authorship profiles (x-axis), points would be close to the dark line $y = x$. Points above this line indicate that Pioneer-funded authors had a relatively greater publishing presence than would be expected given their authorship profile. Length of thin vertical lines indicates difference between paper share and author share. 
E: Empirical density plots of mean difference between paper share and author share for 1,000 randomly drawn sets of 15 authors. Bold vertical lines are the mean values for the Pioneer-funded authors (signed length of lines in panel D). Pioneer-funded authors are similar to randomly drawn sets for topics 7 and 22, but very different for topic 24. 
"
