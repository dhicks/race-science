library(tidyverse)
theme_set(theme_minimal())
library(tmfast)
library(gt)

# devtools::install_github("ddsjoberg/bstfun")
library(bstfun)
library(patchwork)

library(arrow)
library(here)
library(glue)

vocab = 'md'
k = 40
focal_topics = c('MQ race science' = 'V07', 
                 'intelligence' = 'V22', 
                 'intelligence & race' = 'V24')
topic_labs = names(focal_topics)

out_prefix = '??'

## Load data ----
out_dir = here('out')
data_dir = here('data')
tm_dir = here('data', '04_tm')

model = here(tm_dir, glue('04_{vocab}_tmfast.Rds')) |> 
    read_rds()

phrases_ar = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv')

source(here('R', 'author_data.R'))
## Canonicalized names
canonicalized_df = read_csv(here('data', 
                                 '00_authors_canonicalized.csv')) |> 
    filter(!is.na(canonical)) |> 
    select(-...3)

meta_df = open_dataset(here('data', '01_metadata')) |> 
    left_join(author_data(), by = 'article_id') |> 
    collect() |>
    nest(authors = author)

## Extract matrices
beta  = tidy(model, k, matrix = 'beta')


## Table of highest-beta phrases ----
beta_gt = beta |> 
    filter(topic %in% focal_topics) |> 
    group_by(topic) |> 
    top_n(15, beta) |> 
    ungroup() |> 
    arrange(topic, desc(beta)) |> 
    select(topic, phrase = token, beta) |> 
    group_split(topic) |> 
    bind_cols() |> 
    select(-starts_with('topic')) |> 
    gt() |> 
    tab_spanner(label = topic_labs[1], id = 't1',
                1:2) |> 
    tab_spanner(label = topic_labs[2], id = 't2',
                3:4) |> 
    tab_spanner(label = topic_labs[3], id = 't3',
                5:6) |> 
    fmt_number(starts_with('beta'), 
               decimals = 4) |> 
    cols_label(phrase...2 = 'phrase', 
               beta...3   = 'beta', 
               phrase...5 = 'phrase', 
               beta...6   = 'beta', 
               phrase...8 = 'phrase', 
               beta...9   = 'beta') |> 
    tab_style(style = cell_borders('right', 
                                   color = 'transparent',
                                   weight = px(50)),
               locations = list(cells_body(c(beta...3, beta...6)), 
                                cells_column_labels(c(beta...3, beta...6)), 
                                cells_column_spanners(c('t1', 't2')))) |> 
    tab_source_note(glue('Vocabulary {vocab}, k = {k}'))
beta_gt

write_rds(beta_gt, here(out_dir, '{out_prefix}_beta_table.Rds'))


## Compare term lists ----
make_edges = function(df1, df2, by = 'token') {
    df1 = df1 |> 
        rename(rank = starts_with('rank'))
    df2 = df2 |> 
        rename(rank = starts_with('rank'))
    
    inner_join(df1, df2, by = by)
}

betas = beta |> 
    filter(topic %in% focal_topics) |> 
    group_by(topic) |> 
    arrange(desc(beta)) |> 
    top_n(250, beta) |> 
    mutate(rank = row_number()) |> 
    ungroup() |> 
    group_split(topic)
    

term_diff = bind_rows(
    make_edges(betas[[1]], betas[[2]]),
    make_edges(betas[[2]], betas[[3]]),
    {make_edges(betas[[3]], betas[[1]]) |> 
            mutate(topic.y = str_c(topic.y, ' '))}) |> 
    mutate(delta = rank.x - rank.y)

ggplot(term_diff, aes(x = topic.x, xend = topic.y, 
                      y = rank.x, yend = rank.y, 
                      text = token)) +
    geom_segment(aes(color = token)) +
    scale_x_discrete(limits = c(focal_topics, 
                                  str_c(focal_topics[1], ' ')), 
                     labels = c(topic_labs, 
                                str_c(topic_labs[1], ' ')),
                     name = 'topic') +
    scale_y_reverse(name = 'rank') +
    scale_color_viridis_d(guide = 'none') +
    labs(caption = glue('Vocabulary {vocab}, k = {k}'))
plotly::ggplotly()


## Compare authors ----
## Threshold of .05 covers top ~10% of articles for each topic
## .07 covers top ~5%
tidy(model, k = k, matrix = 'gamma') |> 
    filter(topic %in% focal_topics) |> 
    group_by(topic) |> 
    summarize(median = median(gamma), 
              thresh_05 = ecdf(gamma)(.05), 
              thresh_07 = ecdf(gamma)(.07), 
              top_tenth = quantile(gamma, probs = .9), 
              top_vinth = quantile(gamma, probs = .95))
tidy(model, k = k, matrix = 'gamma') |> 
    filter(topic %in% focal_topics) |> 
    ggplot(aes(gamma, color = topic)) +
    stat_ecdf() +
    geom_vline(xintercept = .04) +
    geom_hline(yintercept = .90)

## Authors with at least 5 documents with gamma > .04
author_rank = tidy(model, k = k, matrix = 'gamma') |> 
    filter(topic %in% focal_topics) |> 
    filter(gamma > .04) |> 
    left_join(meta_df, by = c('document' = 'article_id')) |> 
    unnest(authors) |> 
    filter(!is.na(author)) |> 
    group_by(topic) |> 
    count(author) |> 
    arrange(desc(n)) |> 
    filter(n >= 5) |>
    # slice_max(order_by = n, n = 150, with_ties = FALSE) |>
    mutate(rank = row_number()) |> 
    ungroup() |> 
    group_split(topic) |> 
    set_names(focal_topics)

## Export this list for canonicalization
if (FALSE) {
    author_rank |>
        set_names(NULL) |>
        bind_rows() |>
        group_by(author) |>
        summarize(n = sum(n)) |>
        write_csv(here(data_dir, glue('{out_prefix}_scratch_authors.csv')))
}

## How many such authors
author_rank |> 
    set_names(NULL) |> 
    bind_rows() |> 
    count(topic)

## Top 5 authors for each topic
author_rank |> 
    set_names(NULL) |> 
    bind_rows() |> 
    group_by(topic) |> 
    top_n(15, wt = n)

authors_gt = tibble(topic = names(author_rank)) %>% 
    ## Calculate overlap
    full_join(., ., by = character()) |> 
    filter(topic.x != topic.y) |> 
    rowwise() |> 
    mutate(authors = nrow(author_rank[[topic.x]]), 
           overlap.n = nrow(make_edges(author_rank[[topic.x]], 
                                       author_rank[[topic.y]], 
                                       by = 'author'))) |> 
    ungroup() |> 
    mutate(overlap.rel = overlap.n / authors) |> 
    ## Format for table
    mutate(overlap.rel = scales::percent(overlap.rel, accuracy = 1)) |> 
    mutate(overlap = glue('{overlap.n} ({overlap.rel})')) |> 
    ## From long to wide (topic x topic)
    select(topic.x, authors, topic.y, overlap) |> 
    pivot_wider(names_from = topic.y, values_from = overlap) |> 
    ## Add labels and build table
    mutate(label = topic_labs) |> 
    select(label, everything()) |> 
    gt() |> 
    cols_move(columns = matches(unname(focal_topics[1])), 
              after = authors) |> 
    sub_missing() |> 
    fmt_markdown(label) |> 
    cols_align(align = 'right', columns = label) |> 
    cols_label(label = '', 
               topic.x = md('**topic**'),
               authors = md('**authors**'), 
               V07 = md('**MQ race science**'), 
               V22 = md('**intelligence**'), 
               V24 = md('**intelligence & race**'))
authors_gt
write_rds(authors_gt, here(out_dir, glue('{out_prefix}_authors_gt.Rds')))
authors_gg = as_ggplot(authors_gt, zoom = 5, expand = 10)


author_diff = bind_rows(
    make_edges(author_rank[[1]], author_rank[[2]], by = 'author'), 
    make_edges(author_rank[[2]], author_rank[[3]], by = 'author'), 
    {make_edges(author_rank[[3]], author_rank[[1]], by = 'author') |> 
            mutate(topic.y = str_c(topic.y, ' '))})

## Overlap visualization
author_diff_gg = ggplot(author_diff, aes(x = topic.x, xend = topic.y, 
                        y = rank.x, yend = rank.y, 
                        text = author)) +
    ## Bars to mark the maximum rank included in each topic
    geom_crossbar(data = {author_rank |>
            set_names(NULL) |>
            bind_rows({author_rank[[1]] |>
                    mutate(topic = str_c(topic, ' '))}) |>
            count(topic)},
            inherit.aes = FALSE, stat = 'identity',
            mapping = aes(x = topic,
                          y = n,
                          ymin = n,
                          ymax = n),
            width = .2, size = .5, fatten = 0) +
    geom_segment(aes(color = author)) +
    scale_x_discrete(limits = c(focal_topics, 
                                str_c(focal_topics[1], ' ')), 
                     labels = c(topic_labs, 
                                str_c(topic_labs[1], ' ')),
                     expand = expansion(),
                     name = 'topic') +
    scale_y_reverse(name = 'rank') +
    scale_color_viridis_d(guide = 'none') +
    labs(caption = glue('Vocabulary {vocab}, k = {k}')) +
    theme(plot.margin = margin(t = 5, r = 30, b = 5, l = 5))
author_diff_gg
plotly::ggplotly()
author_diff_gg + 
    authors_gg +
    plot_layout(ncol = 1, 
                heights = c(1, .4))
ggsave(here(out_dir, glue('{out_prefix}_authors.png')), 
       height = 7, width = 7, scale = 1, bg = 'white')
