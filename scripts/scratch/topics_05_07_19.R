library(tidyverse)
theme_set(theme_minimal())
library(tmfast)

library(gt)

library(arrow)
library(here)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')

model = read_rds(here(tm_dir, '04_md_tmfast.Rds'))

topics_of_interest = c('V05', 'V07', 'V19')

phrases_ar = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv')


## Extra word-topic distributions ----
beta_19 = tidy(model, 30, matrix = 'beta') |> 
    filter(topic == 'V19') |> 
    arrange(desc(beta)) |> 
    top_n(250, beta) |> 
    mutate(rank_19 = row_number())

beta_05 = tidy(model, 30, matrix = 'beta') |> 
    filter(topic == 'V05') |> 
    arrange(desc(beta)) |> 
    top_n(250, beta) |> 
    mutate(rank_05 = row_number())

beta_07 = tidy(model, 30, matrix = 'beta') |> 
    filter(topic == 'V07') |> 
    arrange(desc(beta)) |> 
    top_n(250, beta) |> 
    mutate(rank_07 = row_number())

## 19 appears to be "race & intelligence"
## 05 appears to be "intelligence testing"
# full_join(beta_19, beta_05, by = 'token') |> 
#     view()

## 07 is "anthropological race science"; 19 is "psychological race science"? 
# full_join(beta_07, beta_19, by = 'token') |> 
#     view()

# full_join(beta_07, beta_05, by = 'token') |> 
#     view()


## Compare term lists ----
make_edges = function(df1, df2, by = 'token') {
    df1 = df1 |> 
        rename(rank = starts_with('rank'))
    df2 = df2 |> 
        rename(rank = starts_with('rank'))
    
    inner_join(df1, df2, by = by)
}

term_diff = bind_rows(
    make_edges(beta_05, beta_07),
    make_edges(beta_07, beta_19),
    {make_edges(beta_19, beta_05) |> 
            mutate(topic.y = str_c(topic.y, ' '))}) |> 
    mutate(delta = rank.x - rank.y)

ggplot(term_diff, aes(x = topic.x, xend = topic.y, 
                  y = rank.x, yend = rank.y, 
                  text = token)) +
    geom_segment(aes(color = token)) +
    scale_x_discrete(limits = c('V05', 'V07', 'V19', 'V05 '), 
                     name = 'topic') +
    scale_y_reverse(name = 'rank') +
    scale_color_viridis_d(guide = 'none')
plotly::ggplotly()


## Hellinger distances among these topics ----
## Not close to each other
source(here('R', 'compare_betas.R'))
tidy(model, 30, matrix = 'beta') |>
    filter(topic %in% topics_of_interest) |> 
    compare_betas(vocab = model$cols) |> 
    round(digits = 3)


# tidy(model, 30, matrix = 'beta') |>
#     compare_betas(vocab = model$cols) |>
#     as.matrix() |>
#     umap() |>
#     ggplot(aes(x, y, label = document)) +
#     geom_label()

## No correlation btwn topics at the document level
# tidy(model, 30, matrix = 'gamma') |>
#     filter(topic %in% c('V05', 'V07', 'V19')) |>
#     pivot_wider(names_from = topic,
#                 values_from = gamma) |>
#     ggplot(aes(x = .panel_x, y = .panel_y)) +
#     geom_point(aes(x = .panel_x, y = .panel_y), alpha = .1) +
#     stat_smooth(method = 'lm') +
#     ggforce::facet_matrix(vars(V05, V07, V19))


## Authors with >= 5 papers with gamma >.07 in each topic ----
source(here('R', 'author_data.R'))

## Canonicalized names
canonicalized_df = read_csv(here('data', 
                                 'scratch_authors_canonicalized.csv')) |> 
    filter(!is.na(canonical)) |> 
    select(author, canonical)

meta_df = open_dataset(here('data', '01_metadata')) |> 
    left_join(author_data(), by = 'article_id') |> 
    left_join(canonicalized_df, by = 'author') |> 
    mutate(author = if_else(!is.na(canonical), 
                            canonical, 
                            author)) |> 
    select(-canonical) |> 
    collect() |>
    nest(authors = author)


author_rank = tidy(model, k = 30, matrix = 'gamma') |> 
    filter(topic %in% topics_of_interest) |> 
    filter(gamma > .07) |> 
    left_join(meta_df, by = c('document' = 'article_id')) |> 
    unnest(authors) |> 
    filter(!is.na(author)) |> 
    group_by(topic) |> 
    count(author) |> 
    arrange(desc(n)) |> 
    filter(n >= 5) |> 
    # slice_max(order_by = n, n = 300, with_ties = TRUE) |> 
    mutate(rank = row_number()) |> 
    ungroup() |> 
    group_split(topic) |> 
    set_names(topics_of_interest)

## Export this list for canonicalization
if (FALSE) {
author_rank |>
    set_names(NULL) |>
    bind_rows() |>
    group_by(author) |>
    summarize(n = sum(n)) |>
    write_csv(here(data_dir, 'scratch_authors.csv'))
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
    top_n(15, wt = n) |> 
    view()

## Compare author lists ----
## Calculate overlap
tibble(topic = names(author_rank)) %>% 
    full_join(., ., by = character()) |> 
    filter(topic.x != topic.y) |> 
    rowwise() |> 
    mutate(authors = nrow(author_rank[[topic.x]]), 
           overlap.n = nrow(make_edges(author_rank[[topic.x]], 
                                       author_rank[[topic.y]], 
                                       by = 'author'))) |> 
    ungroup() |> 
    mutate(overlap.rel = overlap.n / authors, 
           overlap.rel = scales::percent(overlap.rel, accuracy = 1)) |> 
    mutate(overlap = glue('{overlap.n} ({overlap.rel})')) |> 
    select(topic.x, authors, topic.y, overlap) |> 
    pivot_wider(names_from = topic.y, values_from = overlap) |> 
    mutate(label = c('intelligence<br>testing', 
                     'race science', 
                     'race & intelligence')) |> 
    select(label, everything()) |> 
    gt() |> 
    cols_move(V05, authors) |> 
    sub_missing() |> 
    fmt_markdown(label) |> 
    cols_align(align = 'right', columns = label) |> 
    cols_label(label = '', 
               topic.x = md('**topic**'),
               authors = md('**authors**'), 
               V05 = md('**V05**'), 
               V07 = md('**V07**'), 
               V19 = md('**V19**'))
# tab_source_note(md('Authors with 5 or more documents with $\\gamma > .07$ in each of three topics.'))

author_diff = bind_rows(
    make_edges(author_rank[[1]], author_rank[[2]], by = 'author'), 
    make_edges(author_rank[[2]], author_rank[[3]], by = 'author'), 
    {make_edges(author_rank[[3]], author_rank[[1]], by = 'author') |> 
            mutate(topic.y = str_c(topic.y, ' '))}) #|> 
    # mutate(delta = rank.x - rank.y)

## Overlap visualization
ggplot(author_diff, aes(x = topic.x, xend = topic.y, 
                        y = rank.x, yend = rank.y, 
                        text = author)) +
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
    scale_x_discrete(limits = c('V05', 'V07', 'V19', 'V05 '), 
                     expand = expansion(),
                     name = 'topic') +
    scale_y_reverse(name = 'rank') +
    scale_color_viridis_d(guide = 'none')
plotly::ggplotly()


## Looking at individual authors
papers_by_author = function(author_str, k = 30, gamma_thresh = .07) {
    tidy(model, k = k, matrix = 'gamma') |> 
        filter(topic %in% topics_of_interest) |> 
        filter(gamma > gamma_thresh) |> 
        left_join(meta_df, by = c('document' = 'article_id')) |> 
        unnest(authors) |> 
        filter(author == author_str) |> 
        pivot_wider(names_from = topic, 
                    values_from = gamma) |> 
        select(document, one_of(topics_of_interest), everything())
}
## Kamin shows up in V19 with critical commentaries
papers_by_author('Leon J. Kamin') |> view()

## Furnham's papers don't seem to be about race
## Though he does have at least one co-authored w/ Lynn, and his rolemodel was Eysenck
papers_by_author('Adrian Furnham') |> view()

papers_by_author('Adrian Furnham') |> 
    filter(!is.na(V19)) |> 
    select(V19, document, title) %>%
    inner_join(phrases_ar, ., by = c('article_id' = 'document')) |> 
    inner_join(beta_19, by = c('phrase' = 'token')) |> 
    group_by(article_id, title, V19, phrase) |> 
    summarize(beta = sum(beta)) |> 
    collect() |> 
    top_n(5, wt = beta) |> 
    arrange(desc(V19), desc(beta)) |> view()

papers_by_author('Robert Plomin') |> 
    filter(!is.na(V19)) |> 
    select(V19, document, title) %>%
    inner_join(phrases_ar, ., by = c('article_id' = 'document')) |> 
    inner_join(beta_19, by = c('phrase' = 'token')) |> 
    group_by(article_id, title, V19, phrase) |> 
    summarize(beta = sum(beta)) |> 
    collect() |> 
    top_n(10, wt = beta) |> 
    arrange(desc(V19), desc(beta)) |> view()
