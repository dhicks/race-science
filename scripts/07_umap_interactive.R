library(tidyverse)
theme_set(theme_minimal())
library(tmfast)
library(plotly)
library(writexl)

library(arrow)
library(here)
library(glue)

vocab = 'sm'
k = 30
force = FALSE  ## Force re-calculating UMAP projection
outliers = .01 ## Used to calculate window for visualization

## Load data ----
out_dir = here('out')
data_dir = here('data')
tm_dir = here('data', '04_tm')

source(here('R', 'author_data.R'))
meta_df = open_dataset(here('data', '01_metadata')) |> 
    left_join(author_data(), by = 'article_id') |> 
    filter(container.title != 'Psychological Reports') |> 
    collect() |>
    nest(authors = author)

phrases_ar = open_dataset(here(data_dir, '01_phrases.csv'), 
                          format = 'csv')
model = here(tm_dir, glue('04_{vocab}_tmfast.Rds')) |> 
    read_rds()
# umap = here(tm_dir, glue('04_{vocab}_{k}_umap.Rds')) |> 
#     read_rds()

exponent = here(data_dir, glue('05_{vocab}_exponents.Rds')) |> 
    read_rds() |> 
    pull(exponent, name = k) |> 
    magrittr::extract(as.character(k))


gamma = tidy(model, k, matrix = 'gamma', exponent = exponent) |> 
    filter(document %in% meta_df$article_id)
beta  = tidy(model, k, matrix = 'beta')

umap_file = here(data_dir, glue('07_{vocab}_{k}_umap.Rds'))
if (!file.exists(umap_file) || force) {
    message(glue('Calculating UMAP projection for {vocab}-{k}'))
    umap_df = hellinger(gamma, prob1 = gamma) |> 
        umap(verbose = TRUE)
    write_rds(umap_df, umap_file)
} else {
    umap_df = read_rds(umap_file)
}

    

## Weighted phrases for each document ----
gamma_max = gamma |> 
    group_by(document) |> 
    filter(gamma == max(gamma))

weight_df = phrases_ar |> 
    inner_join(gamma_max, by = c('article_id' = 'document')) |> 
    inner_join(beta, by = c('phrase' = 'token', 'topic' = 'topic')) |> 
    mutate(weight = n * beta) |> 
    collect() |> 
    group_by(article_id) |> 
    top_n(5, wt = weight)

## Table of V19 articles, for QC
# weight_df |> 
#     filter(topic == 'V19') |> 
#     group_by(topic, gamma, article_id) |> 
#     summarize(phrases = str_c(phrase, collapse = '; ')) |> 
#     ungroup() |> 
#     arrange(desc(gamma)) |> 
#     # head() |> 
#     left_join(meta_df, by = c('article_id')) |> 
#     mutate(authors = map_chr(authors, 
#                              ~ {. |> 
#                                      pull(author) |> 
#                                      str_c(collapse = '; ')})) |> 
#     select(topic, gamma, article_id, 
#            container.title, year, 
#            title, phrases, authors) |> 
#     # write_csv(here('out', '07_v19_articles.csv'))
#     write_xlsx(here('out', '07_v19_articles.xlsx'))


## Interactive visualization ----
window = umap_df |> 
    reframe(across(c(x, y), 
                     ~ quantile(.x, 
                                probs = c(outliers, 1-outliers)))) |> 
    mutate(across(c(x, y), 
                  ~ .x * 1.15))


umap_plot = weight_df |> 
    select(article_id, topic, phrase) |> 
    group_by(article_id, topic) |> 
    summarize(terms = list(str_c(phrase, sep = '; '))) |> 
    inner_join(umap_df, by = c('article_id' = 'document')) |> 
    inner_join(meta_df, by = 'article_id') |> 
    ggplot(aes(x, y, 
               color = container.title, 
               id = article_id, 
               so = container.title, 
               yr = year,
               ti = title, 
               tp = topic,
               tr = terms)) +
    geom_point(aes(color = container.title), 
               alpha = .25) + #aes(alpha = topic == 'V19')) +
    geom_point(aes(color = topic), 
               alpha = .25) +
    # scale_color_viridis_d(guide = 'none') +
    # scale_alpha_discrete(guide = 'none') +
    scale_color_manual(values = c(viridisLite::viridis(5), 
                                  viridisLite::viridis(k)), 
                       name = '') +
    lims(x = window$x, 
         y = window$y)

if (interactive()) umap_plot

p = ggplotly(umap_plot, 
             tooltip = c('id', 'so', 'yr', 'ti', 'tp', 'tr'), 
             width = 1*1440, 
             height = .8*1080) |> 
    layout(legend = list(font = list(size = 24)), 
           hoverlabel = list(font = list(size = 24))) |> 
    style(visible = FALSE, traces = (1:k)+5) |> 
    layout(updatemenus = list(
        list(
            active = 0,
            type = 'buttons',
            buttons = list(
                list(method = 'restyle', 
                     args = list('visible', 
                                 c(rep(TRUE, 5), 
                                      rep(FALSE, k))), 
                     label = 'Journal'),
                list(method = 'restyle', 
                     args = list('visible', 
                                 c(rep(FALSE, 5), 
                                   rep(TRUE, k))), 
                     label = 'Topic')
            )
        )
    )
    ) |> 
    layout(legend = list(x = 1, 
                         xanchor = 'right', 
                         borderwidth = 1, 
                         bgcolor = rgb(0, 0, 0, alpha = .2)))
if (interactive()) p

write_rds(p, here('out', glue('07_{vocab}_{k}.Rds')))
htmlwidgets::saveWidget(p, here('out', glue('07_{vocab}_{k}.html')))

