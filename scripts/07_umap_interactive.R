renv::load(here::here())
library(tidyverse)
theme_set(theme_minimal())
library(tmfast)
library(plotly)
library(writexl)
library(htmlwidgets)

library(arrow)
library(here)
library(glue)

outliers = .01    ## Used to calculate window for visualization
seed = 2023-04-06 ## RNG seed used by UMAP

## UMAP embeddings are not re-fit automatically.  To refit: 
## 1. Pass --force through the command line 
source(here('R', 'args.R'))
## 2. Set `force` manually here
# force = TRUE

## Load data ----
out_dir = here('out')
data_dir = here('data')
tm_dir = here('data', '04_tm')
prefix = '07'

source(here('R', 'author_data.R'))
meta_df = open_dataset(here('data', '01_metadata')) |> 
    left_join(author_data(), by = 'article_id') |> 
    filter(container.title != 'Psychological Reports') |> 
    collect() |>
    nest(authors = author)

source(here('R', 'phrases.R'))
phrases_ar = phrases()

## Workhorse function, generates visualization for a 
## single model (vocab x k)
process_k = function(k, 
                     model, 
                     vocab, 
                     exponents, 
                     seed = NA_integer_,
                     force = FALSE, 
                     visualization = TRUE,
                     spreadsheet = NULL) {
    message(glue('Processing {vocab}-{k}'))
    exponent = exponents[as.character(k)]
    
    gamma = tidy(model, k, matrix = 'gamma', exponent = exponent) |> 
        filter(document %in% meta_df$article_id)
    beta  = tidy(model, k, matrix = 'beta')
    
    ## Weighted phrases for each document ----
    gamma_max = gamma |> 
        group_by(document) |> 
        filter(gamma == max(gamma))
    
    weight_df = phrases_ar |> 
        inner_join(gamma_max, 
                   by = c('article_id' = 'document')) |> 
        inner_join(beta, 
                   by = c('phrase' = 'token', 
                          'topic' = 'topic')) |> 
        mutate(weight = n * beta) |> 
        collect() |> 
        group_by(article_id) |> 
        top_n(5, wt = weight)
    
    ## List of documents ----
    if (!is.null(spreadsheet)) {
        weight_df |> 
            filter(topic %in% spreadsheet) |> 
            group_by(topic, gamma, article_id) |> 
            summarize(phrases = str_c(phrase, collapse = '; ')) |> 
            ungroup() |> 
            arrange(topic, desc(gamma)) |> 
            left_join(meta_df, 
                      by = c('article_id'), 
                      multiple = 'all') |> 
            mutate(authors = map_chr(authors, 
                                     ~ {. |> 
                                             pull(author) |> 
                                             str_c(collapse = '; ')})) |> 
            select(topic, gamma, article_id, 
                   container.title, year, 
                   title, phrases, authors) |> 
            group_split(topic) |> 
            walk2(spreadsheet, 
                  ~ write_xlsx(.x, 
                               here('out', 
                                    glue('{prefix}_{vocab}_{k}_{.y}.xlsx'))))
    }
    
    ## UMAP embedding, if necessary ----
    if (!visualization) {return(TRUE)}
    
    umap_file = here(data_dir, glue('{prefix}_{vocab}_{k}_umap.Rds'))
    if (!file.exists(umap_file) || force) {
        message(glue('Calculating UMAP embedding for {vocab}-{k}'))
        umap_df = hellinger(gamma, prob1 = gamma) |> 
            umap(verbose = TRUE, min_dist = .2, random_state = seed)
        write_rds(umap_df, umap_file)
    } else {
        umap_df = read_rds(umap_file)
    }
    
    ## Static visualization ----
    window = umap_df |> 
        reframe(across(c(x, y), 
                       ~ quantile(.x, 
                                  probs = c(outliers, 1-outliers)))) |> 
        mutate(across(c(x, y), 
                      ~ .x * 1.15))
    
    umap_plot = weight_df |> 
        select(article_id, topic, phrase) |> 
        group_by(article_id, topic) |> 
        summarize(terms = {phrase |> 
                str_c(sep = '; ') |> 
                str_wrap(width = 50) |> 
                list()}) |> 
        inner_join(umap_df, by = c('article_id' = 'document')) |> 
        inner_join(meta_df, by = 'article_id') |> 
        mutate(title = str_wrap(title, width = 50)) |> 
        ggplot(aes(x, y, 
                   color = container.title, 
                   id = article_id, 
                   so = container.title, 
                   yr = year,
                   ti = title, 
                   tp = topic,
                   tr = terms)) +
        geom_point(aes(color = container.title), 
                   alpha = .25) + 
        geom_point(aes(color = topic), 
                   alpha = .25) +
        scale_color_manual(values = c(viridisLite::viridis(5), 
                                      viridisLite::viridis(k)), 
                           name = '') +
        lims(x = window$x, 
             y = window$y)
    
    if (interactive()) umap_plot
    
    ## Interactive visualization ----
    p = ggplotly(umap_plot, 
                 tooltip = c('id', 'so', 'yr', 'ti', 'tp', 'tr'), 
                 width = .8*1440, 
                 height = .8*.8*1080) |> 
        layout(legend = list(font = list(size = 12)), 
               hoverlabel = list(font = list(size = 12))) |> 
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
    
    write_rds(p, here('out', glue('{prefix}_{vocab}_{k}.Rds')))
    saveWidget(p, here('out', glue('{prefix}_{vocab}_{k}.html')))
    return(TRUE)
}

## Wrapper around a single model, loops through multiple values of k
make_umap = function(vocab, 
                     which_k, 
                     ...) {
    ## Load model and renormalization exponents ----
    model = here(tm_dir, glue('04_{vocab}_tmfast.Rds')) |> 
        read_rds()
    
    exponents = here(data_dir, glue('05_{vocab}_exponents.Rds')) |> 
        read_rds() |> 
        pull(exponent, name = k)
    
    ## Actually generate the vis
    walk(which_k, process_k, model, vocab, exponents, ...)
    return(TRUE)
}

# debugonce(process_k)
## Generate a single spreadsheet for topic quality check
# make_umap('md', 40, spreadsheet = 'V24', visualization = FALSE)
## Generate visualizations for all models of interest
make_umap('md', c(20, 30, 40, 50, 60, 70), seed = seed, force = force)
make_umap('sm', c(20, 30, 40, 50, 60, 70), seed = seed, force = force)
make_umap('lg', c(20, 30, 40, 50, 60, 70), seed = seed, force = force)
