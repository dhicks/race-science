## This scripts fits a STM topic model, as a robustness check
library(tidyverse)
theme_set(theme_bw())
library(stm)
library(tidytext)
library(ggh4x)
library(cowplot)

library(arrow)
library(here)
library(tictoc)
library(glue)

data_dir = here('data')
k = 40
vocab = 'md'

## Load data ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

## Medium vocab
vocabulary = here(data_dir, glue('03-vocab-{vocab}.Rds')) |> 
    read_rds()
source(here('R', 'phrases.R'))
phrases_mx = phrases() |> 
    filter(phrase %in% vocabulary$phrase) |> 
    collect() |> 
    arrange(article_id, phrase) |> 
    cast_sparse(article_id, phrase, n)

article_ids = rownames(phrases_mx)

## Fit topic model ----
## per note below, fitting model is very slow, 
## so we use a cache
model_path = glue('12_stm_{vocab}_{k}.Rds') %>%
    here(data_dir, .)
if (!file.exists(model_path)) {
    ## w/ emtol = 1e-4, fitting model takes 1190 sec ~ 20 minutes
    ## (versus 80+ with 1e-5)
    tic()
    stm_fit = stm(phrases_mx, K = k, seed = 2024-03-29, emtol = 1e-4)
    toc()
    write_rds(stm_fit, model_path)
} else {
    stm_fit = read_rds(model_path)
}


## Match stm and tmfast topics ----
beta_stm = tidy(stm_fit, matrix = 'beta') |>
    mutate(topic = str_pad(topic, 2)) |>
    arrange(term, topic)

beta_tmfast = glue('04_{vocab}_tmfast.Rds') %>%
    here(data_dir, '04_tm', .) |>
    read_rds() |>
    tmfast::tidy(k = 40, matrix = 'beta') |>
    arrange(token, topic)

# magrittr::equals(
#     {tmfast:::build_matrix(beta_stm, topic, term, beta, sparse = FALSE) |>
#             colnames()},
#     {tmfast:::build_matrix(beta_tmfast, topic, token, beta, sparse = FALSE) |>
#             colnames()}
# ) |>
#     all()

hellingers = tmfast:::hellinger.data.frame(
    beta_stm,    id1 = topic, cat1 = term,  prob1 = beta,
    beta_tmfast, id2 = topic, cat2 = token, prob2 = beta,
    df = TRUE)

ggplot(hellingers, aes(topic_x, topic_y, fill = dist)) +
    geom_raster()

soln = tmfast:::build_matrix(hellingers,
                             topic_x,
                             topic_y,
                             dist,
                             sparse = FALSE) |>
    lpSolve::lp.assign()

matches = soln$solution |>
    magrittr::set_rownames(unique(hellingers$topic_x)) |>
    magrittr::set_colnames(unique(hellingers$topic_y)) |>
    as_tibble(rownames = 'topic') |>
    pivot_longer(-topic, names_to = 'topic_match', values_to = 'match') |>
    filter(match != 0) |>
    arrange(topic_match) |>
    select(-match)

## tile plot indicates that lpSolve generally matched up stm topics with the closest tmfast topic
## but notice the minimum Hellinger distance is 0.4
hellingers |>
    left_join(matches, by = c('topic_x' = 'topic')) |>
    mutate(topic_fct = fct_relevel(topic_x,
                                   matches$topic)) |>
    ggplot(aes(topic_fct, topic_y, fill = dist)) +
    geom_raster()

## In particular, for our focal topics the distances are ~.45-.55
hellingers |>
    left_join(matches, by = c('topic_x' = 'topic')) |>
    filter(topic_y == topic_match,
           topic_match %in% c('V07', 'V22', 'V24'))


## Topic-doc tile ----
tidy(stm_fit, matrix = 'gamma', document_names = article_ids) |>
    mutate(topic = str_pad(topic, 2)) |>
    left_join(matches, by = 'topic') |>
    left_join(collect(meta_ar), by = c('document' = 'article_id')) |>
    ggplot(aes(topic_match, document, fill = log1p(gamma))) +
    geom_raster() +
    geom_vline(xintercept = c(10, 20, 30, 40) + .5,
               color = 'black') +
    facet_grid(rows = vars(container.title),
               scales = 'free',
               labeller = label_wrap_gen(width = 15),
               switch = 'y',
               space = 'free_y') +
    scale_x_discrete(breaks = c('10', '20', '30', '40')) +
    scale_y_discrete(breaks = NULL,
                     name = '') +
    scale_fill_viridis_c(option = 'viridis', direction = 1) +
    theme_minimal() +
    theme(axis.ticks.x = element_line(),
          legend.position = 'bottom',
          plot.margin = margin(r = 10))

# Silge plots ----
tidy(stm_fit, matrix = 'beta') |>
    mutate(topic = str_pad(topic, 2)) |>
    left_join(matches, by = 'topic') |>
    group_by(topic) |>
    top_n(10, beta) |>
    ungroup() |>
    arrange(topic, desc(beta)) |>
    mutate(term = reorder_within(term,
                                 beta,
                                 topic)) |>
    ggplot(aes(term, beta)) +
    geom_point() +
    geom_linerange(aes(ymax = beta), ymin = 0) +
    scale_x_reordered() +
    scale_y_continuous(
        breaks = scales::pretty_breaks(n = 3)) +
    coord_flip(ylim = c(0, NA)) +
    facet_wrap(vars(topic_match),
               scales = 'free',
               ncol = 5) +
    theme(plot.background =
              element_rect(colour = "black",
                           fill = NA,
                           linewidth = 1))

## Race and/or intelligence topics ----
create_dataframes = function(model, 
                             topics = NULL, 
                             article_id = article_ids,
                             article_meta = meta_ar, 
                             gamma_threshold = .05,
                             agg_fn = mean) {
    full_topic_list = list()
    
    for (i in k){
        out_prefix = as.character(i)
        ## Median value of gamma by year-topic-journal
        dataframe = tidy(model, 
                         matrix = 'gamma', 
                         document_names = article_id) |>
            mutate(topic = str_pad(topic, 2)) |>
            left_join(matches, by = 'topic') |>
            filter(gamma > gamma_threshold) |>
            left_join(collect(article_meta), 
                      by = c('document' = 'article_id')) |> 
            group_by(topic, topic_match, 
                     container.title, year) |> 
            summarise(gamma = agg_fn(gamma), 
                      .groups = 'drop')
        
        if (is.null(topics)) {
            #what to do if topics is null
            full_topic_list[out_prefix] = list(dataframe)
            
        } else {
            #what to do if topics is not null
            #this is where filtering happens
            dataframe = dataframe |> 
                filter(topic %in% topics)
            full_topic_list[out_prefix] = list(dataframe)
        }
    }
    return(bind_rows(full_topic_list, .id = 'k'))
}
# debugonce(create_dataframes)
# create_dataframes(stm_fit, topics = 16,
#                   gamma_threshold = .5, agg_fn = \(x)(n()))

count_plot = function(tmf, vocab, topics, threshold, 
                      plot = TRUE, # return the plot? or just the dataframe
                      annotate = TRUE # fancy y-axis and notes in caption
) { 
    if (identical(length(topics), 0L)) {
        return(ggplot())
    }
    
    dataf = create_dataframes(tmf, topics, 
                              agg_fn = \(x)(n()), 
                              gamma_threshold = threshold) |> 
        group_by(k, topic, topic_match, container.title) |> 
        complete(year = seq.int(min(year), max(year)), 
                 fill = list('gamma' = 0)) |> 
        mutate(gamma_sm = slider::slide_index_dbl(gamma, year, 
                                                  mean, 
                                                  .complete = TRUE,
                                                  .before = 2, 
                                                  .after = 2)) |> 
        ungroup()
    if (!plot) {
        return(dataf)
    }
    plot = ggplot(dataf, 
                  aes(year,  
                      color = container.title, 
                      group = container.title)) +
        geom_line(aes(y = gamma_sm), linewidth = 1.25, color = 'black') +
        geom_line(aes(y = gamma), alpha = .5) +
        geom_line(aes(y = gamma_sm), linewidth = 1) +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        scale_color_viridis_d(option = 'D', 
                              name = '', 
                              aesthetics = c('color', 'fill')) +
        theme(legend.position = 'bottom')
    if (length(topics) > 1) {
        plot = plot + facet_wrap(vars(topic_match), scales = 'free_y')
    }
    if (annotate) {
        y_label = glue('#{ɣ > [threshold]}', .open = '[', .close = ']')
        plot = plot + labs(y = y_label, 
                           caption = glue('{vocab} vocabulary'))
    }
    return(plot)
}
# count_plot(stm_fit, 'stm', 16, .5, plot = TRUE)

big_grid = function(model, name, plot = TRUE, verbose = TRUE) {
    if (verbose) message(glue('{name} vocabulary'))
    
    ## Identify topics of interest: 
    ## race and/or intelligence keywords in top 15 terms
    topics = tidy(model, matrix = 'beta') |> 
        mutate(topic = str_pad(topic, 2)) |> 
        group_by(topic) |> 
        top_n(15, beta) |> 
        arrange(topic, beta) |> 
        summarize(race = any(str_detect(term, 'race|whites|blacks')), 
                  intelligence = any(str_detect(term, 'intelligence|iq') & 
                                         term != 'emotional_intelligence')) |> 
        ungroup() |> 
        filter(race|intelligence) |> 
        mutate(type = case_when(race & !intelligence ~ 'race', 
                                !race & intelligence ~ 'intelligence',
                                race & intelligence ~ 'both', 
                                TRUE ~ NA_character_),
               type = fct_relevel(type, 'race', 'both', 'intelligence'))
    if (verbose) message(glue('{nrow(topics)} topics of interest'))
    
    ## Create dataframes and 5-year running averages
    dataf = topics |> 
        group_by(type) |> 
        summarize(topics = list(topic)) |> 
        rowwise() |> 
        mutate(dataf = list(count_plot(model, name, 
                                       topics, 
                                       threshold = .5,
                                       plot = FALSE))) |> 
        ungroup() |> 
        hoist(.col = 'dataf', 
              'topic', 'topic_match', 
              'container.title', 'year', 'gamma', 'gamma_sm') |> 
        select(-dataf, -topics) |> 
        unnest_longer(c(topic, topic_match, 
                        container.title, year, gamma, gamma_sm))
    
    if (!plot) {
        return(dataf)
    }
    ggplot(dataf, 
           aes(year, color = container.title, group = container.title)) +
        facet_grid2(cols = vars(type, topic),
                    scales = 'free_y', independent = 'y',
                    switch = 'y',
                    strip = strip_nested(by_layer_x = TRUE, 
                                         background_x = list(element_rect(linewidth = 1), NULL), 
                                         background_y = list(element_rect(linewidth = 1))),
                    drop = TRUE) +
        ## The rest is just copy-pasted from count_plot()
        geom_line(aes(y = gamma_sm), linewidth = 1.25, color = 'black') +
        geom_line(aes(y = gamma), alpha = .5) +
        geom_line(aes(y = gamma_sm), linewidth = 1) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 4), 
                           name = 'article count') +
        scale_color_viridis_d(option = 'D',
                              name = '') +
        theme_minimal() +
        theme(legend.position = 'bottom') +
        labs(caption = glue('{name} vocabulary'))
}
# debugonce(big_grid)
grid_gg = big_grid(stm_fit, glue('stm-{vocab}-{k}'), plot = TRUE)
grid_gg
ggsave(here('out', glue('12-grid-stm.png')), 
       height = 4, width = 8, bg = 'white')

## Combine w/ Silge plot
focal_topics = c(16, 40, 9, 33)
silge_gg = tidy(stm_fit, matrix = 'beta') |> 
    filter(topic %in% focal_topics) |> 
    mutate(topic = factor(topic, levels = focal_topics)) |> 
    group_by(topic) |> 
    top_n(15, beta) |> 
    arrange(topic, desc(beta)) |> 
    mutate(term = reorder_within(term, beta, topic)) |> 
    ggplot(aes(term, beta)) +
    geom_point() +
    geom_linerange(aes(ymax = beta), ymin = 0) +
    coord_flip() +
    scale_x_reordered() + 
    scale_y_continuous(name = 'β', 
                       breaks = scales::pretty_breaks(3), 
                       limits = c(0, NA)) +
    facet_wrap(vars(topic), scales = 'free_y', nrow = 1)
silge_gg

plot_grid(silge_gg, grid_gg, ncol = 1, align = 'v', 
          rel_heights = c(.7, 1))
ggsave(here('out', glue('12-stm.png')), 
       height = 6, width = 8, bg = 'white', scale = 1.5)
