renv::load(here::here())
library(tidyverse)
library(tidytext)
theme_set(theme_bw())

library(tmfast)
library(here)
library(arrow)
# library(patchwork)
library(cowplot)
library(glue)
library(ggh4x)
library(slider)

## Load data ----
data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

meta_ar = open_dataset(here(data_dir, '01_metadata'))

source(here('R', 'phrases.R'))
phrases_ar = phrases() |> 
    mutate(n = log1p(n))

articles_df = phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    # filter(container.title != 'Psychological Reports') |> 
    collect()

model_files = list.files(tm_dir, '*_tmfast.Rds') %>%
    here(tm_dir, .) |> 
    set_names('lg', 'md', 'sm')
exp_files = list.files(data_dir, '*_exponents.Rds') %>%
    here(data_dir, .) |> 
    set_names('lg', 'md', 'sm')
## Helper to read exponents as a named num
read_exp = function(file) {
    file |> 
        read_rds() |> 
        pull(exponent, name = k)
}


## Create a dataframe of aggregated gamma scores by year ----
create_dataframes = function(model, 
                             exponents,
                             k = c(5, seq(10, 70, by = 10)), 
                             topics = NULL, 
                             articles = articles_df, 
                             gamma_threshold = .05,
                             agg_fn = mean) {
    full_topic_list = list()
    
    for (i in k){
        out_prefix = as.character(i)
        ## Median value of gamma by year-topic-journal
        dataframe = tidy(model, 
                         k = i, 
                         matrix = 'gamma', 
                         exponent = exponents[as.character(i)]) |>
            filter(gamma > gamma_threshold) |>
            left_join(articles, by = c('document' = 'article_id')) |> 
            group_by(topic, container.title, year) |> 
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

## Ex: Mean value of gamma, no threshold
# create_dataframes(md_tmf, md_exp,
#                   topics = c('V07', 'V22', 'V05', 'V24'), 
#                   gamma_threshold = 0) |> 
#     ggplot(aes(year, gamma, 
#                color = container.title, 
#                group = container.title)) +
#     geom_line() +
#     facet_grid(rows = vars(k), 
#                cols = vars(topic), 
#                scales = 'free_y')

## Mean value of gamma, 1% threshold
# create_dataframes(md_tmf, md_exp,
#                   topics = c('V07', 'V22', 'V05', 'V24'), 
#                   gamma_threshold = .1) |> 
#     ggplot(aes(year, gamma, 
#                color = container.title, 
#                group = container.title)) +
#     geom_line() +
#     facet_grid(rows = vars(k), 
#                cols = vars(topic), 
#                scales = 'free_y')

## Mean value of gamma, 25% threshold
# create_dataframes(md_tmf, md_exp,
#                   topics = c('V07', 'V22', 'V05'), 
#                   gamma_threshold = .25) |> 
#     ggplot(aes(year, gamma, 
#                color = container.title, 
#                group = container.title)) +
#     geom_line() +
#     facet_grid(rows = vars(k), 
#                cols = vars(topic), 
#                scales = 'free_y')

## "Count plot" ----
## Count documents "in" a topic (gamma > threshold); 
## smooth with 5-year running average; 
## and return a plot
count_plot = function(tmf, vocab, exp, this_k, topics, threshold, 
                      plot = TRUE, # return the plot? or just the dataframe
                      annotate = TRUE # fancy y-axis and notes in caption
) { 
    if (identical(length(topics), 0L)) {
        return(ggplot())
    }
    
    dataf = create_dataframes(tmf, exp, topics, k = this_k,
                              agg_fn = \(x)(n()), 
                              gamma_threshold = threshold) |> 
        group_by(k, topic, container.title) |> 
        complete(year = seq.int(min(year), max(year)), 
                 fill = list('gamma' = 0)) |> 
        mutate(gamma_sm = slide_index_dbl(gamma, year, 
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
        plot = plot + facet_wrap(vars(topic), scales = 'free_y')
    }
    if (annotate) {
        y_label = glue('#{ɣ > [threshold]}', .open = '[', .close = ']')
        plot = plot + labs(y = y_label, 
                           caption = glue('{vocab} vocabulary, k = {this_k}'))
    }
    return(plot)
}


## Individual model plots ----
md_tmf = read_rds(model_files['md'])
md_exp = read_exp(exp_files['md'])

## Similar trends for low, medium, and high threshold
plot_50 = count_plot(md_tmf, 'medium', md_exp, 40, c('V07', 'V24', 'V22'), .50)
if (interactive()) {
    count_plot(md_tmf, 'medium', md_exp, 40, c('V07', 'V24', 'V22'), .25)
    # plot_50 = count_plot(md_tmf, 'medium', md_exp, 40, c('V07', 'V24', 'V22'), .50)
    plot_50
    count_plot(md_tmf, 'medium', md_exp, 40, c('V07', 'V24', 'V22'), .80)
}

## Direct labelled version of plot_50
jr_labels = tribble(
    ~ topic, ~ container.title, ~ label, ~ date, ~ y,
    'V22', 'Personality and Individual Differences', 'Person. & Indiv. Diff.', 1985, 13,
    'V22', 'Intelligence', 'Intelligence', 1985, 10,
    'V07', 'Mankind Quarterly', 'Mankind Quarterly', 1995, 30, 
    'V24', 'Psychological Reports', 'Psychological Reports', 1987, 7
)

time_gg = plot_50 +
    geom_label(aes(x = date, y = y, label = label, 
                   fill = container.title), 
               color = 'black', size = 3,
               data = jr_labels)
if (interactive()) time_gg

# ggsave(here(out_dir, '08_focal_topics.png'), 
#        plot = time_gg,
#        height = 5, width = 9, bg = 'white')

silge_gg = tidy(md_tmf, matrix = 'beta', k = 40) |> 
    filter(topic %in% c('V07', 'V22', 'V24')) |> 
    group_by(topic) |> 
    top_n(15, beta) |> 
    arrange(topic, desc(beta)) |> 
    mutate(token = reorder_within(token, beta, topic)) |> 
    ggplot(aes(token, beta)) +
    geom_point() +
    geom_linerange(aes(ymax = beta), ymin = 0) +
    coord_flip() +
    scale_x_reordered() + 
    labs(y = 'β') +
    facet_wrap(vars(topic), scales = 'free_y')
if (interactive()) silge_gg

# silge_gg / time_gg +
#     plot_layout()

focal_topics_gg = plot_grid(silge_gg, time_gg, ncol = 1, align = 'v', 
                   rel_heights = c(.7, 1))
if (interactive()) focal_topics_gg

ggsave(here(out_dir, '08_focal_topics.png'), 
       plot = focal_topics_gg,
       width = 7.1, height = .6*8.7, scale = 1.4,
       bg = 'white')

## Big grid ----
big_grid = function(model_file, exp_file, name, plot = TRUE, verbose = TRUE) {
    if (verbose) message(glue('{name} vocabulary'))
    
    model = read_rds(model_file)
    exp = read_exp(exp_file)
    
    ## Identify topics of interest: 
    ## race and/or intelligence keywords in top 15 terms
    topics = tidy_all(model) |> 
        filter(k > 5) |> 
        group_by(k, topic) |> 
        top_n(15, beta) |> 
        arrange(k, topic, beta) |> 
        summarize(race = any(str_detect(token, 'race|whites|blacks')), 
                  intelligence = any(str_detect(token, 'intelligence|iq') & 
                                         token != 'emotional_intelligence')) |> 
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
        group_by(k, type) |> 
        summarize(topics = list(topic)) |> 
        rowwise() |> 
        mutate(dataf = list(count_plot(model, name, exp, 
                                       k, topics, 
                                       threshold = .5,
                                       plot = FALSE))) |> 
        ungroup() |> 
        hoist(.col = 'dataf', 
              'topic', 'container.title', 'year', 'gamma', 'gamma_sm') |> 
        select(-dataf, -topics) |> 
        unnest_longer(c(topic, container.title, year, gamma, gamma_sm))
    
    if (!plot) {
        return(dataf)
    }
    ggplot(dataf, 
           aes(year, color = container.title, group = container.title)) +
        facet_grid2(rows = vars(k),
                    cols = vars(type, topic),
                    scales = 'free', independent = 'all',
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
        scale_y_continuous(breaks = scales::pretty_breaks(n = 3), 
                           name = 'article count') +
        scale_color_viridis_d(option = 'D',
                              name = '') +
        theme_minimal() +
        theme(legend.position = 'bottom') +
        labs(caption = glue('{name} vocabulary'))
}
# foo = big_grid(model_files['sm'], exp_files['sm'], 'small')
# foo
# ggsave(here(out_dir, '08_grid_test.pdf'), height = 10, width = 14)

grid_wrapper = function(model, name, width) {
    big_grid(model_files[model], exp_files[model], name) %>%
        ggsave(here(out_dir, glue('08_grid_{model}.pdf')), 
               plot = ., 
               height = 10, width = width * 1.4)
}

tribble(~ model, ~ name, ~ width, 
        'sm', 'small', 10, 
        'md', 'medium', 13, 
        'lg', 'large', 18) |> 
    pwalk(grid_wrapper, .progress = TRUE)
    # walk(~ {big_grid(model_files[.x[[1]]], exp_files[.x[[1]]], .x[[2]]) %>% 
    #         ggsave(here(out_dir, glue('08_grid_{.x[[1]]}.pdf')), 
    #                plot = .,
    #                height = 10, width = .x[[3]]*1.4)}, 
    #      .progress = TRUE)

