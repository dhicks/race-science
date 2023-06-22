renv::load(here::here())
library(tidyverse)
library(tidytext)
theme_set(theme_bw())

library(tmfast)
library(here)
library(arrow)
# remotes::install_github("mtennekes/cols4all")
# library(cols4all)
library(patchwork)
library(glue)

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

# sm_tmf = readRDS(here(tm_dir, '04_sm_tmfast.Rds'))
md_tmf = readRDS(here(tm_dir, '04_md_tmfast.Rds'))

md_exp = read_rds(here(data_dir, '05_md_exponents.Rds')) |> 
    pull(exponent, name = k)

md_tmf |> 
    tidy(k = 30, matrix = 'gamma', exponent = md_exp['30']) |> 
    filter(topic %in% c('V05', 'V07', 'V22')) |> 
    ggplot(aes(gamma, color = topic)) +
    stat_ecdf() +
    geom_hline(yintercept = .9)

create_dataframes = function(model, 
                             exponents,
                             k = c(5, seq(10, 50, by = 10)), 
                             topics = NULL, 
                             articles = articles_df, 
                             gamma_threshold = .05,
                             agg_fn = mean) {
    ## Create a dataframe of aggregated gamma scores by year
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

## Mean value of gamma, no threshold
create_dataframes(md_tmf, md_exp,
                  topics = c('V07', 'V22', 'V05', 'V24'), 
                  gamma_threshold = 0) |> 
    ggplot(aes(year, gamma, 
               color = container.title, 
               group = container.title)) +
    geom_line() +
    facet_grid(rows = vars(k), 
               cols = vars(topic), 
               scales = 'free_y')

## Mean value of gamma, 1% threshold
create_dataframes(md_tmf, md_exp,
                  topics = c('V07', 'V22', 'V05', 'V24'), 
                  gamma_threshold = .1) |> 
    ggplot(aes(year, gamma, 
               color = container.title, 
               group = container.title)) +
    geom_line() +
    facet_grid(rows = vars(k), 
               cols = vars(topic), 
               scales = 'free_y')

## Mean value of gamma, 25% threshold
create_dataframes(md_tmf, md_exp,
                  topics = c('V07', 'V22', 'V05'), 
                  gamma_threshold = .25) |> 
    ggplot(aes(year, gamma, 
               color = container.title, 
               group = container.title)) +
    geom_line() +
    facet_grid(rows = vars(k), 
               cols = vars(topic), 
               scales = 'free_y')

count_plot = function(tmf, vocab, exp, this_k, topics, threshold, 
                      plot = TRUE,
                      position = 'identity', 
                      annotate = TRUE) {
    if (identical(length(topics), 0L)) {
        return(ggplot())
    }
    y_label = glue('#{ɣ > [threshold]}', 
                   .open = '[', .close = ']')
    
    dataf = create_dataframes(tmf, exp, topics, k = this_k,
                      agg_fn = \(x)(n()), 
                      gamma_threshold = threshold) |> 
        group_by(k, topic, container.title) |> 
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
        geom_line(aes(y = gamma_sm), size = 1.25, color = 'black') +
        geom_line(aes(y = gamma), alpha = .5) +
        geom_line(aes(y = gamma_sm), size = 1) +
        coord_cartesian(ylim = c(NA, NA)) +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        scale_color_viridis_d(option = 'D', 
                              name = '') +
        theme(legend.position = 'bottom')
    if (length(topics) > 1) {
        plot = plot + facet_wrap(vars(topic), scales = 'free_y')
    }
    if (annotate) {
        plot = plot + labs(y = y_label, 
                    caption = glue('{vocab} vocabulary, k = {this_k}'))
    }
    return(plot)
}

count_plot(md_tmf, 'medium', md_exp, 40, c('V05', 'V07', 'V22', 'V24'), .25)

## Count, 7% threshold, w/ rolling average
count_plot(md_tmf, 'medium', md_exp, 40, c('V05', 'V07', 'V22', 'V24'), .07)
## Count, 25% threshold, w/ rolling average
count_plot(md_tmf, 'medium', md_exp, 40, c('V05', 'V07', 'V22', 'V24'), .25)
## Count, 50% threshold, w/ rolling average
count_plot(md_tmf, 'medium', md_exp, 40, c('V05', 'V07', 'V22', 'V24'), .50)
## Count, 80% threshold, w/ rolling average
count_plot(md_tmf, 'medium', md_exp, 40, c('V05', 'V07', 'V22', 'V24'), .80)

ggsave(here(out_dir, '08_presentation.png'), 
       height = 5, width = 9, bg = 'white')
 