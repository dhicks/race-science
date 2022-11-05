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

phrases_ar = open_dataset(here(data_dir, '00_phrases')) |> 
    select(-year) |> 
    mutate(n = log1p(n))

articles_df = phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    # filter(container.title != 'Psychological Reports') |> 
    collect()

sm_tmf = readRDS(here(tm_dir, '04_sm_tmfast.Rds'))
md_tmf = readRDS(here(tm_dir, '04_md_tmfast.Rds'))

md_tmf |> 
    tidy(k = 30, matrix = 'gamma') |> 
    filter(topic %in% c('V05', 'V07', 'V19')) |> 
    ggplot(aes(gamma, color = topic)) +
    stat_ecdf() +
    geom_hline(yintercept = .9)

create_dataframes = function(model, 
                             k = c(5, seq(10, 50, by = 10)), 
                             topics = NULL, 
                             articles = articles_df, 
                             gamma_treshhold = .05,
                             agg_fn = mean) {
    full_topic_list = list()
    
    for (i in k){
        out_prefix = as.character(i)
        ## Median value of gamma by year-topic-journal
        dataframe = tidy(model, k = i, matrix = 'gamma') |>
            filter(gamma > gamma_treshhold) |>
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

topics_time_sm = create_dataframes(sm_tmf, topics = 'V05')
topics_time_md = create_dataframes(md_tmf, topics = c('V07', 'V19', 'V05'))

ggplot(topics_time_md, aes(year, gamma, 
                           color = container.title, 
                           group = container.title)) +
    geom_line() +
    facet_grid(rows = vars(k), 
               cols = vars(topic), 
               scales = 'free_y')

topics_time_md_count = create_dataframes(md_tmf, 
                                         topics = c('V07', 'V19', 'V05'), 
                                         agg_fn = \(x)(n()), 
                                         # agg_fn = sum,
                                         gamma_treshhold = .07)
topics_time_md_count |> 
    filter(k == 30) |> 
    group_by(k, topic, container.title) |> 
    mutate(gamma_sm = slider::slide_index_dbl(gamma, year, 
                                              mean, 
                                              .before = 2, 
                                              .after = 2)) |> 
    ungroup() |> 
    ggplot(aes(year,  
               color = container.title, 
               group = container.title)) +
    geom_line(aes(y = gamma_sm), size = 1.25, color = 'black') +
    geom_line(aes(y = gamma), alpha = .5) +
    geom_line(aes(y = gamma_sm), size = 1) +
    facet_wrap(vars(topic), scales = 'free_y') +
    labs(y = expression(paste('#{', gamma, '>.07}')), 
         caption = 'Medium vocabulary, k = 30') +
    coord_cartesian(ylim = c(NA, NA)) +
    scale_color_viridis_d(option = 'D', 
                          name = '') +
    theme(legend.position = 'bottom')

ggsave(here(out_dir, '08_presentation.png'), 
       height = 5, width = 9, bg = 'white')
 