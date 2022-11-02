library(tidyverse)
library(tidytext)
library(tmfast)
library(here)
library(arrow)
# remotes::install_github("mtennekes/cols4all")
library(cols4all)
library(patchwork)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

meta_ar = open_dataset(here(data_dir, '01_metadata'))

phrases_ar = open_dataset(here(data_dir, '00_phrases')) |> 
    select(-year) |> 
    mutate(n = log1p(n))

topics_over_time = phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
   # filter(container.title != 'Psychological Reports') |> 
    collect()

sm_tmf = readRDS(here(tm_dir, '04_sm_tmfast.Rds'))
md_tmf = readRDS(here(tm_dir, '04_md_tmfast.Rds'))

create_dataframes = function(model, k = c(5, seq(10, 50, by = 10)), topics = NULL) {
    
    full_topic_list = list()
    
    for (i in k){
        out_prefix = glue('df_{i}')
        dataframe = tidy(model, k = i, matrix = 'gamma') |>
            filter(gamma > .05) |>
            left_join(topics_over_time, by = c('document' = 'article_id')) |> 
            group_by(topic, container.title, year) |> 
            summarise(gamma = median(gamma))
        
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

test = create_dataframes(sm_tmf, topics = 'V05')
med_test = create_dataframes(md_tmf, topics = c('V07','V12'))
