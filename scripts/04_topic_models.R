## Fit topic models using tmfast
## TODO: abstract process of loading vocab and fitting, to facilitate fitting multiple models (vocab size x k)
library(tidyverse)
theme_set(theme_bw())
# remotes::install_github('dhicks/tmfast')
library(tmfast)

library(arrow)
library(here)
library(tictoc)
library(glue)

data_dir = here('data')

k = c(5, seq(10, 50, by = 10))

## Load data ----
meta_ar = open_dataset(here(data_dir, '01_metadata'))

## Article IDs after dropping Psychological Reports
article_ids = meta_ar |> 
    filter(container.title != 'Psychological Reports') |> 
    pull(article_id)

phrases_ar = open_dataset(here(data_dir, '00_phrases')) |> 
    select(-year) |> 
    mutate(n = log1p(n))

vocabs = list.files(here(data_dir), '03-vocab') %>%
    here(data_dir, .) |> 
    set_names(c('lg', 'md', 'sm')) |> 
    map(read_rds)

## Loop over vocabs ----
process_vocab = function(this_vocab, vocab_name, force = FALSE) {
    message(glue('Vocabulary {vocab_name} ----------------------------------------------'))
    file_prefix = str_c('04_', vocab_name)
    make_path = function(...) {
        here(data_dir, '04_tm', str_c(file_prefix, ..., sep = '_'))
    }
    
    this_vocab = this_vocab$phrase
    
    phrases_df = phrases_ar |> 
        filter(phrase %in% this_vocab) |> 
        collect()
    
    ## What's dropped with this vocabulary? 
    dropped_file = make_path('dropped.csv')
    if (file.exists(dropped_file) || force) {
        dropped_df = read_csv(dropped_file, show_col_types = FALSE)
    } else {
        dropped_df = anti_join(meta_ar, phrases_df, by = 'article_id') |> 
            count(container.title) |> 
            collect()
        write_csv(dropped_df, make_path('dropped.csv'))
    }
    message('Dropped documents for this vocabulary')
    capture.output(dropped_df, type = 'output') |> 
        str_c(collapse = '\n') |> 
        message()
    
    ## Fit topic models ----
    ## Like 20 sec w/ 6 models + sm vocab
    ## ~240 sec w/ 6 models + md vocab
    tm_file = make_path('tmfast.Rds')
    if (file.exists(tm_file) || force) {
        message('Found pre-fitted topic models')
        topic_models = read_rds(tm_file)
    } else {
        message('Fitting topic models')
        tic()
        topic_models = tmfast(phrases_df, 
                              n = k,
                              article_id, 
                              phrase, 
                              n)
        toc()
        write_rds(topic_models, tm_file)
    }
    
    ## Loop over values of k ----
    process_k = function(this_k, force = FALSE) {
        message(glue('k = {this_k}'))
        
        ## Hellinger distance ----
        hellinger_file = make_path(this_k, 'hellinger.Rds')
        # message(hellinger_file)
        if (file.exists(hellinger_file) || force) {
            message('Found pre-calculated Hellinger distances')
            dist_mx = read_rds(hellinger_file)
        } else {
            message('Calculating Hellinger distances')
            tic()
            dist_mx = topic_models |> 
                tidy(this_k, matrix = 'gamma') |> 
                filter(document %in% article_ids) |> 
                hellinger(prob1 = 'gamma') |> 
                as.dist()
            toc()
            write_rds(dist_mx, hellinger_file)
        }
        
        ## UMAP ----
        ## w/o filtering Psychological Reports: 
        ## Memory use maxes out ~29 GB, ~24 min
        ## Nearest neighbors is clearly the most intensive phase
        ## [2022-08-18 16:44:55]  starting umap
        ## [2022-08-18 16:44:56]  creating graph of nearest neighbors
        ## [2022-08-18 17:07:10]  creating initial embedding
        ## [2022-08-18 17:07:17]  optimizing embedding
        ## [2022-08-18 17:08:22]  done
        umap_file = make_path(this_k, 'umap.Rds')
        if (file.exists(umap_file) || force) {
            message('Found pre-calculated UMAP projection')
        } else {
            message('Calculating UMAP projection')
            tic()
            space = dist_mx |> 
                as.matrix() |> 
                umap(verbose = TRUE, df = TRUE)
            toc()
            write_rds(space, umap_file)
        }
    }
    
    map(k, process_k, force)
    return(TRUE)
}

# process_vocab(vocabs$sm, 'sm')
iwalk(vocabs, process_vocab)


