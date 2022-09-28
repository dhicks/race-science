# library(tidyverse)
# library(here)

author_data = function(data_dir = here::here('data')) {
    mainstream_df = arrow::open_dataset(here(data_dir, 
                                      '00_authors_mainstr')) |> 
        select(article_id = doi, given, family) |> 
        mutate(author = paste(given, family), 
               author = str_to_title(author)) |> 
        collect()
    
    #' Flip Lastname, Firstname names
    flip = function(string) {
        split = str_match(string, '([^,]+)(, )?(.+)?')
        if_else(is.na(split[,3]) | split[,4] == 'Jr.', 
                string, 
                paste(split[,4], split[,2]))
    }
    
    mq_df = read_csv(here(data_dir, '00_meta_mq.csv'), 
                     show_col_types = FALSE) |> 
        select(article_id, authors) |> 
        filter(!is.na(authors)) |> 
        mutate(authors = str_split(authors, ';')) |> 
        unnest(authors) |> 
        mutate(author = flip(authors)) |> 
        select(-authors)
    
    comb_df = bind_rows(mq_df, 
                        mainstream_df)
    return(comb_df)
}

# author_data() |> 
#     count(author) |> 
#     arrange(desc(n))
