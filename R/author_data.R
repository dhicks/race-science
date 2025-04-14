author_data = function(data_dir = here::here('data'), 
                       canon_file = '00_authors_canonicalized.csv') {
    mainstream_df = arrow::open_dataset(here(data_dir, 
                                      '00_authors_mainstr')) |> 
        select(article_id = doi, given, family) |> 
        mutate(author = paste(given, family), 
               author = str_to_title(author)) |> 
        collect()
    
    # Flip Lastname, Firstname names
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
    
    ## Canonicalized names
    canonicalized_df = read_csv(here(data_dir, canon_file)) |> 
        filter(!is.na(canonical)) |> 
        select(-...3)
    
    comb_df = bind_rows(mq_df, 
                        mainstream_df) |> 
        select(-given, -family) |> 
        filter(author != 'Na Na') |> 
        left_join(canonicalized_df, by = 'author') |> 
        mutate(author = if_else(!is.na(canonical), 
                                canonical, 
                                author)) |> 
        select(-canonical)
    return(comb_df)
}


pf = tibble(author = c('Thomas J. Bouchard, Jr.', 
                       'Brunetto Chiarelli',
                       'Hans J. Eysenck', 
                       'Robert A. Gordon',
                       'Linda S. Gottfredson', 
                       'Joseph M. Horn', 
                       'Lloyd G. Humphreys', 
                       'Arthur R. Jensen', 
                       'Michael Levin', 
                       'Richard Lynn', 
                       'R. Travis Osborne', 
                       'J. Philippe Rushton', 
                       'Audrey M. Shuey',
                       'Philip A. Vernon', 
                       'Daniel R. Vining, Jr.'
                       ))
