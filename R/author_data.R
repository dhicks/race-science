author_data = function(data_dir = here::here('data'), 
                       canon_file = 'scratch_authors_canonicalized.csv') {
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


mq = tibble(author = c('Arthur R. Jensen', 
                       'J. Philippe Rushton', 
                       'Michael Levin', 
                       'Robert M. Gordon', 
                       'Linda S. Gottfredson', 
                       'Daniel R. Vining, Jr.', 
                       'Richard Lynn', 
                       'R. Travis Osborne', 
                       'Hans J. Eysenck', 
                       'Thomas J. Bouchard, Jr.', 
                       'Joseph M. Horn', 
                       'Philip A. Vernon', 
                       'Brunetto Chiarelli',
                       'Lloyd G. Humphreys', 
                       'Audrey M. Shuey'))
