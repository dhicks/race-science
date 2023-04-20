phrases = function() {
    open_dataset(here(data_dir, '00_phrases'))  |> 
        select(-year) |> 
        filter(!is.na(phrase))
}

