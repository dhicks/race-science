pf_authors_ms = list(
    Bouchard = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Bouchard'), 
                   str_detect(given, '^T')) |> 
            # count(family, given) |> 
            collect()},
    Eysenck = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Eysenck'), 
                   str_detect(given, '^H')
            ) |> 
            # count(family, given) |> 
            collect()},
    Gottfredson = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Gottfredson'), 
                   str_detect(given, '^L')
            ) |> 
            # count(family, given) |> 
            collect()},
    Horn = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Horn'), 
                   str_detect(given, '^J.*L')
            ) |> 
            # count(family, given) |>
            collect()},
    Humphreys = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Humphreys'), 
                   str_detect(given, 'L.*G')) |> 
            collect()},
    Jensen = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Jensen'), 
                   str_detect(given, '^A.*R')) |> 
            collect()},
    Levin = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Levin'), 
                   str_detect(given, 'Michael')) |> 
            collect()},
    Lynn = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Lynn'), 
                   str_detect(given, '^R')) |> 
            collect()},
    Osborne = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Osborne'), 
                   str_detect(given, '^R.*T')) |> 
            collect()},
    Rushton = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Rushton'), 
                   str_detect(given, '^J')
            ) |> 
            # count(family, given) |>
            collect()},
    Shuey = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Shuey')) |> 
            collect()},
    Vernon = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Vernon'), 
                   str_detect(given, '^P.*A')
            ) |> 
            count(family, given) |>
            collect()},
    Vining = {here(data_dir, '00_authors_mainstr') |> 
            open_dataset() |> 
            filter(str_detect(family, 'Vining'), 
                   str_detect(given, '^D')
            ) |> 
            # count(family, given) |>
            collect()}
) |> 
    bind_rows(.id = 'pf_author') |> 
    select(pf_author, article_id = doi)

pf_authors_mq = suppressMessages(list(
    Chiarelli = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Chiarelli')),
    Eysenck = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Eysenck')),
    Horn = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Horn')),
    Levin = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Levin')),
    Lynn = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Lynn')),
    Osborne = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Osborne')),
    Rushton = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Rushton')),
    Vining = read_csv(here(data_dir, '00_meta_mq.csv')) |> 
        filter(str_detect(authors, 'Vining'))
)) |> 
    bind_rows(.id = 'pf_author') |> 
    select(pf_author, article_id)


pf_authors = bind_rows(pf_authors_ms, 
          pf_authors_mq)
