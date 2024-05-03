library(stm)

tic()
test_stm = phrases_ar |> 
    filter(phrase %in% vocabs$sm$phrase) |> 
    mutate(n = as.integer(exp(n) - 1)) |> 
    collect() |> 
    build_matrix(article_id, phrase, n) |> 
    stm(K = 30)
toc()



tmf = read_rds(here(data_dir, '04_tm', '04_sm_tmfast.Rds'))

beta_tmf = tidy(tmf, k = 30, matrix = 'beta')
beta_stm = tidy(test_stm, matrix = 'beta')

source(here('R', 'compare_betas.R'))
compare_betas(beta_tmf, rename(beta_stm, token = term), 
              vocab = unique(vocabs$sm$phrase)) |> 
    as.matrix() |> 
    as_tibble(rownames = 'tmf_topic') |> 
    pivot_longer(cols = -tmf_topic, 
                 names_to = 'stm_topic', 
                 values_to = 'distance') |> 
    ggplot(aes(tmf_topic, stm_topic, fill = distance)) +
    geom_raster()
