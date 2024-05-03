library(tidyverse)
library(tmfast)

library(here)
library(arrow)

source(here('R', 'author_data.R'))

author_data() |> 
    inner_join(mq, by = 'author') |> 
    count(author)

meta_df = open_dataset(here('data', '01_metadata')) |> 
    inner_join(author_data(), by = 'article_id') |> 
    inner_join(mq, by = 'author') |> 
    collect()

## Publications by journal ----
ggplot(meta_df, aes(fct_rev(author), fill = container.title)) +
    geom_bar(color = 'black', size = .25) +
    # scale_y_sqrt(breaks = seq(100, 900, by = 200)) +
    scale_fill_brewer(palette = 'Set1', direction = -1L) +
    coord_flip() +
    labs(x = 'author', 
         fill = '') +
    theme_minimal()
meta_df |> 
    count(author, container.title) |> 
    complete(author, container.title, fill = list(n = 0)) |> 
    group_by(author) |> 
    mutate(share = n/sum(n)) |> 
    ggplot(aes(author, container.title, fill = share)) +
    geom_raster() +
    geom_text(aes(label = n), color = 'white') +
    scale_fill_viridis_c(option = 'A')

## med vocab, k = 30 topics ----
model = read_rds(here('data', '04_tm', '04_md_tmfast.Rds'))

tidy(model, k = 30, matrix = 'gamma') |> 
    filter(gamma > .05) |> 
    inner_join(meta_df, by = c('document' = 'article_id')) |> 
    count(author, topic) |> 
    complete(author, topic, fill = list(n = 0)) |> 
    inner_join(count(meta_df, author), by = 'author') |> 
    mutate(share = n.x / n.y) |> 
    ggplot(aes(topic, author, fill = share)) +
    geom_raster() +
    scale_fill_viridis_c(option = 'A')
