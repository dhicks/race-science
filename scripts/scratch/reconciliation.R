library(tidyverse)
library(here)
library(readxl)

new = read_excel(here('out', '07_md_30_V05.xlsx')) |> 
    mutate(rank = row_number())
old = read_excel(here('data', '07_md_30_v19-DJH.xlsx')) |> 
    mutate(rank = row_number())

comb = full_join(new, old, by = 'article_id', 
                 suffix = c('.new', '.old'))

ggplot(comb, aes(rank.old, rank.new)) +
    geom_point()

comb |> 
    select(container.title.new, 
           title.new, 
           authors.new,
           rank.new, rank.old, race_discourse) |> 
    view()
