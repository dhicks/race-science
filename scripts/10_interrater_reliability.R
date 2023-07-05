renv::load(here::here())
library(tidyverse)
library(readxl)
library(irr)
library(here)

data_dir = here('data')
out_dir = here('out')

data_el = read_xlsx(here(data_dir, '07_md_40_V24-EL.xlsx')) |> 
    filter(!is.na(race_science_discourse)) |> 
    rename(race_discourse_el = race_science_discourse, 
           notes_el = notes) |> 
    mutate(race_discourse_el = race_discourse_el == 'y')
data_djh = read_xlsx(here(data_dir, '07_md_40_V24-DJH.xlsx')) |> 
    filter(!is.na(race_science_discourse)) |> 
    rename(race_discourse_djh = race_science_discourse) |> 
    mutate(race_discourse_djh = race_discourse_djh == 'y')

# anti_join(data_djh, data_el, by = 'article_id') |> 
#     pull(article_id)

data = inner_join(data_el, data_djh, 
                  by = c('topic', 'gamma', 'article_id', 
                         'container.title', 'year', 'authors', 'title', 
                         'phrases', 'url'))

## documents coded
nrow(data)

## quick crosstabs
count(data_djh, race_discourse_djh) |> 
    mutate(share = n / sum(n))
count(data_el, race_discourse_el) |> 
    mutate(share = n / sum(n))
data |> 
    mutate(idx = row_number()) |> 
    select(idx, starts_with('race_discourse')) |> 
    pivot_longer(starts_with('race_discourse'), 
                 names_to = 'rater', 
                 values_to = 'race_discourse') |> 
    ggplot(aes(idx, rater, fill = race_discourse)) +
    geom_tile()
## "false positive" rate is 8.3%; 5% non-consensus
count(data, race_discourse_djh, race_discourse_el) |> 
    mutate(share = n / sum(n))

## 95% agreement
agree(data[,c('race_discourse_djh','race_discourse_el')])
## kappa 0.74
kappa2(data[,c('race_discourse_djh','race_discourse_el')], 
       "unweighted")
## Bennett, Alpert and Goldstein's S
## 0.90
(2 * agree(data[,c('race_discourse_djh','race_discourse_el')])$value/100 - 1) / (2-1)


data |> 
    filter(race_discourse_djh != race_discourse_el) |> #view()
    write_csv(here(out_dir, '10_discordance.csv'))

data2 = subset(data, !race_discourse_djh & !race_discourse_el)

write.csv(data2, here(out_dir, "10_false_positives.csv"),
          row.names = FALSE)
