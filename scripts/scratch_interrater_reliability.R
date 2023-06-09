library(tidyverse)
library(readxl)
library(irr)
library(here)

data_dir = here('data')

data_el = read_xlsx(here(data_dir, '07_md_30_V19 -EL codes.xlsx')) |> 
    filter(!is.na(race_discourse)) |> 
    rename(race_discourse_el = race_discourse, 
           notes_el = notes) |> 
    mutate(race_discourse_el = race_discourse_el == 1L)
data_djh = read_xlsx(here(data_dir, '07_md_30_v19-DJH.xlsx')) |> 
    filter(!is.na(race_discourse)) |> 
    rename(race_discourse_djh = race_discourse) |> 
    mutate(race_discourse_djh = race_discourse_djh == 'y')

data = inner_join(data_el, data_djh)

## documents coded
nrow(data)
## quick crosstab
## "false positive" rate is 15%; 25% non-consensus
count(data, race_discourse_djh, race_discourse_el) |> 
    mutate(share = n / sum(n))


kappa2(data[,c('race_discourse_djh','race_discourse_el')], 
       "unweighted")

data2 = subset(data, !race_discourse_djh & !race_discourse_el)

write.csv(data2, here(data_dir, "07_md_30_V19_false_positives.csv"), 
          row.names = FALSE)
