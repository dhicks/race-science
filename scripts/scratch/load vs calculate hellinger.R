library(tidyverse)
library(here)
library(tictoc)
library(tmfast)

dir = here('data', '04_tm')

tic()
foo = read_rds(here(dir, '04_lg_5_hellinger.Rds'))
toc()

tic()
tm = read_rds(here(dir, '04_lg_tmfast.Rds'))
bar = tm |> 
    tidy(k = 5, matrix = 'gamma') |> 
    filter(document %in% labels(foo)) |> 
    hellinger(prob1 = gamma)
toc()
