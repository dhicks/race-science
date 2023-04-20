library(readxl)
library(irr)
library(here)

data_dir = here('data')

data = read_xlsx(here(data_dir, '07_md_30_V19_coded.xlsx'))

kappa2(data[,c(9,10)], "unweighted")

data2 = subset(data, DJH == 0 & EL == 0)

write.csv(data2, here(data_dir, "07_md_30_V19_false_positives.csv"), row.names = FALSE)
