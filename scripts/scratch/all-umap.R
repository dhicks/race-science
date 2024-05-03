library(tidyverse)

library(arrow)
library(here)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')

meta_df = open_dataset(here(data_dir, '01_metadata')) |> 
    collect()

umap_files = list.files(tm_dir, 'umap')
umap_df = umap_files %>%
    here(tm_dir, .) |> 
    set_names(umap_files) |> 
    map_dfr(read_rds, .id = 'file') |> 
    separate(file, 
             into = c('script', 'vocabulary', 'k', 'umap'), 
             sep = '_') |> 
    select(-script, -umap) |> 
    mutate(k = as.integer(k))

bounds = umap_df |> 
    group_by(vocabulary, k) |> 
    summarize(across(c(x, y), 
                     c(bot = ~ quantile(.x, .05), 
                       top = ~ quantile(.x, .95)))) |> 
    ungroup()


umap_df |> 
    left_join(bounds, by = c('vocabulary', 'k')) |> 
    filter(x_bot <= x, x <= x_top,
           y_bot <= y, y <= y_top) |> 
    left_join(meta_df, by = c('document' = 'article_id')) |> 
    ggplot(aes(x, y, color = container.title)) +
    geom_point(alpha = .1) +
    facet_wrap(vars(k, vocabulary),
               ncol = 3,
               scales = 'free', 
               labeller = label_wrap_gen(multi_line = FALSE)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_bw() +
    theme(legend.position = 'bottom')
ggsave('test.png', 
       width = 3*4, 
       height = 6*4 + 2)

