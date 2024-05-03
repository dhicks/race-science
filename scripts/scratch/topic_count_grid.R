library(tidyverse)
library(tidytext)
theme_set(theme_bw())

library(tmfast)
library(here)
library(arrow)
# remotes::install_github("mtennekes/cols4all")
# library(cols4all)
library(patchwork)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

meta_ar = open_dataset(here(data_dir, '01_metadata'))

source(here('R', 'phrases.R'))
phrases_ar = phrases() |> 
    mutate(n = log1p(n))

articles_df = phrases_ar |>
    group_by(article_id) |>
    summarize(total_phrases = sum(n)) |>
    inner_join(meta_ar, by = 'article_id') |>
    # filter(container.title != 'Psychological Reports') |> 
    collect()

md_tmf = readRDS(here(tm_dir, '04_md_tmfast.Rds'))
md_exp = read_rds(here(data_dir, '05_md_exponents.Rds')) |> 
    pull(exponent, name = k)

topics = tidy_all(md_tmf) |> 
    group_by(k, topic) |> 
    top_n(10, beta) |> 
    arrange(k, topic, beta) |> 
    summarize(race = any(str_detect(token, 'race|whites|blacks')), 
              intelligence = any(str_detect(token, 'intelligence|iq') & 
                                     token != 'emotional_intelligence')) |> 
    ungroup() |> 
    filter(race|intelligence) |> 
    mutate(type = case_when(race & !intelligence ~ 'race', 
                            !race & intelligence ~ 'intelligence',
                            race & intelligence ~ 'both', 
                            TRUE ~ NA_character_),
           type = fct_relevel(type, 'race', 'both', 'intelligence'))

# create_dataframes(md_tmf, md_exp, topics = topics$race[1])
# count_plot(md_tmf, vocab = 'md', md_exp, topics$k[5], unlist(topics$race[5]), .5)

# count_plot(md_tmf, vocab = 'md', md_exp, topics$k[1], unlist(topics$intelligence[1]), .5)

foo = topics |> 
    # slice(1:6) |> 
    rowwise() |> 
    mutate(dataf = list(count_plot(md_tmf, 'md', md_exp, 
                                   k, topic, 
                                   threshold = .5,
                                   plot = FALSE))) |> 
    ungroup() |> 
    hoist(.col = 'dataf', 'container.title', 'year', 'gamma', 'gamma_sm') |> 
    select(-dataf) |> 
    unnest_longer(c(container.title, year, gamma, gamma_sm))

## facet_grid2 approach
foo |> 
    filter(k > 5) |> 
    ggplot(aes(year, 
               color = container.title,
               group = container.title)) +
    ggh4x::facet_grid2(rows = vars(k),
                       cols = vars(type, topic),
                       scales = 'free', independent = 'all',
                       switch = 'y',
                       strip = ggh4x::strip_nested(by_layer_x = TRUE, 
                                                   background_x = list(element_rect(linewidth = 1), NULL), 
                                                   background_y = list(element_rect(linewidth = 1))),
                       drop = TRUE) +
    geom_line(aes(y = gamma_sm), linewidth = 1.25, color = 'black') +
    geom_line(aes(y = gamma), alpha = .5) +
    geom_line(aes(y = gamma_sm), linewidth = 1) +
    coord_cartesian(ylim = c(NA, NA)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3), 
                       name = 'article count') +
    scale_color_viridis_d(option = 'D',
                          name = '') +
    theme_minimal() +
    theme(legend.position = 'bottom')

ggsave('md_topic_grid.pdf', height = 8, width = 12, scale = 1.5)

# foo = topics |> 
#     rowwise() |> 
#     mutate(plot = list(count_plot(md_tmf, 'md', md_exp, 
#                                   k, topic, 
#                                   threshold = .5, 
#                                   annotate = FALSE))) |> 
#     ungroup()

layout = topics |> 
    filter(k > 5) |> 
    arrange(type, k, topic) |> 
    mutate(typetopic = str_c(type, topic), 
           typetopic = fct_inorder(typetopic)) |> 
    arrange(k) |> 
    mutate(plot_id = LETTERS[1:n()]) |> 
    pivot_wider(id_cols = 'k', 
                names_from = 'typetopic', 
                names_sort = TRUE,
                values_from = 'plot_id', 
                values_fill = '#') |> 
    rowwise() |> 
    mutate(row = str_flatten(c_across(-k))) |> 
    pull(row) |> 
    str_flatten(collapse = '\n')

foo |> 
    filter(k > 5) |> 
    ggplot(aes(year, gamma_sm,
           color = container.title,
           group = container.title)) +
    geom_line() +
    ggh4x::facet_manual(
        facets = vars(k, type, topic),
        design = layout,
        scales = 'free_y', 
        # labeller = label_wrap_gen(multi_line=FALSE),
        strip = ggh4x::strip_split(c('left', 'top', 'top'))) +
    geom_line(aes(y = gamma_sm), size = 1.25, color = 'black') +
    geom_line(aes(y = gamma), alpha = .5) +
    geom_line(aes(y = gamma_sm), size = 1) +
    coord_cartesian(ylim = c(NA, NA)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_color_viridis_d(option = 'D',
                          name = '') +
    theme(legend.position = 'bottom')

# layout = foo |> 
#     mutate(plot_id = LETTERS[1:n()]) |> 
#     pivot_wider(id_cols = 'k', 
#                 names_from = 'topic', 
#                 values_from = 'plot_id', 
#                 values_fill = '#') |> 
#     select(-k) |> 
#     rowwise() |> 
#     mutate(row = str_flatten(c_across(everything()))) |> 
#     pull(row) |> 
#     str_c(collapse = '\n')
# 
# wrap_plots(foo$plot, 
#            design = layout, 
#            guides = 'collect') +
#     plot_annotation(theme = list(legend.position = 'bottom'))


