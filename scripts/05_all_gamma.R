renv::load(here::here())
library(tidyverse)
theme_set(theme_minimal())
library(tmfast)
library(magrittr)
library(furrr)
plan(multisession, workers = availableCores() - 2)
warning('This script uses multisession parallel processing.')

library(arrow)
library(here)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

meta_df = open_dataset(here(data_dir, '01_metadata')) |> 
    collect()

model_files = list.files(tm_dir, 'tmfast') %>%
    here(tm_dir, .) |> 
    set_names(c('lg', 'md', 'sm'))

all_gammas = function(path, name) {
    message(glue('Processing topic model {name}'))
    out_prefix = glue('05_{name}')
    model = read_rds(path)
    
    ## Screeplot
    model$sdev |> 
        as_tibble_col(column_name = 'sdev') |> 
        mutate(pc = row_number(), 
               var = sdev^2) |> 
        ggplot(aes(pc, var)) +
        geom_point()
    ggsave(here(out_dir, glue(out_prefix, '_screeplot.png')),
        width = 4, height = 3, scale = 1.5, bg = 'white')
    
    ## Cumulative variance explained
    model$sdev |>
        as_tibble_col(column_name = 'sdev') |>
        mutate(pc = row_number(),
               var = sdev^2,
               var_expl = var / model$totalvar,
               cum_var_expl = cumsum(var_expl)) |>
        ggplot(aes(pc, cum_var_expl)) +
        geom_point()
    ggsave(here(out_dir, glue(out_prefix, '_cumvar.png')),
           width = 4, height = 3, scale = 1.5, bg = 'white')
    
    ## Renormalization
    message('Calculating renormalization exponents')
    get_power = function(this_k, model) {
        ee = expected_entropy(peak_alpha(this_k, 1, peak = .8, scale = 1))
        model |> 
            tidy(this_k, 'gamma') |> 
            target_power(document, gamma, ee)
    }
    all_k = model$n
    exponents = future_map_dbl(all_k, get_power, model, 
                               .progress = TRUE)
    tibble(k = all_k, 
           exponent = exponents) |> 
        write_rds(here(data_dir, glue(out_prefix, '_exponents.Rds')))
    
    message('Applying renormalization exponents')
    gamma = tidy_all(model, matrix = 'gamma') |> 
        group_split(k) |> 
        map2_dfr(exponents, ~ renorm(.x, document, gamma, .y))

    ## Combine w/ metadata to order by year
    message('Joining metadata with topic-doc distributions')
    comb_df = right_join(meta_df, gamma, 
                         by = c('article_id' = 'document'), 
                         multiple = 'all') |> 
        mutate(article_id = fct_reorder(article_id, 
                                        year, 
                                        .fun = min))
    
    ## topic-doc entropies
    message('Calculating topic-doc entropies')
    comb_df |> 
        group_by(k, container.title, article_id) |> 
        summarize(H = sum(-gamma * log2(gamma + 1e-15))) |> 
        ggplot(aes(k, H, color = container.title, 
                   group = container.title)) +
        stat_summary(fun.min = min, 
                     fun.max = max, 
                     fun = median,
                     position = position_dodge(width = 3)) +
        stat_summary(fun = median, geom = 'line', 
                     position = position_dodge(width = 3)) +
        stat_function(fun = log2, inherit.aes = FALSE, color = 'black') +
        theme(legend.position = 'bottom')
    ggsave(here(out_dir, glue(out_prefix, '_entropy.png')), 
           width = 6, height = 3, scale = 1.5, bg = 'white')
    
    ## tile visualization of topic-docs
    message('Building topic-doc tile visualizations')
    ggplot(comb_df, aes(topic, article_id, fill = log1p(gamma))) +
        geom_raster() +
        geom_vline(xintercept = c(10, 20, 30, 40, 50, 60) + .5, 
                   color = 'black') +
        facet_grid(rows = vars(container.title), 
                   cols = vars(k), 
                   scales = 'free', 
                   labeller = label_wrap_gen(width = 15), 
                   switch = 'y') +
        scale_x_discrete(breaks = c('V10', 'V20', 'V30',
                                    'V40', 'V50', 'V60', 'V70')) +
        scale_y_discrete(breaks = NULL, 
                         name = '') +
        scale_fill_viridis_c(option = 'viridis', direction = 1) +
        theme_minimal() +
        theme(axis.ticks.x = element_line(), 
              legend.position = 'bottom', 
              plot.margin = margin(r = 10))
    ggsave(here(out_dir, glue(out_prefix, '_gamma.png')), 
           width = 8, height = 6.5, scale = 1.5, bg = 'white')
}

iwalk(model_files, all_gammas)
