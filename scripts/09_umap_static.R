renv::load(here::here())
library(tidyverse)
theme_set(theme_minimal())
library(geomtextpath)
library(tmfast)

library(arrow)
library(here)
library(glue)

out_dir = here('out')
data_dir = here('data')
tm_dir = here('data', '04_tm')

prefix = '09'

outliers = .01 ## Used to calculate window for visualization


source(here('R', 'author_data.R'))
meta_df = open_dataset(here('data', '01_metadata')) |> 
    left_join(author_data(), by = 'article_id') |> 
    filter(container.title != 'Psychological Reports') |> 
    collect() |>
    nest(authors = author)

## 2d KDE, wrapped in a pipe-friendly way
kde2d_pipe = function(dataf, var1, var2, ...) {
    MASS::kde2d(dataf[[var1]], 
                dataf[[var2]], 
                ...)
}

## Calculate 2d KDE and return a tidy dataframe
calc_densities = function(dataf, var1, var2, group_var, group_values, ...) {
    var1 = ensym(var1)
    var2 = ensym(var2)
    group_var = ensym(group_var)
    dens = dataf |> 
        filter(!is.na({{ var1 }}), 
               !is.na({{ var2 }}), 
               {{ group_var }} %in% group_values) |> 
        mutate({{ group_var }} := fct_drop({{ group_var }})) %>%
        split(.[[rlang::as_string(group_var)]]) |> 
        map(kde2d_pipe, 
            rlang::as_string(var1), 
            rlang::as_string(var2), 
            ...)
    
    tidy_density = function(d) {
        d$z |> 
            as_tibble(.name_repair = 'unique') |> 
            mutate(!!var1 := d$x) |> 
            pivot_longer(-{{ var1 }}, 
                         names_to = rlang::as_string(var2), 
                         values_to = 'z') |> 
            mutate(!!var2 := rep_along({{ var2 }}, d$y))
    }
    dens |> 
        map(tidy_density) |> 
        list_rbind(names_to = rlang::as_string(group_var))
}

## Static UMAP visualization with highlighted topics
visualize = function(vocab, k, highlight_topics, write = TRUE) {
    model = here(tm_dir, glue('04_{vocab}_tmfast.Rds')) |> 
        read_rds()
    
    exponents = here(data_dir, glue('05_{vocab}_exponents.Rds')) |> 
        read_rds() |> 
        pull(exponent, name = k)
    exponent = exponents[as.character(k)]
    
    
    gamma = tidy(model, k, matrix = 'gamma', exponent = exponent) |> 
        filter(document %in% meta_df$article_id)
    
    umap_file = here(data_dir, glue('07_{vocab}_{k}_umap.Rds'))
    umap_df = read_rds(umap_file)
    
    window = umap_df |> 
        reframe(across(c(x, y), 
                       ~ quantile(.x, 
                                  probs = c(outliers, 1-outliers)))) |> 
        mutate(across(c(x, y), 
                      ~ .x * 1.15))
    
    dataf = gamma |> 
        group_by(document) |> 
        top_n(1L, gamma) |> 
        left_join(meta_df, by = c('document' = 'article_id')) |> 
        left_join(umap_df, by = 'document')
    
    densities = calc_densities(dataf, x, y, 
                               topic, highlight_topics, 
                               n = 75) |> 
        group_by(topic) |>
        mutate(z = z / max(z)) |>
        ungroup()
    
    plot = ggplot(dataf, aes(x, y, color = topic)) +
        geom_point(alpha = .01) +
        geom_textcontour(aes(label = topic, z = z), linewidth = 1,
                         upright = TRUE, straight = TRUE,
                         breaks = .15,
                         data = densities) +
        scale_color_viridis_d(guide = 'none') +
        lims(x = window$x, 
             y = window$y) +
        labs(caption = glue('Vocabulary {vocab}, k = {k}'))
    
    if (write) {
        get('prefix', envir = .GlobalEnv)
        path = here(out_dir, glue('{prefix}_{vocab}_{k}_umap.png'))
        ggsave(path, plot = plot, 
               width = 4, height = 4, bg = 'white', scale = 1.2)
    }
    return(plot)
}

# debugonce(visualize)
visualize('md', 30, c('V05', 'V07', 'V19'))

topics_of_interest = tribble(
    ~ vocab, ~ k, ~ highlight_topics,
    'sm', 20, c('V04', 'V12', 'V20'),
    'sm', 30, c('V04', 'V12', 'V20'),
    'sm', 40, c('V04', 'V12', 'V20', 'V34'),
    'md', 30, c('V05', 'V07', 'V19'),
    'md', 40, c('V05', 'V07', 'V22', 'V24', 'V36'),
    'lg', 20, c('V05', 'V06'),
    'lg', 30, c('V06', 'V24', 'V27'),
    'lg', 40, c('V06', 'V24', 'V26')
)

pwalk(topics_of_interest, 
      quietly(visualize), 
      .progress = TRUE)



