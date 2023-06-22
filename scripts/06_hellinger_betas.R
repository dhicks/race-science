renv::load(here::here())
library(tidyverse)
theme_set(theme_minimal())
library(tmfast)

library(arrow)
library(here)
library(assertthat)
library(glue)
library(furrr)
plan(multisession, workers = availableCores() - 2)

library(patchwork)
library(tidytext)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

source(here('R', 'compare_betas.R'))

model_files = list.files(tm_dir, 'tmfast') %>%
    here(tm_dir, .) |> 
    set_names('lg', 'md', 'sm')

hellinger_betas = function(model_file, name) {
    message(glue('{name} vocabulary'))
    out_prefix = glue('06_{name}')
    model = read_rds(model_file)
    
    ## Renormalization
    message('Calculating renormalization exponents')
    get_power = function(this_k, model) {
        ee = expected_entropy(.1, this_k)
        model |> 
            tidy(this_k, 'beta') |> 
            target_power(topic, beta, ee)
    }
    all_k = model$n
    exponents = future_map_dbl(all_k, get_power, model, 
                               .progress = TRUE)
    # return(exponents)
    
    message('Extracting betas for Silge plots')
    betas = tidy_all(model) |> 
        group_split(k) |> 
        map2(exponents, ~ renorm(.x, topic, beta, .y))
    
    ## Silge plots
    betas |> 
        list_rbind() |> 
        group_by(k, topic) |> 
        top_n(10, beta) |> 
        ungroup() |> 
        arrange(k, topic, desc(beta)) |> 
        mutate(token = reorder_within(token, 
                                      beta,
                                      list(k, topic))) |> 
        group_by(k) |> 
        group_map(~ {ggplot(.x, 
                            aes(token, beta)) +
                geom_point() +
                geom_linerange(aes(ymax = beta), ymin = 0) +
                scale_x_reordered() +
                scale_y_continuous(
                    breaks = scales::pretty_breaks(n = 3)) +
                coord_flip(ylim = c(0, NA)) +
                facet_wrap(vars(topic),
                           scales = 'free',
                           ncol = 5) +
                theme(plot.background = 
                          element_rect(colour = "black", 
                                       fill = NA, 
                                       linewidth = 1))}, 
                .keep = TRUE) |> 
        wrap_plots(design = 
              'ABCD
               #BCD
               ##CD
               ##CD
               ###D
               EFGH
               EFGH
               EFGH
               EFGH
               EFGH
               EFGH
               EFGH
               EFGH
               #FGH
               #FGH
               ##GH
               ##GH
               ###H
               ###H') +
        labs(caption = glue('{name} vocabulary'))
    ggsave(here(out_dir, glue('06-{name}-silge.pdf')), 
           width = 4*5, height = (30+70)/5, 
           limitsize = FALSE,
           scale = 3)
    
    ## Pairwise Hellinger distance/similarity
    message('Hellinger similarities')
    vocab = model$cols
    
    compared = map(1:(length(betas) - 1), 
                   ~ compare_betas(betas[[.x]], 
                                   betas[[.x + 1]], 
                                   vocab))
    
    ## Tidy
    comp_df = compared |> 
        map(as.matrix) |>
        map(as_tibble, rownames = 'topic_a') |>
        map(pivot_longer, cols = starts_with('V'),
            names_to = 'topic_b',
            values_to = 'H') |>
        set_names(
            glue('{model$n}-{lead(model$n)}')[1:(length(model$n)-1)]
        ) |>
        bind_rows(.id = 'comparison') |> 
        separate(comparison, 
                 into = c('model_a', 'model_b'), 
                 sep = '-', 
                 remove = FALSE) |> 
        mutate(across(starts_with('topic'), 
                      str_remove, 'V'), 
               across(model_a:topic_b, as.integer), 
               similarity = 1 - H, 
               comparison = fct_inorder(comparison))
    
    ## Heat map
    message('Constructing Hellinger visualizations')
    ggplot(comp_df, aes(topic_b, topic_a, fill = similarity)) +
        geom_raster() +
        facet_wrap(vars(comparison), 
                   scales = 'free') +
        scale_x_continuous(breaks = scales::pretty_breaks(),
                           name = '', 
                           expand = expansion()) +
        scale_y_continuous(breaks = scales::pretty_breaks(),
                           name = '', 
                           expand = expansion()) +
        scale_fill_viridis_c(limits = c(0, 1), 
                             option = 'magma', direction = -1) +
        theme(legend.direction = 'horizontal', 
              legend.position = c(.9, .2),
              legend.justification = c(1, 0))
    ggsave(here(out_dir, glue(out_prefix, '_heat_map.png')), 
           height = 3*3, width = 3*4, bg = 'white')
    
    ## "Mereogram"
    comp_df |> 
        filter(similarity > .25) |> 
        ggplot() +
        geom_segment(aes(x = topic_a, xend = topic_b, 
                         y = model_a, yend = model_b, 
                         color = similarity, 
                         alpha = similarity), 
                     linewidth = 1) +
        scale_color_viridis_c(limits = c(0.25, 1), 
                              direction = -1) +
        scale_y_reverse(breaks = model$n, 
                        minor_breaks = NULL, 
                        expand = expansion()) +
        labs(x = 'topic', 
             y = 'model (k)')
    ggsave(here(out_dir, glue(out_prefix, '_mereogram.png')), 
           height = 5, width = 4, bg = 'white', scale = 1.5)
}

# debug(hellinger_betas)
# hellinger_betas(model_files[3], 'test')
imap(model_files, hellinger_betas)
