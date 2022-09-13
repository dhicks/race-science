library(tidyverse)
theme_set(theme_minimal())
library(tmfast)

library(tidygraph)
library(ggraph)

library(arrow)
library(here)
library(assertthat)
library(glue)

data_dir = here('data')
tm_dir = here(data_dir, '04_tm')
out_dir = here('out')

model_files = list.files(tm_dir, 'tmfast') |> 
    set_names('lg', 'md', 'sm')

# model = here(tm_dir, '04_md_tmfast.Rds') |>
#     read_rds()

compare_betas = function(beta1, beta2, vocab) {
    fill = function(beta) {
        beta |> 
            complete(token = vocab, topic, fill = list(beta = 0)) |> 
            build_matrix(topic, token, beta) %>%
            .[, vocab]
    }
    beta1 = fill(beta1)
    beta2 = fill(beta2)
    
    assert_that(all(colnames(beta1) == colnames(beta2)))
    
    hellinger(beta1, beta2)
}

# compare_betas(betas$`5`, betas$`10`)

hellinger_betas = function(model_file, name) {
    out_prefix = glue('06_{name}')
    model = here(tm_dir, model_file) |> 
        read_rds()
    
    ## Extract betas and calculate pairwise Hellinger distance/similarity
    betas = map(model$n, 
                ~ tidy(model, k = .x, matrix = 'beta', df = TRUE)) |> 
        set_names(model$n)
    
    vocab = model$cols
    
    compared = map(1:(length(betas) - 1), 
                   ~ compare_betas(betas[[.x]], betas[[.x + 1]], vocab))
    
    ## Tidy
    comp_df = compared |> 
        map(as.matrix) |>
        map(as_tibble, rownames = 'topic_a') |>
        map(pivot_longer, cols = starts_with('V'),
            names_to = 'topic_b',
            values_to = 'H') |>
        set_names(c('5-10', '10-20', '20-30', '30-40', '40-50')) |>
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
           height = 2*3, width = 3*4, bg = 'white')
    
    ## Dendrogram
    # comp_df |> 
    #     filter(similarity > .5) |> 
    #     ggplot() +
    #     geom_segment(aes(x = topic_a, xend = topic_b, 
    #                      y = model_a, yend = model_b, 
    #                      color = similarity), 
    #                  size = 1) +
    #     scale_color_viridis_c(limits = c(0.5, 1), 
    #                           direction = -1) +
    #     scale_y_reverse(breaks = c(5, 10, 20, 30, 40, 50), 
    #                     minor_breaks = NULL, 
    #                     expand = expansion()) +
    #     labs(x = 'topic', 
    #          y = 'model (k)')
    graph = comp_df |> 
        mutate(start_id = str_c(model_a, '-', topic_a), 
               end_id = str_c(model_b, '-', topic_b)) |> 
        select(start_id, end_id, everything()) |> 
        as_tbl_graph() |> 
        mutate(model = str_split(name, '-', simplify = TRUE)[,1], 
               topic = str_split(name, '-', simplify = TRUE)[,2], 
               across(c(model, topic), 
                      as.integer))
    
    graph |> 
        activate(edges) |> 
        filter(similarity > .5) |> 
        activate(nodes) |> 
        filter(!node_is_isolated()) |> 
        ggraph(layout = 'dendrogram') +
        geom_edge_link(aes(color = similarity), width = 1) +
        geom_node_label(aes(label = name), 
                        alpha = .5, 
                        nudge_y = -.15) +
        scale_x_reverse() +
        scale_y_reverse() +
        scale_edge_color_viridis(limits = c(0.5, 1),
                                 direction = -1) +
        coord_flip() +
        theme(legend.position = c(.2, .2))
    ggsave(here(out_dir, glue(out_prefix, '_cladogram.png')), 
           height = 6, width = 6, bg = 'white', scale = 1.5)
}

imap(model_files, hellinger_betas)
