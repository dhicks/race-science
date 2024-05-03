library(tidygraph)
library(ggraph)


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
