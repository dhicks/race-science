library(tidyverse)
library(geomtextpath)
library(palmerpenguins)

data('penguins')

kde2d_pipe = function(dataf, var1, var2, ...) {
    MASS::kde2d(dataf[[var1]], 
                dataf[[var2]], 
                ...)
}

calc_densities = function(dataf, var1, var2, group_var, group_values) {
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
            n = 50)
    
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


densities <- calc_densities(penguins, 
                            bill_length_mm, 
                            flipper_length_mm, 
                            species, c('Adelie', 'Gentoo')) |> 
    group_by(species) |>
    mutate(z = z / max(z)) |>
    ungroup()

ggplot(penguins, aes(bill_length_mm, 
                     flipper_length_mm, 
                     color = species)) +
    geom_point(alpha = 0.5) +
    geom_textcontour(aes(label = species, z = z), 
                     breaks = .15, 
                     data = densities)

