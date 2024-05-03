library(tidyverse)
library(plotly)
library(palmerpenguins)

data(penguins)

gg = ggplot(penguins, aes(bill_length_mm, bill_depth_mm)) +
    geom_point(aes(color = species)) +
    geom_point(aes(color = island))

p = ggplotly(gg)
plotly_json(p)

p |> 
    style(visible = FALSE, traces = 4:6) |> 
    layout(updatemenus = list(
        list(
            active = 0, 
            type = 'buttons',
            buttons = list(
                list(method = 'update', 
                     args = list(list('visible' =
                                          c(rep(TRUE, 3), 
                                            rep(FALSE, 3)))),
                     label = 'Species'), 
                list(method = 'update', 
                     args = list(list('visible' =
                                          c(rep(FALSE, 3), 
                                            rep(TRUE, 3)))),
                     label = 'Island')
            )
        )
    )
    ) |> 
    layout(legend = list(x = 1, 
                         xanchor = 'right', 
                         borderwidth = 1, 
                         bgcolor = rgb(0, 0, 0, alpha = .1)))

