library(shiny)
library(dplyr)
library(plotly)

shinyServer(
  function(input, output) {
    output$headtohead <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
      add_trace(
        r = c(0.6, 0.75, 0.5, 0.6, 0.5, 0.35),
        theta = c('R Ground DEF\nB Ground ATK', 'R Distance DEF\nB Distance ATK', 'R Clinch DEF\nB Clinch ATK', 'R Clinch ATK\nB Clinch DEF', 'R Distance ATK\nB Distance DEF','R Ground ATK\nB Ground DEF'),
        name = 'Player A'
      ) %>%
      add_trace(
        r = c(0.45, 0.5, 0.8, 0.65, 0.35, 0.65),
        theta = c('R Ground DEF\nB Ground ATK', 'R Distance DEF\nB Distance ATK', 'R Clinch DEF\nB Clinch ATK', 'R Clinch ATK\nB Clinch DEF', 'R Distance ATK\nB Distance DEF','R Ground ATK\nB Ground DEF'),
        name = 'Player B'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            side="clockwise",
            visible = T,
            range = c(0,1)
          )
        )
      )
  })
  }
)