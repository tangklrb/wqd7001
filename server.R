library(shiny)
library(dplyr)
library(plotly)

shinyServer(
  function(input, output) {
    output$headtohead <- renderPlotly({
      plot_ly(
        type = 'scatterpolargl',
        fill = 'toself',
        mode = 'lines'
      ) %>%
      config(
        displaylogo = F
      ) %>%
      add_trace(
        r = c(0.6, 0.75, 0.5, 0.6, 0.5, 0.35, 0.6),
        fillcolor = "rgba(255, 15, 0, 0.5)",
        line = list(color = 'rgba(230, 13, 0, 0.75)', width = 1),
        theta = c('Distance\nA Def\nvs\nB Atk', 'Clinch\nA Def vs B Atk', 'Clinch\nB Def vs A Atk', 'Distance\nB Def\nvs\nA Atk','Ground\nB Def vs A Atk', 'Ground\nA Def vs B Atk', 'Distance\nA Def\nvs\nB Atk'),
        name = 'Player A'
      ) %>%
      add_trace(
        r = c(0.45, 0.5, 0.8, 0.65, 0.35, 0.65, 0.45),
        fillcolor = "rgba(0, 15, 255, 0.5)",
        line = list(color = 'rgba(13, 0, 230, 0.75)', width = 1),
        theta = c('Distance\nA Def\nvs\nB Atk', 'Clinch\nA Def vs B Atk', 'Clinch\nB Def vs A Atk', 'Distance\nB Def\nvs\nA Atk','Ground\nB Def vs A Atk', 'Ground\nA Def vs B Atk', 'Distance\nA Def\nvs\nB Atk'),
        name = 'Player B'
      ) %>%
      layout(
        plot_bgcolor = "rgba(0, 0, 0, 1)",
        paper_bgcolor = "rgba(255, 255, 255, 0)",
        polar = list(
          angularaxis = list(
            visible = T,
            layer = 'below traces',
            tickwidth = 1,
            linewidth = 1,
            tickfont = list(
              color = "white"
            )
          ),
          radialaxis = list(
            visible = T,
            side="clockwise",
            range = c(0,1)
          )
        ),
        legend = list(
          font = list(
            color = "white"
          ),
          orientation = 'h',
          xanchor = "center",
          x = 0.5
        ),
        margin = list(l = 50, r = 50, b = 8, t = 8, pad = 4)
      )
    })
    
    output$particular1 <- renderTable(
      data.frame(value=c("Mike", "90kg", "180cm", "65cm"), row.names = c("Name", "Weight", "Height", "Reach")),
      rownames = TRUE, 
      colnames = FALSE
    )  
    
    output$particular2 <- renderTable(
      data.frame(value=c("Frank", "78kg", "176cm", "61cm"), row.names = c("Name", "Weight", "Height", "Reach")),
      rownames = TRUE, 
      colnames = FALSE
    )
  }
)