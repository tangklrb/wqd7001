library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)

fighter_list <- c(
  "Mike" = "1234",
  "Jack" = "5678",
  "Frank" = "9012"
)

shinyUI(
  fluidPage(
    theme = shinytheme("slate"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    mainPanel(
      width=12,
      h3("UFC Pre-match Analysis"),
      fluidRow(
        align = "center",
        column(3, selectInput("fighter1", "Fighter 1",c(Choose='', fighter_list), selectize=TRUE)),
        column(6),
        column(3, selectInput("fighter2", "Fighter 2",c(Choose='', fighter_list), selectize=TRUE)),
      ),
      fluidRow(
        align = "center",
        style = "font-size: 16px; padding: 16px 16px; margin-top:1em; background-color: rgba(0, 0, 0, 0.25); border-radius: 16px;",
        column(3, 
          style = "font-size: 16px; padding: 4px 8px; margin-top:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 8px;",
          h4('Fighter 1'),
          fixedRow(
            column(6, tags$img(src = "fighter/No-Photo-00001", width = "200px", style="max-width: 100%")),
            column(6, 
              tableOutput('particular1')     
            )
          )
        ),
        column(6, 
          fixedRow(
            style = "font-size: 16px; padding: 4px 8px; margin:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 8px;",
            align = "center",
            h4('Head to Head'),
            plotlyOutput("headtohead", width = "95%", height = "95%")
          )
        ),
        column(3, 
          style = "font-size: 16px; padding: 4px 8px; margin-top:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 8px;",
          h4('Fighter 2'),
          fixedRow(
            column(6,
              tableOutput('particular2')
            ),
            column(6, tags$img(src = "fighter/No-Photo-00002", width = "200px", style="max-width: 100%"))
          )
        ),
      ),
    )
  )
)
