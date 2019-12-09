library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)

fighter_list <- c("Mike" = "1234",
                  "Jack" = "5678",
                  "Frank" = "9012");


shinyUI(
  fluidPage(
    #shinythemes::themeSelector(),
    theme = shinytheme("slate"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    mainPanel(
      width=12,
      #tabsetPanel(
        #tabPanel("Tab 1"),
        #tabPanel("Tab 2")
      #)
      h4("UFC Pre-match Analysis"),
      fluidRow(
        align = "center",
        column(3,
          h4('Fighter 1'),
          selectInput(
            "fighter1", "",
            fighter_list
            # selected=tableOutput("data")
          )
        ),
        column(6),
        column(3,
          h4('Fighter 2'),
          selectInput(
            "fighter2", "",
            fighter_list
            # selected=tableOutput("data")
          )
        ),
      ),
      fluidRow(
        align = "center",
        column(3, 
          style = "font-size: 16px; padding: 0px 0px; margin-top:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 5px;",
          h4('Fighter 1'),
          fluidRow(
            column(4,
              tags$img(src = "fighter/No-Photo-00001", width = "200px", style="max-width: 100%")
            ),
            column(8,
              h5("test")     
            )
          )
        ),
        column(6, 
          fluidRow(
            align = "center",
            style = "font-size: 16px; padding: 16px 16px; margin-top:1em; background-color: rgba(0, 0, 0, 0.5); border-radius: 5px;",
            h4('Head to Head'),
            fluidRow(
              align = "center",
              column(6,
                plotlyOutput("headtohead")
              )
            )
          )
        ),
        column(3, 
          style = "font-size: 16px; padding: 0px 0px; margin-top:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 5px;",
          h4('Fighter 2'),
          fluidRow(
            column(8
            ),
            column(4,
              tags$img(src = "fighter/No-Photo-00002", width = "200px", style="max-width: 100%")
            )
          )
        ),
      ),
    )
  )
)
