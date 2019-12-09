library(shiny)
library(shinythemes)
library(shinyWidgets)

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
        column(4,
          h4('Fighter 1'),
          selectInput(
            "fighter1", "Fighter Name:",
            fighter_list
            # selected=tableOutput("data")
          )
        ),
        column(4),
        column(4,
          h4('Fighter 2'),
          selectInput(
            "fighter2", "Fighter Name:",
            fighter_list
            # selected=tableOutput("data")
          )
        ),
      ),
      fluidRow(
        align = "center",
        column(4, 
          #tags$head(tags$style(HTML("font-size: 20px; padding: 0px 0px; margin-top:-2em; background-color: rgba(255, 255, 255, 0.5)"))),
          h4('Fighter 1'),
          fluidRow(
            column(8,
              tags$img(src = "fighter/No-Photo-00001", width = "100%")
            ),
            column(4,
              h5("test")     
            )
          )
        ),
        column(4, 
          h4('Head to Head')
        ),
        column(4, 
          h4('Fighter 2'),
          fluidRow(
            column(4
            ),
            column(8,
              tags$img(src = "fighter/No-Photo-00002", width = "100%")
            )
          )
        ),
      ),
    )
  )
)
