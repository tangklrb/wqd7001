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
        column(4,
               h4('Head to Head')
        ),
        column(4,
               h4('Fighter 2'),
               selectInput(
                 "fighter2", "Fighter Name:",
                 fighter_list
                 # selected=tableOutput("data")
               )
        ),
      ),
    )
  )
)