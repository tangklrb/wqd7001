library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)

#setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters = read.csv("data/fighter_data.csv", header = T)
fighter_list <- fighters %>% select(fighter) %>% mutate(value = fighter) %>% as.vector

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
        column(3, selectInput("fighter_1", "Fighter 1",c(Choose='', fighter_list), selectize=TRUE)),
        column(6),
        column(3, selectInput("fighter_2", "Fighter 2",c(Choose='', fighter_list), selectize=TRUE)),
      ),
      fluidRow(
        align = "center",
        style = "font-size: 16px; padding: 16px 16px; margin-top:1em; background-color: rgba(0, 0, 0, 0.25); border-radius: 16px;",
        column(3, 
          style = "font-size: 16px; padding: 4px 8px; margin-top:1em; background-color: rgba(255, 255, 255, 0.25); border-radius: 8px;",
          h4('Fighter 1'),
          fixedRow(
            column(6, 
              imageOutput('fighter_picture_1'), 
              style="width: 200px, max-width: 100%"
            ),
            column(6, 
              tableOutput('fighter_details_1')     
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
              tableOutput('fighter_details_2')
            ),
            column(6, 
              imageOutput('fighter_picture_2'), 
              style="width: 200px, max-width: 100%"
            )
          )
        ),
      ),
    )
  )
)
