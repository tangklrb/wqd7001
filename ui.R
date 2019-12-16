library(shiny)
library(shinythemes)
library(htmlwidgets)
library(plotly)

#setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters = read.csv("data/fighter_data.csv", header = T)
fighter_list <- fighters %>% arrange(desc(date)) %>% select(fighter) %>% mutate(value = fighter) %>% as.vector

shinyUI(
  fluidPage(
    theme = shinytheme("slate"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$head(tags$style(".container {font-size: 12px; width:100%; padding: 2px; background-color: rgba(255, 255, 255, 0.25); border-radius: 8px;}")),
      tags$head(tags$style("#fighter_picture_1 img,#fighter_picture_2 img {max-width:98%; width: 200px}"))
    ),
    mainPanel(
      width=12,
      h3("UFC Fight Analysis"),
      fluidRow(
        align = "center",
        column(3,
               class="col-xs-5 col-md-3 col-lg-3",
               selectInput("fighter_1", "Fighter 1",c(Choose='', fighter_list), selectize=TRUE)
        ),
        column(6,
               class="col-xs-2 col-md-6 col-lg-6"
        ),
        column(3, 
               class="col-xs-5 col-md-3 col-lg-3",
               selectInput("fighter_2", "Fighter 2",c(Choose='', fighter_list), selectize=TRUE)
        )
      ),
      fluidRow(
        align="center",
        column(3, 
               class="col-xs-12 col-md-3 col-lg-3",
               h4('Fighter 1'),
               fluidRow(
                 class="container",
                 column(12, 
                        class="col-xs-6 col-md-12 col-lg-6 col-xl-6",
                        style="padding: 4px",
                        htmlOutput('fighter_picture_1', width="100%", height="100%")
                 ),
                 column(12, 
                        class="col-xs-6 col-md-12 col-lg-6 col-xl-6",
                        style="padding: 4px",
                        h4('Profile'),
                        tableOutput('fighter_details_1')
                 )
               )
        ),
        column(3, 
               class="col-xs-12 col-md-3 col-lg-3 col-xl-3 col-sm-push-6 col-md-push-6 col-lg-push-6",
               h4('Fighter 2'),
               fluidRow(
                 class="container",
                 column(12, 
                        class="col-xs-6 col-md-12 col-lg-6 col-xl-6 col-lg-push-6",
                        style="padding: 4px",
                        htmlOutput('fighter_picture_2', width="100%", height="100%")
                 ),
                 column(12,
                        class="col-xs-6 col-md-12 col-lg-6 col-xl-6 col-lg-pull-6",
                        style="padding: 4px",
                        h4('Profile'),
                        tableOutput('fighter_details_2')
                 )
               )
        ),
        column(6, 
               class="col-xs-12 col-md-6 col-lg-6 col-xl-6 col-sm-pull-3 col-md-pull-3 col-lg-pull-3",
               h4('Head to Head Comparison'),
               tabsetPanel(
                 type="tab",
                 tabPanel(
                   "General Stat",
                   fixedRow(
                     column(12, 
                            class="container",
                            align = "center",
                            h5("General Statistic"),
                            fluidRow(
                              column(12,
                                     class="col-xs-12 col-md-12 col-lg-6 col-xl-6 col-lg-push-3 col-xl-push-3",
                                     align = "center",
                                     h6("Winning Method"),
                                     plotlyOutput("winning_stat", width = "95%", height = "95%")
                              ),
                              column(6,
                                     class="col-xs-6 col-md-6 col-lg-3 col-xl-3 col-lg-pull-6 col-xl-pull-6",
                                     align = "center",
                                     h6("Career Results"),
                                     plotlyOutput("career_summary_1", width = "93%", height = "93%"),
                                     tableOutput('streak_detail_1')
                              ),
                              column(6,
                                     class="col-xs-6 col-md-6 col-lg-3 col-xl-3",
                                     align = "center",
                                     h6("Career Results"),
                                     plotlyOutput("career_summary_2", width = "93%", height = "93%"),
                                     tableOutput('streak_detail_2')
                              )
                            )
                     )
                   )
                 ),
                 tabPanel(
                   "In Game Stat",
                   fixedRow(
                     column(12, 
                            class="container",
                            h5("In Game Statistic"),
                            fluidRow(
                              column(6,
                                     class="col-xs-6 col-md-6 col-lg-3 col-xl-3",
                                     align = "center",
                                     h6("Target % (Accuracy %)"),
                                     plotlyOutput("attack_area_1", width = "95%", height = "95%")
                              ),
                              column(6,
                                     class="col-xs-6 col-md-6 col-lg-3 col-xl-3 col-lg-push-6 col-xl-push-6",
                                     align = "center",
                                     h6("Target % (Accuracy %)"),
                                     plotlyOutput("attack_area_2", width = "95%", height = "95%")
                              ) ,
                              column(12,
                                     class="col-xs-12 col-md-12 col-lg-6 col-xl-6 col-lg-pull-3 col-xl-pull-3",
                                     align = "center",
                                     tableOutput('attack_summary')
                              )
                            )
                     )
                   )
                 ),
                 tabPanel(
                   "Def/Att Stat",
                   fixedRow(
                     column(12, 
                            class="container",
                            h5("Defence & Attack Rating"),
                            fluidRow(
                              column(12,
                                     align = "center",
                                     plotlyOutput("in_game_stat", width = "95%")
                              )
                            )
                     )
                   )
                 ),
                 tabPanel(
                   "Prediction",
                   fixedRow(
                     column(12, 
                            class="container",
                            align = "center",
                            h5("Winner Prediction"),
                            plotlyOutput("prediction_results", width = "95%")
                     )
                   )
                 )
               )
        )
      )
    )
  )
)  
  