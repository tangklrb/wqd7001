library(shiny)
library(dplyr)
library(plotly)
library(stringr)

#setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters <- read.csv("data/fighter_data.csv", header = T)

write_log <- function(content) {
  cat(paste0(content, "\n"), file = "log.txt")
}

pound_to_kg <- function(weight) {
  return (round(weight*0.45359237, digits=2))
}

get_in_game_stat <- function(fighter_seq, fighter_name) {
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    return(c(0, 0, 0, 0, 0, 0, 0))
  }
  
  # fighter 1: [def] distance, close, [att], close, distance, ground, [def] ground
  # fighter 2: [att] distance, close, [def], close, distance, ground, [att] ground
  if(fighter_seq == 1) {
      fighter_in_game_stat <- fighters %>% 
      filter(fighter==fighter_name) %>% 
      select(ground_def, distance_def, close_def, close_att, distance_att, ground_att) %>%
      mutate(gap_closure = distance_def) %>% t %>% as.vector
  } else if(fighter_seq == 2) {
    fighter_in_game_stat <- fighters %>% 
      filter(fighter==fighter_name) %>% 
      select(ground_att, distance_att, close_att, close_def, distance_def, ground_def) %>%
      mutate(gap_closure = distance_att) %>% t %>% as.vector
  }
  
  # for debug purpose
  #write.csv(fighter_in_game_stat, paste0("data/fighter_data.",fighter_seq,".csv"), row.names = FALSE)
  #write_log(class(fighter_in_game_stat))
  
  return(fighter_in_game_stat)
}

get_profile <- function(fighter_name) {
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    return(
      data.frame(
        value=c("-", "-", "-", "-", "-", "-"), 
        row.names = c("Name", "Age", "Height (cm)", "Weight (kg)", "Reach (cm)", "Stance"))
    )
  }

  profile <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    mutate(Weight_kgs = pound_to_kg(Weight_lbs)) %>%
    select(fighter, age, Height_cms, Weight_kgs, Reach_cms, Stance) %>%
    rename(
      "Name" = fighter, "Age" = age, 
      "Height (cm)" = Height_cms, "Weight (kg)" = Weight_kgs, 
      "Reach (cm)" = Reach_cms
    ) %>% t
  
  return(profile)
}

get_profile_pic <- function(fighter_seq, fighter_name) {
  
  no_picture <- "fighter/No-Photo-0000"

    if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    return(no_picture)
  }
  
  profile_pic <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    select(url) %>% t
  
  if(length(profile_pic) == 0) {
    return(no_picture)
  }
  else {
    if(!file.exists(paste0("www/", profile_pic))) {
      return(no_picture)
    } else {
      return(profile_pic[1,1])  
    } 
  }
}

shinyServer(
  function(input, output) {
    # fighter_1 <- input$fighter_1
    # fighter_2 <- input$fighter_2
    
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
        r = get_in_game_stat(1, input$fighter_1),
        fillcolor = "rgba(255, 15, 0, 0.5)",
        line = list(color = 'rgba(230, 13, 0, 0.75)', width = 1),
        theta = c('Ground\nA Def vs B Att', 'Distance\nA Def\nvs\nB Att', 'Close\nA Def vs B Att', 'Close\nB Def vs A Att', 'Distance\nB Def\nvs\nA Att','Ground\nB Def vs A Att', 'Ground\nA Def vs B Att'),
        name = ifelse(is.na(input$fighter_1) || trimws(input$fighter_1) == "", "Fighter 1", input$fighter_1)
      ) %>%
      add_trace(
        r = get_in_game_stat(2, input$fighter_2),
        fillcolor = "rgba(0, 15, 255, 0.5)",
        line = list(color = 'rgba(13, 0, 230, 0.75)', width = 1),
        theta = c('Ground\nA Def vs B Att', 'Distance\nA Def\nvs\nB Att', 'Close\nA Def vs B Att', 'Close\nB Def vs A Att', 'Distance\nB Def\nvs\nA Att','Ground\nB Def vs A Att', 'Ground\nA Def vs B Att'),
        name = ifelse(is.na(input$fighter_2) || trimws(input$fighter_2) == "", "Fighter 2", input$fighter_2)
      ) %>%
      layout(
        plot_bgcolor = "rgba(0, 0, 0, 1)",
        paper_bgcolor = "rgba(255, 255, 255, 0)",
        polar = list(
          angularaxis = list(
            visible = T,
            tickwidth = 1,
            linewidth = 1,
            rotation=240,
            direction='clockwise',
            layer = 'below traces',
            tickfont = list(color = "white")
          ),
          radialaxis = list(
            visible = T,
            side="clockwise",
            range = c(0,1)
          )
        ),
        legend = list(
          x = 0.5,
          orientation = 'h',
          xanchor = "center",
          font = list(color = "white")
        ),
        margin = list(l = 50, r = 50, b = 8, t = 8, pad = 4)
      )
    })
    
    output$fighter_details_1 <- renderTable(
      get_profile(input$fighter_1),
      width="97%",
      rownames = TRUE, 
      colnames = FALSE
    )  
    
    output$fighter_details_2 <- renderTable(
      get_profile(input$fighter_2),
      width="97%",
      rownames = TRUE, 
      colnames = FALSE
    )
    
    output$fighter_picture_1 <- renderImage({
      list(src = file.path(getwd(), sprintf("www/%s", get_profile_pic(1, input$fighter_1))))
    }, deleteFile = FALSE)
    
    output$fighter_picture_2 <- renderImage({
      list(src = file.path(getwd(), sprintf("www/%s", get_profile_pic(2, input$fighter_2))))
    }, deleteFile = FALSE)
    
  }
)