library(shiny)
library(dplyr)
library(plotly)
library(stringr)
library(caret)

#setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters <- read.csv("data/fighter_data.csv", header = T)

write_log <- function(content) {
  cat(paste0(content, "\n"), file = "log.txt")
}

pound_to_kg <- function(weight) {
  return (weight*0.45359237)
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
    mutate(
      years_till_now = as.numeric(difftime(today, as.Date(date), unit="days"))/365,
      current_age = round(age + years_till_now),
      Weight_kgs = round(pound_to_kg(Weight_lbs), 2),
      Height_cms = round(Height_cms, 2),
      Reach_cms = round(Reach_cms, 2)
    ) %>%
    select(fighter, current_age, Height_cms, Weight_kgs, Reach_cms, Stance) %>%
    rename(
      "Name" = fighter, "Age" = current_age, 
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

plot_failure <- function(message) {
  return(plotly_empty() %>% 
           config(displayModeBar = FALSE) %>%
           layout(
             height = 300, 
             paper_bgcolor = 'rgba(248, 248, 255, 0)', plot_bgcolor = 'rgba(248, 248, 255, 0)',
             margin = list(l = 10, r = 10, t = 10, b = 10)
           ) %>% 
           add_annotations(
             text = message,
             xanchor = "center",
             yanchor = "top", 
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             font = list(family = 'Arial', size = 12,
                         color = 'rgb(224, 224, 224)')
           ) 
  )
}

plot_figure <- function(data) {
  p <- plot_ly(
    data, x = ~x, y = ~y, type = 'scatter', mode = 'markers', width = 150, height = 225, hoverinfo = "none", 
    marker = list(size = ~attack_count, opacity = ~attack_opacity, color = ~attack_color, line = list(color = 'rgba(225, 225, 225, 0)', width = 0)) 
  ) %>%
    config(displayModeBar = F) %>%
    layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE,
        range = c(0.5, 2.5)),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE,
        range = c(0, 3.2)),
      images = list(
        list(
          source =  paste('data:image/png;base64', txt, sep=','),
          xref = "x",
          yref = "y",
          x = 1,
          y = 3,
          sizex = 1,
          sizey = 3,
          sizing = "stretch",
          opacity = 0.4,
          layer = "below"
        )
      ),
      margin = list(l = 0, t = 0, r = 0, b = 0, pad = 0),
      paper_bgcolor = 'rgba(248, 248, 255, 0)', plot_bgcolor = 'rgba(248, 248, 255, 0)'
    )
  
  return(p)
}

get_general_stat <- function(fighter_name_1, fighter_name_2) {
  
  if(is.na(fighter_name_1) || trimws(fighter_name_1) == "" || is.na(fighter_name_2) || trimws(fighter_name_2) == "" ) {
    return(plot_failure("Please Select Fighter 1 and Fighter 2"))
  } else if (fighter_name_1 == fighter_name_2) {
    return(plot_failure("Please Select a different fighter"))
  }
  
  # fighter_general_stat_1 <- fighters %>% 
  #   filter(fighter==fighter_name_1) %>% 
  #   select(avg_BODY_att, avg_BODY_landed, avg_HEAD_att, avg_HEAD_landed, avg_LEG_att, avg_LEG_landed) %>%
  #   t %>% as.vector
  # 
  # fighter_general_stat_2 <- fighters %>% 
  #   filter(fighter==fighter_name_1) %>% 
  #   select(avg_BODY_att, avg_BODY_landed, avg_HEAD_att, avg_HEAD_landed, avg_LEG_att, avg_LEG_landed) %>%
  #   t %>% as.vector
  # 
  return(
    plot_failure("Work in progress")
  )
}

get_figure <- function(fighter_name) {
  
  x <- c(1.5, 1.5, 1.5, 1.5, 1.6, 1.6)
  y <- c(2.7, 2.7, 2, 2, 1.2, 1.2)
  attack_color <- c('rgb(250,190,192)', 'rgb(113,0,25)', 'rgb(250,190,192)', 'rgb(113,0,25)', 'rgb(250,190,192)', 'rgb(113,0,25)')
  attack_opacity <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    attack_count =c(0,0,0,0,0,0)
    fighter_data <- data.frame(x, y, attack_count, attack_color, attack_opacity)
    return(plot_figure(fighter_data))
  }
  
  attack_count <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    select(avg_BODY_att, avg_BODY_landed, avg_HEAD_att, avg_HEAD_landed, avg_LEG_att, avg_LEG_landed) %>% t %>% as.vector
  
  fighter_data <- data.frame(x, y, attack_count, attack_color, attack_opacity)
  
  return(
    plot_figure(fighter_data)
  )
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
      mutate(gap_closure = ground_def) %>% t %>% as.vector
  } else if(fighter_seq == 2) {
    fighter_in_game_stat <- fighters %>% 
      filter(fighter==fighter_name) %>% 
      select(ground_att, distance_att, close_att, close_def, distance_def, ground_def) %>%
      mutate(gap_closure = ground_att) %>% t %>% as.vector
  }
  
  # for debug purpose
  #write.csv(fighter_in_game_stat, paste0("data/fighter_data.",fighter_seq,".csv"), row.names = FALSE)
  #write_log(class(fighter_in_game_stat))
  
  return(fighter_in_game_stat)
}

prediction <- function(fighter_name_1, fighter_name_2) {
  
  if(is.na(fighter_name_1) || trimws(fighter_name_1) == "" || is.na(fighter_name_2) || trimws(fighter_name_2) == "" ) {
    return(plot_failure("Please Select Fighter 1 and Fighter 2"))
  } else if (fighter_name_1 == fighter_name_2) {
    return(plot_failure("Please Select a different fighter"))
  }
  
  #Load Model
  load("fighter_model.RData")
  
  #Red Corner
  red <- fighters %>% filter(fighter==fighter_name_1)

  #Blue Corner
  blue <- fighters %>% filter(fighter==fighter_name_2)

  #Data needed for prediction
  fighter_choosen_data=data.frame(
    B_longest_win_streak        = blue$longest_win_streak,     
    B_total_rounds_fought       = blue$total_rounds_fought,  
    B_total_time_fought.seconds.= blue$total_time_fought.seconds.,
    B_total_title_bouts         = blue$total_title_bouts, 
    B_win_by_Decision_Majority  = blue$win_by_Decision_Majority,
    B_win_by_Decision_Split     = blue$win_by_Decision_Split,    
    B_win_by_Decision_Unanimous = blue$win_by_Decision_Unanimous,
    B_win_by_KO.TKO             = blue$win_by_KO.TKO,       
    B_win_by_Submission         = blue$win_by_Submission,        
    B_win_by_TKO_Doctor_Stoppage= blue$win_by_TKO_Doctor_Stoppage,
    B_Height_cms                = blue$Height_cms,              
    B_Reach_cms                 = blue$Reach_cms,                
    R_longest_win_streak        = red$longest_win_streak,        
    R_total_rounds_fought       = red$total_rounds_fought,       
    R_total_time_fought.seconds.= red$total_time_fought.seconds.,
    R_total_title_bouts         = red$total_title_bouts,       
    R_win_by_Decision_Majority  = red$win_by_Decision_Majority,
    R_win_by_Decision_Split     = red$win_by_Decision_Split,    
    R_win_by_Decision_Unanimous = red$win_by_Decision_Unanimous,
    R_win_by_KO.TKO             = red$win_by_KO.TKO,     
    R_win_by_Submission         = red$win_by_Submission,         
    R_win_by_TKO_Doctor_Stoppage= red$win_by_TKO_Doctor_Stoppage,
    R_Height_cms                = red$Height_cms,            
    R_Reach_cms                 = red$Reach_cms,              
    B_age                       = blue$age,         
    R_age                       = red$age,         
    B_Stance                    = case_when(blue$Stance=="Open Stance"~ 1 ,
                                            blue$Stance=="Orthodox"~ 2 ,
                                            blue$Stance=="Sideways"~ 3 ,
                                            blue$Stance=="Southpaw"~ 4 ,
                                            blue$Stance=="Switch"~ 5,
                                            TRUE~0),
    R_Stance                    = case_when(red$Stance=="Open Stance"~ 1 ,
                                            red$Stance=="Orthodox"~ 2 ,
                                            red$Stance=="Sideways"~ 3 ,
                                            red$Stance=="Southpaw"~ 4 ,
                                            red$Stance=="Switch"~ 5,
                                            TRUE~0),          
    diff_distance_att           =blue$DistanceAtt - red$DistanceAtt,
    diff_close_att              =blue$CloseAtt - red$CloseAtt,
    diff_ground_att             =blue$GroundAtt - red$GroundAtt,
    diff_distance_def           =blue$DistanceDef - red$DistanceDef,
    diff_close_def              =blue$CloseDef - red$CloseDef,
    diff_ground_def             =blue$GroundDef- red$GroundDef,
    R_total_normal_att          =red$total_normal_att,
    B_total_normal_att          =blue$total_normal_att,
    diff_head_att_pct           =blue$HeadAttactPct - red$HeadAttactPct,
    diff_head_att               =blue$HeadAttactAccuracy - red$HeadAttactAccuracy,
    diff_body_att_pct           =blue$BodyAttactPct - red$BodyAttactPct,
    diff_body_att               =blue$BodyAttactAccuracy-red$BodyAttactAccuracy,
    diff_leg_att_pct            =blue$LegAttactPct - red$LegAttactPct,
    diff_leg_att                =blue$LegAttactAccuracy - red$LegAttactAccuracy,
    B_current_streak            =ifelse(blue$current_lose_streak==0,blue$current_win_streak,-blue$current_lose_streak),       
    R_current_streak            =ifelse(red$current_lose_streak==0,red$current_win_streak,-red$current_lose_streak),                 
    B_win_pct                   =(blue$draw/2+blue$wins)/(blue$draw+blue$wins+blue$losses),          
    R_win_pct                   =(red$draw/2+red$wins)/(red$draw+red$wins+red$losses),                       
    diff_KD                     =blue$avg_KD - red$avg_KD,               
    diff_PASS                   =blue$avg_PASS - red$avg_PASS,
    diff_REV                    =blue$avg_REV - red$avg_REV,
    diff_SIG_STR_pct            =blue$avg_SIG_STR_pct - red$avg_SIG_STR_pct,           
    diff_SUB_att                =blue$avg_SUB_ATT - red$avg_SUB_ATT,
    diff_TD_pct                 =blue$avg_TD_pct - red$avg_TD_pct,          
    diff_KD_def                 =blue$avg_opp_KD - red$avg_opp_KD,         
    diff_PASS_def               =blue$avg_opp_PASS - red$avg_opp_PASS,            
    diff_REV_def                =blue$avg_opp_REV - red$avg_opp_REV,          
    diff_SIG_STR_pct_def        =blue$avg_opp_SIG_STR_pct - red$avg_opp_SIG_STR_pct,     
    diff_SUB_def                =blue$avg_opp_SUB_ATT - red$avg_opp_SUB_ATT,      
    diff_TD_pct_def             =blue$avg_opp_TD_pct - red$avg_opp_TD_pct
  )
  
  results <- predict(fit.lda,fighter_choosen_data,type="prob")
  
  y <- c('')
  # x1 <- c(25 + 50 * results$Red)
  # x2 <- c(25 + 50 * results$Blue)
  x1 <- c(round(100*results$Red))
  x2 <- c(round(100*results$Blue))
  
  data <- data.frame(y, x1, x2)
  label_pos <- c(x1/2, x1+x2/2)
  top_labels <- c('Fighter 1 Win', 'Fighter 2 Win')
  
  chart <- plot_ly(
    data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
    marker = list(color = 'rgba(239, 83, 80, 0.8)',
                  line = list(color = 'rgba(0, 0, 0, 5)', width = 1))
  ) %>%
    add_trace(x = ~x2, marker = list(color = 'rgba(66, 165, 245, 0.8)')) %>%
    config(displayModeBar = F) %>%
    layout(
      dragmode = FALSE,
      xaxis = list(
        title = "",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        zeroline = FALSE,
        domain = c(0.15, 1)),
      yaxis = list(title = "",
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = FALSE,
                   zeroline = FALSE),
      barmode = 'stack',
      height = 300, 
      paper_bgcolor = 'rgba(248, 248, 255, 0)', plot_bgcolor = 'rgba(248, 248, 255, 0)',
      margin = list(l = 0, r = 0, t = 140, b = 80, pad = 0),
      showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(224, 224, 224)'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the percentages of each bar (x_axis)
    add_annotations(xref = 'x', yref = 'y',
                    x = x1 / 2, y = y,
                    text = paste(data[,"x1"], '%'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(224, 224, 224)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = x1 + x2 / 2, y = y,
                    text = paste(data[,"x2"], '%'),
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(224, 224, 224)'),
                    showarrow = FALSE) %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = label_pos,
                    y = 1.15,
                    text = top_labels,
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(224, 224, 224)'),
                    showarrow = FALSE)
  
  
  return(chart)
}

shinyServer(
  function(input, output) {
    
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
    
    output$general_stat <- renderPlotly(
      get_general_stat(input$fighter_1, input$fighter_2)
    )
    
    output$general_figure_1 <- renderPlotly(
      get_figure(input$fighter_1)
    )
    
    output$general_figure_2 <- renderPlotly(
      get_figure(input$fighter_2)
    )
    
    output$in_game_stat <- renderPlotly({
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
      config(displayModeBar = FALSE) %>%
      layout(
        plot_bgcolor = "rgba(0, 0, 0, 1)",
        paper_bgcolor = "rgba(255, 255, 255, 0)",
        dragmode = FALSE,
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
    
    output$prediction_results <- renderPlotly(
      prediction(input$fighter_1, input$fighter_2)
    )
  }
)