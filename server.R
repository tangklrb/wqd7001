library(shiny)
library(dplyr)
library(plotly)
library(stringr)
library(caret)
library(RCurl)

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
        value=c("-", "-", "-", "-", "-", "-", "-"), 
        row.names = c("Name", "Age", "Height (cm)", "Weight (kg)", "Reach (cm)", "Stance", "Title Bout"))
    )
  }
  
  profile <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    mutate(
      years_till_now = as.numeric(difftime(Sys.Date(), as.Date(date), unit="days"))/365,
      current_age = round(age + years_till_now),
      Weight_kgs = round(pound_to_kg(Weight_lbs), 2),
      Height_cms = round(Height_cms, 2),
      Reach_cms = round(Reach_cms, 2)
    ) %>%
    select(fighter, current_age, Height_cms, Weight_kgs, Reach_cms, Stance, total_title_bouts) %>%
    rename(
      "Name" = fighter, "Age" = current_age, 
      "Height (cm)" = Height_cms, "Weight (kg)" = Weight_kgs, 
      "Reach (cm)" = Reach_cms, "Title Bout" = total_title_bouts,
    ) %>% t
  
  return(profile)
}

get_profile_picture <- function(fighter_name) {
  
  no_picture <- "http://www.pedal-up.com/wqd7001/fighter/No-Photo-0000"
  
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
    image_url <- paste0("http://www.pedal-up.com/wqd7001", profile_pic[1,1])
    if(!url.exists(image_url)) {
      return(no_picture)
    } else {
      return(image_url)  
    } 
  }
}

plot_failure <- function(message) {
  return(plotly_empty(height = 250) %>% 
           config(displayModeBar = FALSE) %>%
           layout(
             paper_bgcolor = 'rgba(0, 0, 0, 0.25)', plot_bgcolor = 'rgba(0, 0, 0, 0.25)',
             margin = list(l = 0, t = 0, r = 0, b = 0, pad = 0),
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

get_career_summary <- function(fighter_name) {
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    return(plot_failure("Fighter not selected"))
  }
  
  career_summary <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    select(wins, draw, losses) %>%
    rename(
      "Wins" = wins, "Draw" = draw, "Losses" = losses
    ) %>% t %>% as.data.frame %>% 
    add_rownames() %>% 
    rename (
      "count" = V1
    )
  
  colors <- c('rgb(131,208,112)', 'rgb(248,207,29)', 'rgb(227,99,66)')
  
  return(plot_ly(career_summary, labels = ~rowname, values = ~count, type = 'pie', height = 250,
                 textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'), 
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
           config(displayModeBar = F) %>%
           layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  paper_bgcolor = 'rgba(0, 0, 0, 0.25)', plot_bgcolor = 'rgba(0, 0, 0, 0.25)',
                  margin = list(l = 8, t = 0, r = 8, b = 0, pad = 8))
  )
}

get_streak_detail <- function(fighter_name) {
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    empty_matrix = matrix("", nrow = 2, ncol = 1)
    rownames(empty_matrix) <- c("Current Streak", "Longest Wins")
    return(empty_matrix)
  }
  
  streak_detail <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    mutate(
      current_streak = ifelse(current_lose_streak>0, paste0(current_lose_streak, " loses"), paste0(current_win_streak, " wins"))
    ) %>% 
    select(current_streak, longest_win_streak) %>%
    rename(
      "Current_streak" = current_streak, "Longest Win Streak" = longest_win_streak
    ) %>% t 
  
  return(streak_detail)
}

get_general_stat <- function(fighter_name_1, fighter_name_2) {
  
  if(is.na(fighter_name_1) || trimws(fighter_name_1) == "" || is.na(fighter_name_2) || trimws(fighter_name_2) == "" ) {
    return(plot_failure("Please Select Fighter 1 and Fighter 2"))
  } else if (fighter_name_1 == fighter_name_2) {
    return(plot_failure("Please Select a different fighter"))
  }
  
  fighter_winning_stat_1 <- fighters %>%
    filter(fighter==fighter_name_1) %>%
    mutate(
      win_by_KO = win_by_KO.TKO + win_by_TKO_Doctor_Stoppage,
      win_by_Decision = win_by_Decision_Majority + win_by_Decision_Split + win_by_Decision_Unanimous,
      win_by_SUB = win_by_Submission
    ) %>%
    select(win_by_Decision, win_by_KO, win_by_SUB) %>%
    t %>% as.vector
  
  fighter_winning_stat_2 <- fighters %>%
    filter(fighter==fighter_name_2) %>%
    mutate(
      win_by_KO = win_by_KO.TKO + win_by_TKO_Doctor_Stoppage,
      win_by_Decision = win_by_Decision_Majority + win_by_Decision_Split + win_by_Decision_Unanimous,
      win_by_SUB = win_by_Submission
    ) %>%
    select(win_by_Decision, win_by_KO, win_by_SUB) %>%
    t %>% as.vector
  
  winning_type <- c("By KO", "By Decision", "By Submission")
  data <- data.frame(winning_type, fighter_winning_stat_1, fighter_winning_stat_2)
  
  return(
    plot_ly(data, x = ~fighter_winning_stat_2, y = ~winning_type, height = 250,
            type = 'bar', orientation = 'h', name = 'Fighter 2',
            marker = list(color = 'rgba(66, 165, 245, 0.8)',
                          line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
      add_trace(x = ~fighter_winning_stat_1, name = 'Fighter 1',
                marker = list(color = 'rgba(239, 83, 80, 0.8)',
                              line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
      config(displayModeBar = F) %>%
      add_annotations(xref = 'x', yref = 'y', x = 'paper' , y = ~winning_type,
                      xanchor = 'right',
                      text = ~winning_type,
                      font = list(family = 'sans-serif', size = 10,
                                  color = 'rgb(255, 255, 255)'),
                      showarrow = FALSE, align = 'right') %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ~fighter_winning_stat_2, y = ~winning_type,
                      text = ~fighter_winning_stat_2,
                      xanchor = 'left',
                      yanchor='top',
                      font = list(family = 'sans-serif', size = 10,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      add_annotations(xref = 'x', yref = 'y',
                      x = ~fighter_winning_stat_1, y = ~winning_type,
                      text = ~fighter_winning_stat_1,
                      xanchor = 'left',
                      yanchor='bottom',
                      font = list(family = 'sans-serif', size = 10,
                                  color = 'rgb(248, 248, 255)'),
                      showarrow = FALSE) %>%
      layout(
        dragmode = FALSE,
        xaxis = list(
          title = "",
          showgrid = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          zeroline = TRUE,
          domain = c(0.15, 1)),
        legend = list(
          font = list(
            family = "sans-serif",
            size = 10,
            color = 'rgba(255, 255, 255, 0.75'),
          bgcolor = 'rgba(0, 0, 0, 0.5)',
          bordercolor = 'rgba(0, 0, 0, 0.625)',
          borderwidth = 1),
        barmode = 'group',
        paper_bgcolor = 'rgba(0, 0, 0, 0.25)', plot_bgcolor = 'rgba(0, 0, 0, 0.25)',
        margin = list(l = 0, t = 0, r = 0, b = 0, pad = 0),
        showlegend = TRUE
      )
  )
}

plot_figure <- function(data) {
  
  image_file <- "www/figure.png"
  txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  
  p <- plot_ly(
    data, x = ~x, y = ~y, type = 'scatter', mode = 'markers', width = 150, height = 225, hoverinfo = "none", 
    marker = list(size = ~attack_count, opacity = ~attack_opacity, color = ~attack_color, line = list(color = 'rgba(225, 225, 225, 0)', width = 0)) 
  ) %>%
    config(displayModeBar = F) %>%
    add_annotations(x = ~attack_note_x,
                    y = ~attack_note_y,
                    text = ~attack_note,
                    xref = "paper",
                    yref = "y",
                    font = list(family = 'Arial', size = 10,
                                color = 'rgb(0, 0, 0)'),
                    showarrow = FALSE) %>%
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
          opacity = 0.25,
          layer = "below"
        )
      ),
      margin = list(l = 0, t = 0, r = 0, b = 0, pad = 0),
      paper_bgcolor = 'rgba(248, 248, 248, 0.5)', plot_bgcolor = 'rgba(248, 248, 248, 0.5)'
    )
  
  return(p)
}

get_attack_area <- function(fighter_seq, fighter_name) {
  
  x <- c(1.5, 1.5, 1.5, 1.5, 1.6, 1.6)
  y <- c(2.7, 2.7, 2, 2, 1.2, 1.2)
  attack_note_x <- ifelse(fighter_seq==1, c(1, 1, 1), c(0, 0, 0))
  attack_note_y <- c(2.7, 2, 1.2)
  attack_color <- c('rgb(251,141,160)', 'rgb(251,69,112)', 'rgb(251,141,160)', 'rgb(251,69,112)', 'rgb(251,141,160)', 'rgb(251,69,112)')
  attack_opacity <- c(0.35, 0.5, 0.35, 0.5, 0.35, 0.5)
  
  if(is.na(fighter_name) || trimws(fighter_name) == "" ) {
    attack_note = c("", "", "", "", "", "")
    attack_count =c(0,0,0,0,0,0)
    fighter_data <- data.frame(x, y, attack_count, attack_color, attack_opacity, attack_note, attack_note_x, attack_note_y)
    return(plot_figure(fighter_data))
  }
  
  attack_count <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    select(avg_HEAD_att, avg_HEAD_landed, avg_BODY_att, avg_BODY_landed, avg_LEG_att, avg_LEG_landed) %>% t %>% as.vector
  
  attack_note <- fighters %>% 
    filter(fighter==fighter_name) %>% 
    mutate(
      head = paste0("(H) ", round(head_att_pct*100, 0), "% (", round(head_att_acc*100, 0), "%)"),
      body = paste0("(B) ", round(body_att_pct*100, 0), "% (", round(body_att_acc*100, 0), "%)"),
      leg = paste0("(L) ", round(leg_att_pct*100, 0), "% (", round(leg_att_acc*100, 0), "%)")
    ) %>%
    select(head, body, leg) %>% t %>% as.vector
  
  fighter_data <- data.frame(x, y, attack_count, attack_color, attack_opacity, attack_note, attack_note_x, attack_note_y)
  
  return(
    plot_figure(fighter_data)
  )
}

get_attack_summary <- function(fighter_name_1, fighter_name_2) {
  
  if(is.na(fighter_name_1) || trimws(fighter_name_1) == "" || is.na(fighter_name_2) || trimws(fighter_name_2) == "" ) {
    return(matrix(c("Please Select Fighter 1 and Fighter 2"), dimnames = list(c("Message:"), c("Error"))))
  } else if (fighter_name_1 == fighter_name_2) {
    return(matrix(c("Please Select a different fighter"), dimnames = list(c("Message:"), c("Error"))))
  }
  
  fighter_general_stat_1 <- fighters %>%
    filter(fighter==fighter_name_1) %>%
    mutate(
      avg_SIG_STR_landed = round(avg_SIG_STR_landed, 2),
      avg_SIG_STR_summary = paste0(avg_SIG_STR_landed, "/", avg_SIG_STR_att),
      avg_SIG_STR_pct = round(avg_SIG_STR_pct * 100, 2),
      avg_TD_landed = round(avg_TD_landed, 2),
      avg_TD_summary = paste0(avg_TD_landed, "/", avg_TD_att),
      avg_TD_pct = round(avg_TD_pct * 100, 2),
      avg_TOTAL_STR_landed = round(avg_TOTAL_STR_landed, 2),
      avg_TOTAL_STR_summary = paste0(avg_TOTAL_STR_landed, "/", avg_TOTAL_STR_att),
      avg_TOTAL_STR_pct = round(avg_TOTAL_STR_landed/avg_TOTAL_STR_att * 100, 2),
      avg_KD = round(avg_KD, 2),
      avg_PASS = round(avg_PASS, 2),
      avg_REV = round(avg_REV, 2),
      avg_SUB_ATT = round(avg_SUB_ATT, 2)
    ) %>%
    select(
      avg_SIG_STR_landed, avg_TD_landed, avg_TOTAL_STR_landed,
      # avg_SIG_STR_summary, avg_TD_summary, avg_TOTAL_STR_summary, 
      # avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, 
      avg_KD, avg_PASS, avg_REV, avg_SUB_ATT
    ) %>%
    #select(avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
    rename(
      "Avg Significant Strike" = avg_SIG_STR_landed, "Avg Total Strike" = avg_TOTAL_STR_landed, "Avg Take Down" = avg_TD_landed, 
      # "Significant Strike" = avg_SIG_STR_summary, "Total Strike" = avg_TOTAL_STR_summary, "Take Down" = avg_TD_summary, 
      # "Significant Strike %" = avg_SIG_STR_pct, "Total Strike %" = avg_TOTAL_STR_pct, "Take Down %" = avg_TD_pct, 
      "Avg Knock Down" = avg_KD, "Avg Pass" = avg_PASS, "Avg Reversal" = avg_REV, "Avg Submission Attemp" = avg_SUB_ATT
    ) %>% t
  
  fighter_general_stat_2 <- fighters %>%
    filter(fighter==fighter_name_2) %>%
    mutate(
      avg_SIG_STR_landed = round(avg_SIG_STR_landed, 2),
      avg_SIG_STR_summary = paste0(avg_SIG_STR_landed, "/", avg_SIG_STR_att),
      avg_SIG_STR_pct = round(avg_SIG_STR_pct * 100, 2),
      avg_TD_landed = round(avg_TD_landed, 2),
      avg_TD_summary = paste0(avg_TD_landed, "/", avg_TD_att),
      avg_TD_pct = round(avg_TD_pct * 100, 2),
      avg_TOTAL_STR_landed = round(avg_TOTAL_STR_landed, 2),
      avg_TOTAL_STR_summary = paste0(avg_TOTAL_STR_landed, "/", avg_TOTAL_STR_att),
      avg_TOTAL_STR_pct = round(avg_TOTAL_STR_landed/avg_TOTAL_STR_att * 100, 2),
      avg_KD = round(avg_KD, 2),
      avg_PASS = round(avg_PASS, 2),
      avg_REV = round(avg_REV, 2),
      avg_SUB_ATT = round(avg_SUB_ATT, 2)
    ) %>%
    select(
      avg_SIG_STR_landed, avg_TD_landed, avg_TOTAL_STR_landed,
      # avg_SIG_STR_summary, avg_TD_summary, avg_TOTAL_STR_summary, 
      # avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, 
      avg_KD, avg_PASS, avg_REV, avg_SUB_ATT
    ) %>%
    #select(avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
    rename(
      "Avg Significant Strike" = avg_SIG_STR_landed, "Avg Total Strike" = avg_TOTAL_STR_landed, "Avg Take Down" = avg_TD_landed, 
      # "Significant Strike" = avg_SIG_STR_summary, "Total Strike" = avg_TOTAL_STR_summary, "Take Down" = avg_TD_summary, 
      # "Significant Strike %" = avg_SIG_STR_pct, "Total Strike %" = avg_TOTAL_STR_pct, "Take Down %" = avg_TD_pct, 
      "Avg Knock Down" = avg_KD, "Avg Pass" = avg_PASS, "Avg Reversal" = avg_REV, "Avg Submission Attemp" = avg_SUB_ATT
    ) %>% t
  
  fighter_general_stat <- cbind(fighter_general_stat_1, fighter_general_stat_2)
  colnames(fighter_general_stat) <- c("Fighter 1", "Fighter 2")
  
  return(fighter_general_stat)
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
  
  return(fighter_in_game_stat)
}

prediction <- function(fighter_name_1, fighter_name_2) {
  
  if(is.na(fighter_name_1) || trimws(fighter_name_1) == "" || is.na(fighter_name_2) || trimws(fighter_name_2) == "" ) {
    return(plot_failure("Please Select Fighter 1 and Fighter 2"))
  } else if (fighter_name_1 == fighter_name_2) {
    return(plot_failure("Please Select a different fighter"))
  }
  
  #Load Model
  load("Model2.RData")
  load("Model_Compare.RData")
  
  #Red Corner
  red <- fighters %>% filter(fighter==fighter_name_1)
  
  #Blue Corner
  blue <- fighters %>% filter(fighter==fighter_name_2)
  
  
  #Data needed for prediction
  fighter_choosen_data=data.frame(
    diff_distance_att           =blue$distance_att - red$distance_att,
    diff_close_att              =blue$close_att - red$close_att,
    diff_ground_att             =blue$ground_att - red$ground_att,
    diff_distance_def           =blue$distance_def - red$distance_def,
    diff_close_def              =blue$close_def - red$close_def,
    diff_ground_def             =blue$ground_def- red$ground_def,
    diff_head_att_pct           =blue$head_att_pct - red$head_att_pct,
    diff_head_att               =blue$head_att_acc - red$head_att_acc,
    diff_body_att_pct           =blue$body_att_pct - red$body_att_pct,
    diff_body_att               =blue$body_att_acc-red$body_att_acc,
    diff_leg_att_pct            =blue$leg_att_pct - red$leg_att_pct,
    diff_leg_att                =blue$leg_att_acc - red$leg_att_acc,
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
    data, x = ~x1, y = ~y, type = 'bar', orientation = 'h', height = 300, 
    marker = list(color = 'rgba(239, 83, 80, 0.8)',
                  line = list(color = 'rgba(0, 0, 0, 5)', width = 1))
  ) %>%
    add_trace(x = ~x2, marker = list(color = 'rgba(66, 165, 245, 0.8)',
                                     line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
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
    
    output$fighter_picture_1 <- renderText({c('<img src="', get_profile_picture(input$fighter_1),'">')})
    output$fighter_picture_2 <- renderText({c('<img src="', get_profile_picture(input$fighter_2),'">')})
    
    output$career_summary_1 <- renderPlotly(
      get_career_summary(input$fighter_1)
    )
    
    output$career_summary_2 <- renderPlotly(
      get_career_summary(input$fighter_2)
    )
    
    output$streak_detail_1 <- renderTable(
      get_streak_detail(input$fighter_1),
      width="97%",
      rownames = TRUE, 
      colnames = FALSE
    )
    
    output$streak_detail_2 <- renderTable(
      get_streak_detail(input$fighter_2),
      width="97%",
      rownames = TRUE, 
      colnames = FALSE
    )
    
    output$winning_stat <- renderPlotly(
      get_general_stat(input$fighter_1, input$fighter_2)
    )
    
    output$attack_area_1 <- renderPlotly(
      get_attack_area(1, input$fighter_1)
    )
    
    output$attack_area_2 <- renderPlotly(
      get_attack_area(2, input$fighter_2)
    )
    
    output$attack_summary <- renderTable(
      get_attack_summary(input$fighter_1, input$fighter_2),
      width="97%",
      rownames = TRUE, 
      colnames = TRUE
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