library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
library(plotly)

# setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
# profiles <- read.csv('data/data.csv', header=TRUE, sep = ",")
# print(profiles)
# 

# 
# y <- c('The course was effectively<br>organized',
#        'The course developed my<br>abilities and skills for<br>the subject',
#        'The course developed my<br>ability to think critically about<br>the subject',
#        'I would recommend this<br>course to a friend')
# x1 <- c(21, 24, 27, 29)
# x2 <-c(30, 31, 26, 24)
# # x3 <- c(21, 19, 23, 15)
# # x4 <- c(16, 15, 11, 18)
# # x5 <- c(12, 11, 13, 14)
# 
# 
# y <- c('Winning Possibility')
# x1 <- c(25 + 50 * 0)
# x2 <- c(25 + 50 * 1)
# 
# data <- data.frame(y, x1, x2)
# label_pos <- c(x1/2, x1+x2/2)
# top_labels <- c('Fighter 1 Win', 'Fighter 2 Win')
# 
# # data <- data.frame(y, x1, x2, x3, x4, x5)
# 
# # top_labels <- c('Strongly<br>agree', 'Agree', 'Neutral', 'Disagree', 'Strongly<br>disagree')
# 
# p <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
#              marker = list(color = 'rgba(239, 83, 80, 0.8)',
#                            line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
#   add_trace(x = ~x2, marker = list(color = 'rgba(66, 165, 245, 0.8)')) %>%
#   layout(xaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE,
#                       domain = c(0.15, 1)),
#          yaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE),
#          barmode = 'stack',
#          width = '95%', height = 300, 
#          paper_bgcolor = 'rgba(248, 248, 255, 0)', plot_bgcolor = 'rgba(248, 248, 255, 0)',
#          margin = list(l = 120, r = 10, t = 140, b = 80),
#          showlegend = FALSE) %>%
#   # labeling the y-axis
#   add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
#                   xanchor = 'right',
#                   text = y,
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE, align = 'right') %>%
#   # labeling the percentages of each bar (x_axis)
#   add_annotations(xref = 'x', yref = 'y',
#                   x = x1 / 2, y = y,
#                   text = paste(data[,"x1"], '%'),
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(248, 248, 255)'),
#                   showarrow = FALSE) %>%
#   add_annotations(xref = 'x', yref = 'y',
#                   x = x1 + x2 / 2, y = y,
#                   text = paste(data[,"x2"], '%'),
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(248, 248, 255)'),
#                   showarrow = FALSE) %>%
#   # labeling the first Likert scale (on the top)
#   add_annotations(xref = 'x', yref = 'paper',
#                   x = label_pos,
#                   y = 1.15,
#                   text = top_labels,
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE)
# p


# setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
# fighters <- read.csv("data/fighter_data.csv", header = T)
# # 
# # today <- Sys.Date()
# # fighters %>% mutate(
# #   age = round(age),
# #   years_till_now = as.numeric(difftime(today, as.Date(date), unit="weeks"))/52.25,
# #   current_age = round(age + years_till_now)
# # ) %>% select(date, years_till_now, age, current_age) 
# 
# image_file <- "www/figure.png"
# txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
# 
# x <- c(1.5, 1.5, 1.5, 1.5, 1.6, 1.6)
# y <- c(2.7, 2.7, 2, 2, 1.2, 1.2)
# attack_count <- c(23, 5, 14, 12, 13, 5)
# attack_color <- c('rgb(250,190,192)', 'rgb(228,61,64)', 'rgb(250,190,192)', 'rgb(228,61,64)', 'rgb(250,190,192)', 'rgb(228,61,64)')
# data <- data.frame(x, y, attack_count, attack_color)
# 
# plot_ly(
#   data, x = ~x, y = ~y, type = 'scatter', mode = 'markers', width = 200, height = 250, 
#   marker = list(size = ~attack_count, opacity = 0.75, color = ~attack_color) 
# ) %>%
#   config(displayModeBar = F) %>%
#   layout(
#     xaxis = list(
#       title = "",
#       showgrid = FALSE,
#       showline = FALSE,
#       showticklabels = FALSE,
#       zeroline = FALSE,
#       range = c(0.5, 2.5)),
#     yaxis = list(
#       title = "",
#       showgrid = FALSE,
#       showline = FALSE,
#       showticklabels = FALSE,
#       zeroline = FALSE,
#       range = c(0, 3.2)),
#     images = list(
#       list(
#         source =  paste('data:image/png;base64', txt, sep=','),
#         xref = "x",
#         yref = "y",
#         x = 1,
#         y = 3,
#         sizex = 1,
#         sizey = 3,
#         sizing = "stretch",
#         opacity = 0.4,
#         layer = "below"
#       )
#     ),
#     paper_bgcolor = 'rgba(248, 248, 255, 0)', plot_bgcolor = 'rgba(248, 248, 255, 0)'
#   )

# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)
# 
# plot_ly(data, x = ~SF_Zoo, y = ~Animals, type = 'bar', orientation = 'h',name = 'SF Zoo') %>%
#   add_trace(x = ~LA_Zoo, name = 'LA Zoo') %>%
#   layout(yaxis = list(title = 'Count'), barmode = 'group')

setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters <- read.csv("data/fighter_data.csv", header = T)
fighter_winning_stat_1 <- fighters %>%
  filter(fighter=="Alexa Grasso") %>%
  mutate(
    win_by_KO = win_by_KO.TKO + win_by_TKO_Doctor_Stoppage,
    win_by_Decision = win_by_Decision_Majority + win_by_Decision_Split + win_by_Decision_Unanimous,
    win_by_SUB = win_by_Submission
  ) %>%
  select(win_by_Decision, win_by_KO, win_by_SUB) %>%
  t %>% as.vector

fighter_winning_stat_2 <- fighters %>%
  filter(fighter=="Aljamain Sterling") %>%
  mutate(
    win_by_KO = win_by_KO.TKO + win_by_TKO_Doctor_Stoppage,
    win_by_Decision = win_by_Decision_Majority + win_by_Decision_Split + win_by_Decision_Unanimous,
    win_by_SUB = win_by_Submission
  ) %>%
  select(win_by_Decision, win_by_KO, win_by_SUB) %>%
  t %>% as.vector

winning_type <- c("By KO", "By Decision", "By Submission")
data <- data.frame(winning_type, fighter_winning_stat_1, fighter_winning_stat_2)

plot_ly(data, x = ~fighter_winning_stat_1, y = ~winning_type, 
        type = 'bar', orientation = 'h', name = 'Fighter 1', 
        marker = list(color = 'rgba(239, 83, 80, 0.8)',
                      line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
  add_trace(x = ~fighter_winning_stat_2, name = 'Fighter 2',
            marker = list(color = 'rgba(66, 165, 245, 0.8)',
                          line = list(color = 'rgba(0, 0, 0, 5)', width = 1))) %>%
  add_annotations(xref = 'x', yref = 'y', x = 'paper', y = ~winning_type,
                  xanchor = 'right',
                  text = ~winning_type,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(255, 255, 255)'),
                  showarrow = FALSE, align = 'left') %>%
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
    barmode = 'group',
    paper_bgcolor = 'rgba(0, 0, 0, 0.25)', plot_bgcolor = 'rgba(0, 0, 0, 0.25)',
    margin = list(l = 0, t = 0, r = 0, b = 0, pad = 0),
    showlegend = FALSE
  )




setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
fighters <- read.csv("data/fighter_data.csv", header = T)
fighter_general_stat_1 <- fighters %>%
  filter(fighter=="Al Iaquinta") %>%
  mutate(
    avg_SIG_STR_summary = paste0(avg_SIG_STR_landed, "/", avg_SIG_STR_att),
    avg_SIG_STR_pct = round(avg_SIG_STR_pct * 100, 2),
    avg_TD_summary = paste0(avg_TD_landed, "/", avg_TD_att),
    avg_TD_pct = round(avg_TD_pct * 100, 2),
    avg_TOTAL_STR_summary = paste0(avg_TOTAL_STR_landed, "/", avg_TOTAL_STR_att),
    avg_TOTAL_STR_pct = round(avg_TOTAL_STR_landed/avg_TOTAL_STR_att * 100, 2),
    avg_PASS = round(avg_PASS, 2),
    avg_REV = round(avg_REV, 2),
    avg_SUB_ATT = round(avg_SUB_ATT, 2)
  ) %>%
  # select(avg_SIG_STR_summary, avg_SIG_STR_pct, avg_TD_summary, avg_TD_pct, avg_TOTAL_STR_summary, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
  select(avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
  rename(
    #"Significant Strike" = avg_SIG_STR_summary, "Total Strike" = avg_TOTAL_STR_summary, "Take Down" = avg_TD_summary, 
    "Significant Strike %" = avg_SIG_STR_pct, "Total Strike %" = avg_TOTAL_STR_pct, "Take Down %" = avg_TD_pct, 
    "Pass" = avg_PASS, "Reversal" = avg_REV, "Submission Attemp" = avg_SUB_ATT
  ) %>% t

fighter_general_stat_2 <- fighters %>%
  filter(fighter=="Alejandro Perez") %>%
  mutate(
    avg_SIG_STR_summary = paste0(avg_SIG_STR_landed, "/", avg_SIG_STR_att),
    avg_SIG_STR_pct = round(avg_SIG_STR_pct * 100, 2),
    avg_TD_summary = paste0(avg_TD_landed, "/", avg_TD_att),
    avg_TD_pct = round(avg_TD_pct * 100, 2),
    avg_TOTAL_STR_summary = paste0(avg_TOTAL_STR_landed, "/", avg_TOTAL_STR_att),
    avg_TOTAL_STR_pct = round(avg_TOTAL_STR_landed/avg_TOTAL_STR_att * 100, 2),
    avg_PASS = round(avg_PASS, 2),
    avg_REV = round(avg_REV, 2),
    avg_SUB_ATT = round(avg_SUB_ATT, 2)
  ) %>%
  # select(avg_SIG_STR_summary, avg_SIG_STR_pct, avg_TD_summary, avg_TD_pct, avg_TOTAL_STR_summary, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
  select(avg_SIG_STR_pct, avg_TD_pct, avg_TOTAL_STR_pct, avg_PASS, avg_REV, avg_SUB_ATT) %>%
  rename (
    #"Significant Strike" = avg_SIG_STR_summary, "Total Strike" = avg_TOTAL_STR_summary, "Take Down" = avg_TD_summary, 
    "Significant Strike %" = avg_SIG_STR_pct, "Total Strike %" = avg_TOTAL_STR_pct, "Take Down %" = avg_TD_pct, 
    "Pass" = avg_PASS, "Reversal" = avg_REV, "Submission Attemp" = avg_SUB_ATT
  ) %>% t

fighter_general_stat <- cbind(fighter_general_stat_1, fighter_general_stat_2)
colnames(fighter_general_stat) <- c("Fighter 1", "Fighter 2")





profile_pic <- fighters %>% 
  filter(fighter=="Mitch Gagnon") %>% 
  select(url) %>% t



library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

fighters <- read.csv("data/fighter_data_v2.csv", header = T)
fighters_old <- read.csv("data/fighter_data_v1.csv", header = T)

fighters_new <- merge(fighters, fighters_old %>% select(fighter, url), by = "fighter")
fighters_new <- fighters_new %>% select(-url.x) %>% rename("url" = url.y)

write.csv(fighters_new, "data/fighter_data.csv", row.names=FALSE)

