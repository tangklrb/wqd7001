
library(dplyr)
library(ggplot2)
library(scales) 
library(tidyr)
library(stringr)

#load data
#df = read.csv("C:/Users/Soon Loong/Desktop/ufcdata/data.csv")
setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
df = read.csv('data/data.csv', header=TRUE, sep = ",")

#Split Red And Blue Fighter into 2 data set
df1 = df[c(4,2,10:76,144)] 
df2 = df[c(4,1,77:143,145)]

#Rename Column for btoh data set to prepare for row bind
for (x in names(df1)){ 
  names(df1)[names(df1) == x] <- str_replace(x,"B_","")
}

for (x in names(df2)){ 
  names(df2)[names(df2) == x] <- str_replace(x,"R_","")
}

#Bind 2 dataset
df3 <- rbind(df1,df2)

##Sort the data base on date, latest on top, 
##so that when remove duplicate, 
##lastest fight data will be on top
# df4$date <- as.Date(df3$date,"%d/%m/%Y")
# df4 <- df4[rev(order(df4$date)),]
df4 <- df3
df4$date <- as.Date(df3$date, format = "%Y-%m-%d")
df4 <- df4[rev(order(df4$date)),]

##A quick Cleaning on the Data
  ## Remove Record with NA on fight data
df5 <- df4[!is.na(df4$avg_BODY_att),]

  ## 1 NA on Height, replace with Mean
df5[is.na(df5$Height_cms),]$Height_cms <- mean(df5$Height_cms, na.rm = T)

  ## Replace NA on reach with height x reach ratio
avgHeightReachRatio <- mean(df5$Reach_cms, na.rm = T) / mean(df5$Height_cms,na.rm = T)
df5[is.na(df5$Reach_cms),]$Reach_cms <- df5[is.na(df5$Reach_cms),]$Height_cms * avgHeightReachRatio

  #replace NA age with mean
df5[is.na(df5$age),]$age <- mean(df5$age, na.rm = T)

#Remove Duplicate Fighter Data, only use the lastest
df6 <- df5[!duplicated(df5$fighter),]


# """ 
# Not Using These
# MaxDistance <- max(df6$avg_DISTANCE_landed)
# MaxClose <- max(df6$avg_CLINCH_landed)
# MaxGround <- max(df6$avg_GROUND_landed)
# 
# df6$DistanceAttack <- df6$avg_DISTANCE_landed / MaxDistance
# df6$CloseAttack <- df6$avg_CLINCH_landed/ MaxClose
# df6$GroundAttack <- df6$avg_GROUND_landed/MaxGround
# """

## Compute Variable for visualization
df6$distance_att <- ifelse(df6$avg_DISTANCE_att == 0, 0, df6$avg_DISTANCE_landed / df6$avg_DISTANCE_att) 
df6$close_att <- ifelse(df6$avg_CLINCH_att == 0,0,df6$avg_CLINCH_landed / df6$avg_CLINCH_att) 
df6$ground_att <- ifelse(df6$avg_GROUND_att == 0 ,0, df6$avg_GROUND_landed / df6$avg_GROUND_att)
df6$distance_def <- ifelse(df6$avg_opp_DISTANCE_att == 0, 0, 1 - df6$avg_opp_DISTANCE_landed / df6$avg_opp_DISTANCE_att) 
df6$close_def <- ifelse(df6$avg_opp_CLINCH_att == 0,0,1 - df6$avg_opp_CLINCH_landed / df6$avg_opp_CLINCH_att) 
df6$ground_def <- ifelse(df6$avg_opp_GROUND_att == 0 ,0,1 - df6$avg_opp_GROUND_landed / df6$avg_opp_GROUND_att)

df6$total_normal_att <-df6$avg_HEAD_att + df6$avg_BODY_att + df6$avg_LEG_att
df6$head_att_pct <- ifelse(df6$total_normal_att == 0, 0 , df6$avg_HEAD_att / df6$total_normal_att)
df6$head_att_acc <- ifelse(df6$avg_HEAD_att ==0,0,df6$avg_HEAD_landed /df6$avg_HEAD_att)
df6$body_att_pct <- ifelse(df6$total_normal_att ==0 , 0, df6$avg_BODY_att / df6$total_normal_att)
df6$body_att_acc <- ifelse(df6$avg_BODY_att== 0,0,df6$avg_BODY_landed /df6$avg_BODY_att)
df6$leg_att_pct <- ifelse(df6$total_normal_att == 0,0,df6$avg_LEG_att / df6$total_normal_att)
df6$leg_att_acc <- ifelse(df6$avg_LEG_att == 0,0,df6$avg_LEG_landed /df6$avg_LEG_att)

 

## Merge Data with profile
#profile = read.csv("C:/Users/Soon Loong/Desktop/ufcdata/profile.csv")
profile = read.csv("data/profiles_full.csv")
image <- profile[c("name","url")]
#image <- image[duplicated(image$name),]
dup_image <- image[duplicated(image$name),]
image <- setdiff(image, dup_image)

##Join All
df7 <- merge(x = df6, y = image, by.x = "fighter",  by.y = "name",all.x = TRUE)
summary(df7)

##Names of Fighter that do not have url
df7[is.na(df7$url),]$fighter

write.csv(df7,"data/fighter_data.1.csv", row.names = FALSE)
# 
# fighter1 = read.csv("data/fighter_data.1.csv")
# fighter2 = read.csv("data/fighter_data.csv")
# 
# setdiff(fighter1, fighter2)
# names(fighter1)
# names(fighter2)

fighters = read.csv("data/fighter_data.csv")

in_game_stat <- fighters %>% 
  filter(fighter=="Aaron Riley") %>% 
  select(distance_def, close_def, close_att, distance_att, ground_att, ground_def) %>%
  mutate(gap_closure = distance_def) %>% t %>% as.vector

pound_to_kg <- function(weight) {
  return (round(weight*0.45359237, digits=2))
}

profile <- fighters %>% 
  filter(fighter=="Aaron Riley") %>% 
  mutate(Weight_kgs = pound_to_kg(Weight_lbs)) %>%
  select(fighter, age, Height_cms, Weight_kgs, Reach_cms, Stance) %>%
  rename(
    "Name" = fighter, "Age" = age, 
    "Height (cm)" = Height_cms, "Weight (kg)" = Weight_kgs, 
    "Reach (cm)" = Reach_cms
  ) %>% t

profile_pic <- fighters %>% 
  filter(fighter=="Aaron Riley") %>% 
  select(url) %>% t

