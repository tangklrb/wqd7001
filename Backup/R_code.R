
library(dplyr)
library(ggplot2)
library(scales) 
library(tidyr)
library(stringr)

#load data
df = read.csv("C:/Users/Soon Loong/Desktop/ufcdata/data.csv", na.strings=c("","NA"))

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
df3$date <- as.Date(df3$date,"%d/%m/%Y")
df4 <- df3[rev(order(df3$date)),]

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


""" 
Not Using These
MaxDistance <- max(df6$avg_DISTANCE_landed)
MaxClose <- max(df6$avg_CLINCH_landed)
MaxGround <- max(df6$avg_GROUND_landed)

df6$DistanceAttack <- df6$avg_DISTANCE_landed / MaxDistance
df6$CloseAttack <- df6$avg_CLINCH_landed/ MaxClose
df6$GroundAttack <- df6$avg_GROUND_landed/MaxGround
"""

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
profile = read.csv("C:/Users/Soon Loong/Desktop/ufcdata/profile.csv")
image <- profile[c("name","url")]
image <- image[duplicated(image$name),]

##Join All
df7 <- merge(x = df6, y = image, by.x = "fighter",  by.y = "name",all.x = TRUE)


##R doesnt have built in Mode, so has to create a funtion for it
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


##Replace empty stance with mode 
df7 <-  df7 %>% mutate( Stance = replace(Stance,
                                        is.na(Stance),
                                        Mode(Stance)))

summary(df7)


##Names of Fighter that do not have url
df7[is.na(df7$url),]$fighter

write.csv(df7,"C:/Users/Soon Loong/Desktop/ufcdata/fighter_data.csv", row.names = FALSE)




