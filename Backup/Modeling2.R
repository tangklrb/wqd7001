#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("caTools")
#install.packages("resample")
#install.packages("corrplot")
#install.packages("lattice")
#install.packages("caret")

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(resample)
library(corrplot)
library(lattice)
library(caret)

data_ori <- read.csv("preprocessed_data.csv")

#Remove unwanted data: title bout, number of rounds, weight and etc.
data_clean <- data_ori %>% select(-title_bout,-no_of_rounds,-contains("Weight"),B_total_rounds_fought,R_total_rounds_fought,
                                  -B_avg_opp_SIG_STR_att,-B_avg_opp_SIG_STR_landed,-B_avg_SIG_STR_att,-B_avg_SIG_STR_landed,
                                  -R_avg_opp_SIG_STR_att,-R_avg_opp_SIG_STR_landed,-R_avg_SIG_STR_att,-R_avg_SIG_STR_landed,
                                  -B_avg_opp_TD_att,-B_avg_opp_TD_landed,-B_avg_TD_att,-B_avg_TD_landed,
                                  -R_avg_opp_TD_att,-R_avg_opp_TD_landed,-R_avg_TD_att,-R_avg_TD_landed,
                                  -B_avg_opp_TOTAL_STR_att,-B_avg_opp_TOTAL_STR_landed,-B_avg_TOTAL_STR_att,-B_avg_TOTAL_STR_landed,
                                  -R_avg_opp_TOTAL_STR_att,-R_avg_opp_TOTAL_STR_landed,-R_avg_TOTAL_STR_att,-R_avg_TOTAL_STR_landed,
                                  -B_avg_opp_HEAD_landed,-B_avg_opp_HEAD_att,-R_avg_opp_HEAD_landed,-R_avg_opp_HEAD_att,
                                  -B_avg_opp_BODY_landed,-B_avg_opp_BODY_att,-R_avg_opp_BODY_landed,-R_avg_opp_BODY_att,
                                  -B_avg_opp_LEG_landed,-B_avg_opp_LEG_att,-R_avg_opp_LEG_landed,-R_avg_opp_LEG_att)

#Gather Stance Data
data_clean <- data_clean %>% gather(B_Stance,B_Value,contains("B_Stance")) %>% filter(B_Value!=0) %>% 
  gather(R_Stance,R_Value,contains("R_Stance")) %>% filter(R_Value!=0) %>% select(-B_Value,-R_Value)

data_clean <- data_clean %>% mutate(R_Stance=case_when(R_Stance=="R_Stance_Open.Stance"~ 1 ,
                                                       R_Stance=="R_Stance_Orthodox"~ 2 ,
                                                       R_Stance=="R_Stance_Sideways"~ 3 ,
                                                       R_Stance=="R_Stance_Southpaw"~ 4 ,
                                                       R_Stance=="R_Stance_Switch"~ 5)) %>%
  mutate(B_Stance=case_when(B_Stance=="B_Stance_Open.Stance"~ 1 ,
                            B_Stance=="B_Stance_Orthodox"~ 2 ,
                            B_Stance=="B_Stance_Sideways"~ 3 ,
                            B_Stance=="B_Stance_Southpaw"~ 4 ,
                            B_Stance=="B_Stance_Switch"~ 5))

#Find Difference between fighters used skills: distance, clinch, ground for both attack and defence
data_clean <- data_clean %>% mutate(diff_distance_att=ifelse(B_avg_DISTANCE_att == 0, 0, B_avg_DISTANCE_landed / B_avg_DISTANCE_att)-
                                      ifelse(R_avg_DISTANCE_att == 0, 0, R_avg_DISTANCE_landed / R_avg_DISTANCE_att)) %>% 
  select(-R_avg_DISTANCE_landed,-R_avg_DISTANCE_att,-B_avg_DISTANCE_landed,-B_avg_DISTANCE_att)

data_clean <- data_clean %>% mutate(diff_close_att=ifelse(B_avg_CLINCH_att == 0, 0, B_avg_CLINCH_landed / B_avg_CLINCH_att)-
                                      ifelse(R_avg_CLINCH_att == 0, 0, R_avg_CLINCH_landed / R_avg_CLINCH_att)) %>% 
  select(-R_avg_CLINCH_landed,-R_avg_CLINCH_att,-B_avg_CLINCH_landed,-B_avg_CLINCH_att)

data_clean <- data_clean %>% mutate(diff_ground_att=ifelse(B_avg_GROUND_att == 0, 0, B_avg_GROUND_landed / B_avg_GROUND_att)-
                                      ifelse(R_avg_GROUND_att == 0, 0, R_avg_GROUND_landed / R_avg_GROUND_att)) %>% 
  select(-R_avg_GROUND_landed,-R_avg_GROUND_att,-B_avg_GROUND_landed,-B_avg_GROUND_att)

data_clean <- data_clean %>% mutate(diff_distance_def=ifelse(B_avg_opp_DISTANCE_att == 0, 0, B_avg_opp_DISTANCE_landed / B_avg_opp_DISTANCE_att)-
                                      ifelse(R_avg_opp_DISTANCE_att == 0, 0, R_avg_opp_DISTANCE_landed / R_avg_opp_DISTANCE_att)) %>% 
  select(-R_avg_opp_DISTANCE_landed,-R_avg_opp_DISTANCE_att,-B_avg_opp_DISTANCE_landed,-B_avg_opp_DISTANCE_att)

data_clean <- data_clean %>% mutate(diff_close_def=ifelse(B_avg_opp_CLINCH_att == 0, 0, B_avg_opp_CLINCH_landed / B_avg_opp_CLINCH_att)-
                                      ifelse(R_avg_opp_CLINCH_att == 0, 0, R_avg_opp_CLINCH_landed / R_avg_opp_CLINCH_att)) %>% 
  select(-R_avg_opp_CLINCH_landed,-R_avg_opp_CLINCH_att,-B_avg_opp_CLINCH_landed,-B_avg_opp_CLINCH_att)

data_clean <- data_clean %>% mutate(diff_ground_def=ifelse(B_avg_opp_GROUND_att == 0, 0, B_avg_opp_GROUND_landed / B_avg_opp_GROUND_att)-
                                      ifelse(R_avg_opp_GROUND_att == 0, 0, R_avg_opp_GROUND_landed / R_avg_opp_GROUND_att)) %>% 
  select(-R_avg_opp_GROUND_landed,-R_avg_opp_GROUND_att,-B_avg_opp_GROUND_landed,-B_avg_opp_GROUND_att)

#Difference between statistic of fighters based on the body parts
data_clean <- data_clean %>% mutate(R_total_normal_att=R_avg_HEAD_att+R_avg_BODY_att+R_avg_LEG_att)
data_clean <- data_clean %>% mutate(B_total_normal_att=B_avg_HEAD_att+B_avg_BODY_att+B_avg_LEG_att)


data_clean <- data_clean %>% mutate(diff_head_att_pct=ifelse(B_total_normal_att == 0, 0, B_avg_HEAD_att / B_total_normal_att)-
                                      ifelse(R_total_normal_att == 0, 0, R_avg_HEAD_att / R_total_normal_att)) 
data_clean <- data_clean %>% mutate(diff_head_att=ifelse(B_avg_HEAD_att == 0, 0, B_avg_HEAD_landed / B_avg_HEAD_att)-
                                      ifelse(R_avg_HEAD_att == 0, 0, R_avg_HEAD_landed / R_avg_HEAD_att)) %>%
  select(-B_avg_HEAD_landed,-B_avg_HEAD_att,-R_avg_HEAD_landed,-R_avg_HEAD_att)

data_clean <- data_clean %>% mutate(diff_body_att_pct=ifelse(B_total_normal_att == 0, 0, B_avg_BODY_att / B_total_normal_att)-
                                      ifelse(R_total_normal_att == 0, 0, R_avg_BODY_att / R_total_normal_att)) 
data_clean <- data_clean %>% mutate(diff_body_att=ifelse(B_avg_BODY_att == 0, 0, B_avg_BODY_landed / B_avg_BODY_att)-
                                      ifelse(R_avg_BODY_att == 0, 0, R_avg_BODY_landed / R_avg_BODY_att)) %>%
  select(-B_avg_BODY_landed,-B_avg_BODY_att,-R_avg_BODY_landed,-R_avg_BODY_att)

data_clean <- data_clean %>% mutate(diff_leg_att_pct=ifelse(B_total_normal_att == 0, 0, B_avg_LEG_att / B_total_normal_att)-
                                      ifelse(R_total_normal_att == 0, 0, R_avg_LEG_att / R_total_normal_att)) 
data_clean <- data_clean %>% mutate(diff_leg_att=ifelse(B_avg_LEG_att == 0, 0, B_avg_LEG_landed / B_avg_LEG_att)-
                                      ifelse(R_avg_LEG_att == 0, 0, R_avg_LEG_landed / R_avg_LEG_att)) %>%
  select(-B_avg_LEG_landed,-B_avg_LEG_att,-R_avg_LEG_landed,-R_avg_LEG_att)

#Combine Current losses and wins
data_clean <- data_clean %>% mutate(B_current_streak=ifelse(B_current_lose_streak==0,B_current_win_streak,-B_current_lose_streak)) %>%
  mutate(R_current_streak=ifelse(R_current_lose_streak==0,R_current_win_streak,-R_current_lose_streak)) %>%
  select(-B_current_lose_streak,-B_current_win_streak,-R_current_lose_streak,-R_current_win_streak)

#Calculate Wins percentage
data_clean <- data_clean %>% mutate(B_win_pct=(B_draw/2+B_wins-B_losses)/sum(B_draw+B_wins+B_losses)) %>%
  mutate(R_win_pct=(R_draw/2+R_wins-R_losses)/sum(R_draw+R_wins+R_losses)) %>%
  select(-B_draw,-B_wins,-B_losses,-R_draw,-R_wins,-R_losses)

#Calculate other attributes: Knockdown, Significant Strices, etc.
data_clean <- data_clean %>% mutate(diff_KD=B_avg_KD-R_avg_KD,diff_PASS=B_avg_PASS-R_avg_PASS,diff_REV=B_avg_REV-R_avg_REV,
                                    diff_SIG_STR_pct=B_avg_SIG_STR_pct-R_avg_SIG_STR_pct,
                                    diff_SUB_att=B_avg_SUB_ATT-R_avg_SUB_ATT,diff_TD_pct=B_avg_TD_pct-R_avg_TD_pct)
data_clean <- data_clean %>% mutate(diff_KD_def=B_avg_opp_KD-R_avg_opp_KD,diff_PASS_def=B_avg_opp_PASS-R_avg_opp_PASS,diff_REV_def=B_avg_opp_REV-R_avg_opp_REV,
                                    diff_SIG_STR_pct_def=B_avg_opp_SIG_STR_pct-R_avg_opp_SIG_STR_pct,
                                    diff_SUB_def=B_avg_opp_SUB_ATT-R_avg_opp_SUB_ATT,diff_TD_pct_def=B_avg_opp_TD_pct-R_avg_opp_TD_pct)  %>%
  select(-contains("_avg_"))

#Save in csv file
write.csv(data_clean,"data_model2.csv", row.names = FALSE)


##Modeling Part
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(123)
split = sample.split(data_clean, SplitRatio = 0.80)
training_set = subset(data_clean, split==TRUE)
test_set = subset(data_clean, split==FALSE)

#a) linear algorithms
set.seed(123)
fit.lda <- train(Winner~., data=data_clean, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(123)
fit.cart <- train(Winner~., data=data_clean, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(123)
fit.knn <- train(Winner~., data=data_clean, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(123)
fit.svm <- train(Winner~., data=data_clean, method="svmRadial", metric=metric, trControl=control)

#Comparisons
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm))
results

# estimate skill of LDA on the validation dataset
predict(fit.lda,test_set[2,],type="prob")

