library(caret)
library(dplyr)
data_fighter <- read.csv("fighter_data2.csv")
#Load Model
load("Model2.RData")
load("Model_Compare.RData")
#Red Corner
red=data_fighter[1,]
#Blue Corner
blue=data_fighter[2,]

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

predict(fit.lda,fighter_choosen_data,type="prob")

