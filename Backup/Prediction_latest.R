library(caret)
library(dplyr)
data_fighter <- read.csv("fighter_data.csv")
#Load Model
load("Model.RData")

#Red Corner
red=data_fighter[1,]
#Blue Corner
blue=data_fighter[2,]

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
  diff_distance_att           =blue$distance_att - red$distance_att,
  diff_close_att              =blue$close_att - red$close_att,
  diff_ground_att             =blue$ground_att - red$ground_att,
  diff_distance_def           =blue$distance_def - red$distance_def,
  diff_close_def              =blue$close_def - red$close_def,
  diff_ground_def             =blue$ground_def- red$ground_def,
  R_total_normal_att          =red$total_normal_att,
  B_total_normal_att          =blue$total_normal_att,
  diff_head_att_pct           =blue$head_att_pct - red$head_att_pct,
  diff_head_att               =blue$head_att_acc - red$head_att_acc,
  diff_body_att_pct           =blue$body_att_pct - red$body_att_pct,
  diff_body_att               =blue$body_att_acc-red$body_att_acc,
  diff_leg_att_pct            =blue$leg_att_pct - red$leg_att_pct,
  diff_leg_att                =blue$leg_att_acc - red$leg_att_acc,
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

predict(fit.lda,fighter_choosen_data,type="prob")
