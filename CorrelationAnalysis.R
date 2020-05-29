library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggpubr)
library(grid)

rm(list = ls()) #clears all variables from workspace
setwd("C:/Users/wills/Documents/Cataract/Data")

####################### Exp 1 #######################
## import task data
exp1_data <- read.csv("All_Ps_GD_PP_EY_PbyROW.csv") #should change this to All_Ps_GD_PP_EY_PbyRow

# removing Ps 29 and 31 as they don't have Both Eye condition data for any task
exp1_data <- exp1_data[-c(28,29,33,34),]

#selecting just the columns we're interested in.
exp1_Tracking <- exp1_data %>%
  select(P_ID_, Eye_order_2_,Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)

# Select only the first 50 aiming trials, then only PL_T columns, calculate mean and median of PLT for aiming
#then calculate the mean 
exp1_aiming <- exp1_data %>%
  select(P_ID_, Eye_order_2_, aim_1_base_PL_T_1:aim_50_base_PL_T_50)

exp1_aiming <- exp1_aiming %>%
  select(P_ID_, Eye_order_2_, grep("PL_T_", names(exp1_aiming)))%>%
  mutate(Aim_Mean = rowMeans(select(., starts_with("aim")), na.rm = TRUE),) %>%
  select(P_ID_, Eye_order_2_, Aim_Mean)


pPA <- function(PA, PL){
  PA *(1+((PL -36)/36))
}

exp1_steering <- exp1_data %>%
  select(P_ID_, Eye_order_2_,Trace1_shapeA_PA_1, Trace1_shapeA_PL_T_1, Trace2_shapeB_PA_1, 
         Trace2_shapeB_PL_T_1, Trace3_shapeA_PA_1, Trace3_shapeA_PL_T_1, Trace4_shapeB_PA_1, 
         Trace4_shapeB_PL_T_1, Trace5_shapeA_PA_1, Trace5_shapeA_PL_T_1, Trace6_shapeB_PA_1, 
         Trace6_shapeB_PL_T_1)%>% 
  mutate(Trace1_shapeA_pPA = pPA(Trace1_shapeA_PA_1, Trace1_shapeA_PL_T_1),
         Trace2_shapeB_pPA = pPA(Trace2_shapeB_PA_1, Trace2_shapeB_PL_T_1),
         Trace3_shapeA_pPA = pPA(Trace3_shapeA_PA_1, Trace3_shapeA_PL_T_1),
         Trace4_shapeB_pPA = pPA(Trace4_shapeB_PA_1, Trace4_shapeB_PL_T_1),
         Trace5_shapeA_pPA = pPA(Trace5_shapeA_PA_1, Trace5_shapeA_PL_T_1), 
         Trace6_shapeB_pPA = pPA(Trace6_shapeB_PA_1, Trace6_shapeB_PL_T_1), 
         ShapeA_mean_pPA = (Trace1_shapeA_pPA + Trace3_shapeA_pPA + Trace5_shapeA_pPA)/3,
         ShapeB_mean_pPA = (Trace2_shapeB_pPA + Trace4_shapeB_pPA + Trace6_shapeB_pPA)/3,
  )%>%
  select(P_ID_, Eye_order_2_, ShapeA_mean_pPA:ShapeB_mean_pPA)

## now to transform data to wide format for analysis in SPSS/JASP.

exp1_wide_Tracking <- exp1_Tracking %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)
  )

exp1_wide_aiming <- exp1_aiming %>% # converts the data to an analysable format
  spread(Eye_order_2_, Aim_Mean)
#df_wide_Aiming$DV <- rep("Aiming_mean",nrow(df_wide_Aiming)) #this just adds new column saying this is PLT_mean

exp1_wide_steering<- exp1_steering %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(ShapeA_mean_pPA, ShapeB_mean_pPA)
  )

# reorders each by P_ID so can be combined in next step
exp1_wide_Tracking<- exp1_wide_Tracking %>%
  arrange(P_ID_)
exp1_wide_aiming <- exp1_wide_aiming %>%
  arrange(P_ID_)
exp1_wide_steering <- exp1_wide_steering %>%
  arrange(P_ID_)

# combines all wide dfs together - need to reorder by P_ID first first if individual files have PID in different orders?
exp1_wide_All_CKAT <- cbind(exp1_wide_Tracking, exp1_wide_aiming, exp1_wide_steering) # combines all tasks

colnames(exp1_wide_All_CKAT) <- make.unique(names(exp1_wide_All_CKAT))

# remove extra P_ID cols
exp1_wide_All_CKAT <- exp1_wide_All_CKAT %>%
  select(-c(P_ID_.1, P_ID_.2))

# rename columns to something sensible
exp1_wide_All_CKAT <- dplyr::rename(exp1_wide_All_CKAT, "Trkng_NG_Slow_Both" = "Trkng_NG_Slow_F_track_error_RMS_1_Both", "Trkng_NG_Slow_Worse" = "Trkng_NG_Slow_F_track_error_RMS_1_Bad", "Trkng_NG_Slow_Better" = "Trkng_NG_Slow_F_track_error_RMS_1_Good",
                                    "Trkng_NG_Medi_Both" = "Trkng_NG_Med_F_track_error_RMS_2_Both", "Trkng_NG_Medi_Worse" = "Trkng_NG_Med_F_track_error_RMS_2_Bad", "Trkng_NG_Medi_Better" = "Trkng_NG_Med_F_track_error_RMS_2_Good", 
                                    "Trkng_NG_Fast_Both" = "Trkng_NG_Fst_F_track_error_RMS_3_Both", "Trkng_NG_Fast_Worse" = "Trkng_NG_Fst_F_track_error_RMS_3_Bad", "Trkng_NG_Fast_Better" = "Trkng_NG_Fst_F_track_error_RMS_3_Good",
                                    "Trkng_WG_Slow_Both" = "Trkng_WG_Slow_F_track_error_RMS_1_Both", "Trkng_WG_Slow_Worse" = "Trkng_WG_Slow_F_track_error_RMS_1_Bad", "Trkng_WG_Slow_Better" = "Trkng_WG_Slow_F_track_error_RMS_1_Good",
                                    "Trkng_WG_Medi_Both" = "Trkng_WG_Med_F_track_error_RMS_2_Both", "Trkng_WG_Medi_Worse" = "Trkng_WG_Med_F_track_error_RMS_2_Bad", "Trkng_WG_Medi_Better" = "Trkng_WG_Med_F_track_error_RMS_2_Good", 
                                    "Trkng_WG_Fast_Both" = "Trkng_WG_Fst_F_track_error_RMS_3_Both", "Trkng_WG_Fast_Worse" = "Trkng_WG_Fst_F_track_error_RMS_3_Bad", "Trkng_WG_Fast_Better" = "Trkng_WG_Fst_F_track_error_RMS_3_Good",
                                    "Aiming_Better" = "Good", "Aiming_Worse" = "Bad", "Aiming_Both" = "Both",
                                    "SteeringA_Better" = "ShapeA_mean_pPA_Good", "SteeringA_Worse" = "ShapeA_mean_pPA_Bad", "SteeringA_Both" = "ShapeA_mean_pPA_Both", 
                                    "SteeringB_Better" = "ShapeB_mean_pPA_Good", "SteeringB_Worse" = "ShapeB_mean_pPA_Bad", "SteeringB_Both" = "ShapeB_mean_pPA_Both"
)



## import vision data

exp1_vision_data <- read.csv("exp1_ParticiapntVisionData.csv")

# convert to long format
exp1_long_vision <- exp1_vision_data %>%
  pivot_longer(-P_ID_, names_to = c("eye_condition"), values_to = "VA_LogMAR") 
  
exp1_cor_vision <- exp1_long_vision %>% 
  filter(P_ID_ != 44, eye_condition == c("LogMAR_BetterEye", "LogMAR_WorseEye", "LogMAR_BothEyes"))
  

exp1_cor_vision$eye_condition <- revalue(exp1_cor_vision$eye_condition, c(LogMAR_BetterEye = "Better",
                                                                            LogMAR_WorseEye = "Worse",
                                                                            LogMAR_BothEyes = "Both")) 
exp1_cor_vision <- exp1_cor_vision %>% 
  arrange(P_ID_, eye_condition)
 


#### import Tracking data and get ready for correlation - change
exp1_long_Tracking <- exp1_wide_All_CKAT[-c(16),] %>% # removed P44 (row 16) as no bad eye Tracking data
  select(c(P_ID_, Trkng_NG_Slow_Worse:Trkng_WG_Fast_Both)) %>%
  gather(condition_comb, RMSE, Trkng_NG_Slow_Worse:Trkng_WG_Fast_Both) %>% # might need to switch from gather to pivot_longer as no longer supported. df %>% gather("key", "value", x, y, z) is equivalent to df %>% pivot_longer(c(x, y, z), names_to = "key", values_to = "value")
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 5))
  )%>%
  mutate(guide = as.factor(str_sub(condition_comb, start = 7, end = 8))
  )%>%
  mutate(speed = as.factor(str_sub(condition_comb, start = 10, end = 13))
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 15, end = 20))
  )%>%
  mutate(RMSE = as.numeric(RMSE))

## select outcome measure for each condition

exp1_cor_TrackingNG_Slow <- exp1_long_Tracking %>%
  filter(guide == "NG", speed == "Slow") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_NG_Slow_RMSE" = "RMSE") %>%
  select(Tracking_NG_Slow_RMSE) 

exp1_cor_TrackingNG_Medi <- exp1_long_Tracking %>%
  filter(guide == "NG", speed == "Medi") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_NG_Medi_RMSE" = "RMSE")%>%
  select(Tracking_NG_Medi_RMSE)

exp1_cor_TrackingNG_Fast <- exp1_long_Tracking %>%
  filter(guide == "NG",  speed == "Fast") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_NG_Fast_RMSE" = "RMSE")%>%
  select(Tracking_NG_Fast_RMSE)


exp1_cor_TrackingWG_Slow  <- exp1_long_Tracking %>%
  filter(guide == "WG", speed == "Slow") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_WG_Slow_RMSE" = "RMSE")%>%
  select(Tracking_WG_Slow_RMSE)

exp1_cor_TrackingWG_Medi  <- exp1_long_Tracking %>%
  filter(guide == "WG", speed == "Medi") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_WG_Medi_RMSE" = "RMSE")%>%
  select(Tracking_WG_Medi_RMSE)

exp1_cor_TrackingWG_Fast  <- exp1_long_Tracking %>%
  filter(guide == "WG", speed == "Fast") %>%
  arrange(P_ID_, eye_condition) %>%
  dplyr::rename("Tracking_WG_Fast_RMSE" = "RMSE")%>%
  select(Tracking_WG_Fast_RMSE)

# join outcome measures to 1 data frame

exp1_cor_Tracking_ALL <- cbind(exp1_cor_TrackingNG_Slow, exp1_cor_TrackingNG_Medi, exp1_cor_TrackingNG_Fast,
                           exp1_cor_TrackingWG_Slow,exp1_cor_TrackingWG_Medi, exp1_cor_TrackingWG_Fast)

######## Import aiming data and get ready for correlation andalysis

exp1_long_aiming <- exp1_wide_All_CKAT %>%
  select(c(P_ID_, Aiming_Worse:Aiming_Better)) %>%
  gather(condition_comb, MT, Aiming_Worse:Aiming_Better) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 6)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 13))
  ) %>%
  arrange(P_ID_, eye_condition)

exp1_cor_aiming <- exp1_long_aiming %>%
  filter(P_ID_ != 44) %>%
  dplyr::rename("Aiming_MT" = "MT") %>% 
  select(Aiming_MT)


######## Import steering data and get ready for correlation andalysis

exp1_long_steering <- exp1_wide_All_CKAT %>%
  select(c(P_ID_, SteeringA_Worse:SteeringB_Both)) %>%
  gather(condition_comb, pPA, SteeringA_Worse:SteeringB_Both) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 8))
  )%>%
  mutate(shape = as.factor(str_sub(condition_comb, start = 9, end = 9))
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 11, end = 16))
  ) %>%
  arrange(P_ID_, eye_condition)

exp1_cor_steering_A <- exp1_long_steering %>%
  filter(shape == "A", P_ID_ != 44) %>%
  dplyr::rename("Steering_A_pPA" = "pPA")%>%
  select(Steering_A_pPA)  

exp1_cor_steering_B <- exp1_long_steering %>%
  filter(shape == "B", P_ID_ != 44) %>%
  dplyr::rename("Steering_B_pPA" = "pPA")%>%
  select(Steering_B_pPA) 

exp1_cor_steering <- cbind(exp1_cor_steering_A, exp1_cor_steering_B)

##### Bring together exp1 data for correlation anaylsis ######

exp1_participant_condition_list <- exp1_long_aiming %>% ##### did not use in the end, could be helpful to keep
  filter(P_ID_ != 44) %>%
  select(P_ID_, eye_condition)

exp1_cor_ALL_TASKS <- cbind(exp1_cor_vision,exp1_cor_Tracking_ALL, exp1_cor_aiming, exp1_cor_steering)


### Calculate change

Both = filter(exp1_cor_ALL_TASKS, exp1_cor_ALL_TASKS$eye_condition=="Both")
Better = filter(exp1_cor_ALL_TASKS, exp1_cor_ALL_TASKS$eye_condition=="Better")
Worse = filter(exp1_cor_ALL_TASKS, exp1_cor_ALL_TASKS$eye_condition=="Worse")

## create individual dfs for change between conditions

BetterMinusBoth = Better[,3:12] - Both[,3:12]
BetterMinusBoth$change <- ("BetterMinusBoth")
BetterMinusBoth$P_ID <- (Both$P_ID)

WorseMinusBoth = Worse[,3:12] - Both[,3:12]
WorseMinusBoth$change <- ("WorseMinusBoth")
WorseMinusBoth$P_ID <- (Both$P_ID)

WorseMinusBetter = Worse[,3:12] - Better[,3:12]
WorseMinusBetter$change <- ("WorseMinusBetter")
WorseMinusBetter$P_ID <- (Both$P_ID)

## join dataframes

exp1_ALL_CHANGE <- rbind(BetterMinusBoth, WorseMinusBoth, WorseMinusBetter)

## create correlation matric

exp1_change_cor <- rcorr((as.matrix(exp1_ALL_CHANGE[, 1:10])), type = "spearman")
exp1_change_corplot <- corrplot.mixed(exp1_change_cor$r, order="hclust", upper = "ellipse", lower = "number",
                             p.mat = exp1_change_cor$P, sig.level = 0.05, insig = "blank")

exp1_change_VA_trackingNG <- ggplot(exp1_ALL_CHANGE, aes(x = VA_LogMAR, y = Tracking_NG_Fast_RMSE)) +
  geom_point()+
  geom_smooth(method = lm) + 
  stat_cor(method = "spearman")+
  xlab("Change in visual acuity (LogMAR)") +
  ylab("Change in RMSE")+
  labs(title = "Delta VA - Tracking Fast WG")

exp1_change_VA_trackingWG <- ggplot(exp1_ALL_CHANGE, aes(x = VA_LogMAR, y = Tracking_WG_Medi_RMSE)) +
  geom_point()+
  geom_smooth(method = lm) + 
  stat_cor(method = "spearman")+
  xlab("Change in visual acuity (LogMAR)") +
  ylab("Change in RMSE")+
  labs(title = "Delta VA - Tracking Medium NG")

exp1_change_VA_aiming <- ggplot(exp1_ALL_CHANGE, aes(x = VA_LogMAR, y = Aiming_MT)) +
  geom_point()+
  geom_smooth(method = lm)+ 
  stat_cor(method = "spearman")+
  xlab("Change in visual acuity (LogMAR)") +
  ylab("Change in Movement Time (s)") +
  labs(title = "Delta VA - Aiming MT")

## does removing outliers change much?

filtered <- exp1_ALL_CHANGE %>%
  select(P_ID, change, VA_LogMAR, Tracking_NG_Fast_RMSE, Aiming_MT) %>%
  rename("VA" = VA_LogMAR, "tracking" = Tracking_NG_Fast_RMSE,  "aiming" = Aiming_MT) 

filtered_tracking <- filtered %>%
  select(P_ID, change, VA, tracking) %>%
  filter(tracking < (mean(tracking) + 2*sd(tracking)), tracking > (mean(tracking) - 2*sd(tracking)))
  
filtered_aiming <- filtered %>%
  select(P_ID, change, VA, aiming) %>%
  filter(aiming < (mean(aiming) + 2*sd(aiming)), aiming > (mean(aiming) - 2*sd(aiming)))

filtered_tracking_fig <- ggplot(filtered_tracking, aes(x = VA, y = tracking)) +
  geom_point()+
  geom_smooth(method = lm) + 
  stat_cor(method = "spearman") +
  ylab("RMSE")

filtered_aiming_fig <- ggplot(filtered_aiming, aes(x = VA, y = aiming)) +
  geom_point()+
  geom_smooth(method = lm)+ 
  stat_cor(method = "spearman") +
  ylab("MT")



####################### Exp 2 #######################

#reading the data
exp2_data <- read.csv("AllData_forJASP.csv")

#exp2_data$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("Worse","Better","Both"))

#convert data to long format for making figures. at some point need to start using pivot_longer instead of gather

exp2_long_VA <- exp2_data %>%
  select(c(P_ID, VA_NoFilter:VA_2_Filter)) %>%
  gather(condition_comb, DV, VA_NoFilter:VA_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  ) %>%
  arrange(P_ID, eye_condition)

exp2_long_CS <- exp2_data %>%
  select(c(P_ID, CS_NoFilter:CS_2_Filter)) %>%
  gather(condition_comb, DV, CS_NoFilter:CS_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  ) %>%
  arrange(P_ID, eye_condition)


exp2_long_stereo <- exp2_data %>%
  select(c(P_ID, Stereo_NoFilter:Stereo_2_Filter)) %>%
  gather(condition_comb, DV, Stereo_NoFilter:Stereo_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 15))
  )%>%
  arrange(P_ID, eye_condition)

exp2_long_aiming <- exp2_data %>%
  select(c(P_ID, Aim_NoFilter_Mean:Aim_2_Filter_Mean)) %>%
  gather(condition_comb, DV, Aim_NoFilter_Mean:Aim_2_Filter_Mean) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 5, end = 12))
  ) %>%
  arrange(P_ID, eye_condition)

exp2_long_pegboard <- exp2_data %>%
  select(c(P_ID, Pegb_NoFilter:Pegb_2_Filter)) %>%
  gather(condition_comb, DV, Pegb_NoFilter:Pegb_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 6, end = 13))
  ) %>%
  arrange(P_ID, eye_condition)

exp2_long_water_time <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_Time:Water_2_Filter_Time)) %>%
  gather(condition_comb, DV, Water_NoFilter_Time:Water_2_Filter_Time) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  ) %>%
  arrange(P_ID, eye_condition)

exp2_long_water_acc <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  ) %>%
  arrange(P_ID, eye_condition)

exp2_long_water_time_acc <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  ) %>%
  arrange(P_ID, eye_condition)

condition <- exp2_long_aiming %>%
  select(P_ID, eye_condition)

exp2_long_ALL_MEASURES <- cbind(condition, exp2_long_VA$DV, exp2_long_CS$DV, exp2_long_stereo$DV, 
                                exp2_long_aiming$DV, exp2_long_pegboard$DV, 
                                exp2_long_water_acc$DV, exp2_long_water_time$DV, 
                                exp2_long_water_time_acc$DV)
# make names more readable

exp2_long_ALL_MEASURES <- exp2_long_ALL_MEASURES %>%
  rename("VA" = "exp2_long_VA$DV",
         "CS" = "exp2_long_CS$DV",
         "Stereo" = "exp2_long_stereo$DV",
         "AimingMT" = "exp2_long_aiming$DV",
         "Pegboard" = "exp2_long_pegboard$DV",
         "PouringAcc" = "exp2_long_water_acc$DV",
         "PouringTime" = "exp2_long_water_time$DV",
         "PouingTimeAcc" = "exp2_long_water_time_acc$DV")
  

### Calculate change between filter conditons ###

NoFilter = filter(exp2_long_ALL_MEASURES, exp2_long_ALL_MEASURES$eye_condition=="NoFilter")
OneFilter = filter(exp2_long_ALL_MEASURES, exp2_long_ALL_MEASURES$eye_condition=="1_Filter")
TwoFilter = filter(exp2_long_ALL_MEASURES, exp2_long_ALL_MEASURES$eye_condition=="2_Filter")

OneMinusNone = OneFilter[,3:10] - NoFilter[,3:10]
OneMinusNone$change <- ("OneMinusNone")
OneMinusNone$P_ID <- (NoFilter$P_ID)

TwoMinusNone = TwoFilter[,3:10] - NoFilter[,3:10]
TwoMinusNone$change <- ("TwoMinusNone")
TwoMinusNone$P_ID <- (NoFilter$P_ID)

TwoMinusOne = TwoFilter[,3:10] - OneFilter[,3:10]
TwoMinusOne$change <- ("TwoMinusOne")
TwoMinusOne$P_ID <- (NoFilter$P_ID)

OMN_cor <- rcorr((as.matrix(OneMinusNone[,1:8])), type = "spearman")
OMN_corplot <- corrplot.mixed(OMN_cor$r, order="hclust",
                              p.mat = OMN_cor$P, sig.level = 0.05, insig = "blank")

TMN_cor <- rcorr((as.matrix(TwoMinusNone[,1:8])), type = "spearman")
TMN_corplot <- corrplot.mixed(TMN_cor$r, order="hclust",
                              p.mat = TMN_cor$P, sig.level = 0.05, insig = "blank")

TMO_cor <- rcorr((as.matrix(TwoMinusOne[,1:8])), type = "spearman")
TMO_corplot <- corrplot.mixed(TMO_cor$r, order="hclust",
                              p.mat = TMO_cor$P, sig.level = 0.05, insig = "blank")

exp2_ALL_CHANGE <- rbind(OneMinusNone, TwoMinusOne, TwoMinusOne)
AC_cor <- rcorr((as.matrix(exp2_ALL_CHANGE[, 1:8])), type = "spearman")
AC_corplot <- corrplot.mixed(AC_cor$r, order="hclust", upper = "ellipse", lower = "number",
                             p.mat = AC_cor$P, sig.level = 0.05, insig = "blank")

exp2_change_stereo_pouringtime <- ggplot(exp2_ALL_CHANGE, aes(x = Stereo, y = PouringTime)) +
  geom_point()+
  geom_smooth(method = lm)+ 
  stat_cor(method = "spearman")+
  xlab("Change in Stereoacuity") +
  ylab("Change in Pouring Time (s)")+
  labs(title = "Delta Steroacuity - Pouring time")
