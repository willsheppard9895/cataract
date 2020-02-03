library(plyr)
library(tidyverse)
library(cowplot)
library (ggpubr)
library(Hmisc)

## Exp 1

rm(list = ls()) #clears all variables from workspace
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
setwd("C:/Users/wills/Documents/Cataract/Data")

#reading the data
exp1_data <- read.csv("All_Ps_GD_PP_EY_PbyROW.csv") #should change this to All_Ps_GD_PP_EY_PbyRow

# removing Ps 29 and 31 as they don't have Both Eye condition data for any task
exp1_data <- exp1_data[-c(28,29,33,34),]

#selecting just the columns we're interested in.
exp1_tracking <- exp1_data %>%
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


##################
## now to transform data to wide format for analysis in SPSS/JASP.

exp1_wide_tracking <- exp1_tracking %>%
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
exp1_wide_tracking<- exp1_wide_tracking %>%
  arrange(P_ID_)
exp1_wide_aiming <- exp1_wide_aiming %>%
  arrange(P_ID_)
exp1_wide_steering <- exp1_wide_steering %>%
  arrange(P_ID_)

# combines all wide dfs together - need to reorder by P_ID first first if individual files have PID in different orders?
exp1_wide_All_CKAT <- cbind(exp1_wide_tracking, exp1_wide_aiming, exp1_wide_steering) # combines all tasks

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

#write.csv(df_wide_All_CKAT,"ALL_CKAT_for_JASP.csv", row.names = F)


#######################################
# to make figures
# these lines convert the data back into 'long' form -again!

exp1_long_tracking <- exp1_wide_All_CKAT[-c(16),] %>% # removed P44 (row 16) as no bad eye tracking data
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


exp1_long_aiming <- exp1_wide_All_CKAT %>%
  select(c(P_ID_, Aiming_Worse:Aiming_Better)) %>%
  gather(condition_comb, MT, Aiming_Worse:Aiming_Better) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 6)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 13))
  )

exp1_long_steering <- exp1_wide_All_CKAT %>%
  select(c(P_ID_, SteeringA_Worse:SteeringB_Both)) %>%
  gather(condition_comb, pPA, SteeringA_Worse:SteeringB_Both) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 8))
  )%>%
  mutate(shape = as.factor(str_sub(condition_comb, start = 9, end = 9))
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 11, end = 16))
  )

# obtain group summary stats to make figures #### throwing error ####



exp1_summary_tracking <- exp1_long_tracking%>% 
  group_by(eye_condition,guide,speed)%>%
  dplyr::summarise(mean_RMSE = mean(RMSE), #why wont mean work but median will?
                   sd = sd(RMSE), 
                   n = (dplyr::n()), 
                   se = sd/sqrt(n))%>%
  ungroup()

exp1_summary_aiming <- exp1_long_aiming%>%
  group_by(eye_condition)%>%
  dplyr::summarise(mean_MT = mean(MT), 
                   sd = sd(MT), 
                   n = (dplyr::n()), 
                   se = sd/sqrt(n))%>%
  ungroup()

exp1_summary_steering <- exp1_long_steering%>%
  group_by(eye_condition,shape)%>%
  dplyr::summarise(mean_pPA = mean(pPA), 
                   sd = sd(pPA), 
                   n = (dplyr::n()), 
                   se = sd/sqrt(n))%>%
  ungroup()

### not doing this anymore for some reason
# R was treating eye_condition as a numerical variable so we had to convert it to a factor
#exp1_summary_tracking$eye_condition <- as.factor(exp1_summary_tracking$eye_condition)
#Aiming_data_summary$eye_condition <- as.factor(Aiming_data_summary$eye_condition)
#Steering_data_summary$eye_condition <- as.factor(Steering_data_summary$eye_condition)

# the below lines rename the levels, and reorder them (as R defaults to alphabetical order)
exp1_summary_tracking$speed <- revalue(exp1_summary_tracking$speed,
                                       c("Medi" = "Medium"))
exp1_summary_tracking$guide <- revalue(exp1_summary_tracking$guide,
                                       c("NG" = "No Guide", "WG" = "With Guide"))
exp1_summary_tracking$speed <- factor(exp1_summary_tracking$speed, levels=c("Slow","Medium","Fast")) # this says reorder to slow, med, fast
exp1_summary_tracking$eye_condition <- factor(exp1_summary_tracking$eye_condition, levels=c("Worse","Better","Both"))

exp1_summary_steering$shape <- revalue(exp1_summary_steering$shape,
                                       c("A" = "Path_A", "B" = "Path_B"))
exp1_summary_steering$eye_condition <- factor(exp1_summary_steering$eye_condition, levels=c("Worse","Better","Both"))

exp1_summary_aiming$eye_condition <- factor(exp1_summary_aiming$eye_condition, levels=c("Worse","Better","Both"))


#this is the code that draws the plot itself
exp1_summary_tracking_NG <- exp1_summary_tracking  %>%
  filter(guide=="No Guide")

exp1_summary_tracking_WG <- exp1_summary_tracking %>%
  filter(guide=="With Guide")

################################################################################## Exp 2 ################################################################################################## 

#reading the data
exp2_data <- read.csv("AllData_forJASP.csv")

#exp2_data$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("Worse","Better","Both"))

#convert data to long format for making figures. at some point need to start using pivot_longer instead of gather

exp2_long_aiming <- exp2_data %>%
  select(c(P_ID, Aim_NoFilter_Mean:Aim_2_Filter_Mean)) %>%
  gather(condition_comb, DV, Aim_NoFilter_Mean:Aim_2_Filter_Mean) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 5, end = 12))
  )

exp2_long_VA <- exp2_data %>%
  select(c(P_ID, VA_NoFilter:VA_2_Filter)) %>%
  gather(condition_comb, DV, VA_NoFilter:VA_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  )

exp2_long_CS <- exp2_data %>%
  select(c(P_ID, CS_NoFilter:CS_2_Filter)) %>%
  gather(condition_comb, DV, CS_NoFilter:CS_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  )


exp2_long_stereo <- exp2_data %>%
  select(c(P_ID, Stereo_NoFilter:Stereo_2_Filter)) %>%
  gather(condition_comb, DV, Stereo_NoFilter:Stereo_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 15))
  )

exp2_long_pegboard <- exp2_data %>%
  select(c(P_ID, Pegb_NoFilter:Pegb_2_Filter)) %>%
  gather(condition_comb, DV, Pegb_NoFilter:Pegb_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 6, end = 13))
  )

exp2_long_water_time <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_Time:Water_2_Filter_Time)) %>%
  gather(condition_comb, DV, Water_NoFilter_Time:Water_2_Filter_Time) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )

exp2_long_water_acc <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )

exp2_long_water_time_acc <- exp2_data %>%
  select(c(P_ID, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )


#obtain summary stats (eye_condition means) for plotting
#change test_data_summary and test_data_long to test you want

summary <- function(df_long, grouping_var, task){
  # Summarises long form data around a chosen grouping variable, returns the mean, sd and standard erorr.
  # When writing your funciton you must put quo() around the grouping variable 
  # group_by(!!...) then removes the qutation, refered to as bang bang formating, without this the code will look for something called grouping_var

  
  summary_df <- df_long %>%
    group_by(!!grouping_var)%>%
    dplyr::summarise(mean_DV = mean(DV), 
              sd = sd(DV), 
              n = dplyr::n(), 
              se = sd/sqrt(n))%>%
    ungroup() 
    
    names(summary_df)[names(summary_df) == "mean_DV"] <-paste(task, "_mean", sep = "") 
    names(summary_df)[names(summary_df) == "sd"] <-paste(task, "_sd", sep = "")
    names(summary_df)[names(summary_df) == "se"] <-paste(task, "_se", sep = "")
  
  
  return(summary_df)
}

exp2_summary_aiming <- summary(exp2_long_aiming, quo(eye_condition), task = "aiming")
exp2_summary_VA <- summary(exp2_long_VA, quo(eye_condition), task = "VisualAcuity")
exp2_summary_CS <- summary(exp2_long_CS, quo(eye_condition), task = "ConstrastSensitivity")
exp2_summary_stereo <- summary(exp2_long_stereo, quo(eye_condition), task = "Stereoacuity")
exp2_summary_pegboard <- summary(exp2_long_pegboard, quo(eye_condition), task = "PegboardPlacements")
exp2_summary_water_time <- summary(exp2_long_water_time, quo(eye_condition), task = "PouringTime")
exp2_summary_water_acc <- summary(exp2_long_water_acc, quo(eye_condition), task = "PouringAccuracy")
exp2_summary_water_time_acc <- summary(exp2_long_water_time_acc, quo(eye_condition), task = "PouringTimeAcc")


exp2_summary_aiming$eye_condition <- factor(exp2_summary_aiming$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_VA$eye_condition <- factor(exp2_summary_VA$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_CS$eye_condition <- factor(exp2_summary_CS$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_stereo$eye_condition <- factor(exp2_summary_stereo$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_pegboard$eye_condition <- factor(exp2_summary_pegboard$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_water_time$eye_condition <- factor(exp2_summary_water_time$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_water_acc$eye_condition <- factor(exp2_summary_water_acc$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
exp2_summary_water_time_acc$eye_condition <- factor(exp2_summary_water_time_acc$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))

exp2_ALL_MEASURES <- cbind(exp2_summary_VA, subset(exp2_summary_CS, select = -c(eye_condition, n)), subset(exp2_summary_stereo, select = -c(eye_condition, n)), 
                           subset(exp2_summary_aiming, select = -c(eye_condition, n)), subset(exp2_summary_pegboard, select = -c(eye_condition, n)),
                           subset(exp2_summary_water_time, select = -c(eye_condition, n)),  subset(exp2_summary_water_acc, select = -c(eye_condition, n)),
                           subset(exp2_summary_water_time_acc, select = -c(eye_condition, n)))

exp2_ALL_MEASURES <- exp2_ALL_MEASURES[c(1, 4, 2, 3, 5:26)]

ZeroOne <- exp2_ALL_MEASURES[3, c(3, 6, 9, 12, 15, 18, 21, 24)] - exp2_ALL_MEASURES[1, c(3, 6, 9, 12, 15, 18, 21, 24)]
ZeroTwo <- exp2_ALL_MEASURES[3, c(3, 6, 9, 12, 15, 18, 21, 24)] - exp2_ALL_MEASURES[2, c(3, 6, 9, 12, 15, 18, 21, 24)]
OneTwo <- exp2_ALL_MEASURES[1, c(3, 6, 9, 12, 15, 18, 21, 24)] - exp2_ALL_MEASURES[2, c(3, 6, 9, 12, 15, 18, 21, 24)]

delta <- rbind(ZeroOne, ZeroTwo, OneTwo)

change = c("ZeroMinusOne", "ZeroMinusTwo", "OneMinusTwo")

delta <- cbind(change, delta)

delta <- delta %>% remove_rownames %>% column_to_rownames(var="change")


######################################### Create plots with delta data set ####################################

#corr_matrix <- cor(delta)
#corr_matrix

corr_matrix2 <- rcorr(as.matrix(delta))
corr_matrix2

               