### creating a CKAT battery score for each participant
## Aplly recipricol transformations to all data - this reverses order and normalises the data
## to do this - z scores must be calculated for each condition, for each task
## then take the within task average for each participant
## then take an average of the three tasks for each participant
# Tracking - reciprocal RMSE for SlowWG, mediWG, fastWG, SlowNG, mediNG and fastNG for each condition  
# Aiming - reciprocal MT for each condition
# Steering - reciprocal pPA for Path A and Path B for each condition

# Import libraries
library(tidyverse)

# Import data
Aiming <- read.csv("C:/Users/wills/Documents/Cataract/Data/monoc_binoc_aiming.csv")
Tracking <- read.csv("C:/Users/wills/Documents/Cataract/Data/monoc_binoc_tracking.csv")
Steering <- read.csv("C:/Users/wills/Documents/Cataract/Data/monoc_binoc_steering.csv")

# remove participants with missing data - 44 (tracking)
Aiming <- Aiming %>%
  filter(P_ID_ != 44) %>%
  mutate(recip_MT = 1/MT)

Tracking <- Tracking %>%
  filter(P_ID_ != 44) %>%
  mutate(recip_RMSE = 1/RMSE)

z_score <- function(df, IV = condition_comb, condition, ID = P_ID_, DV){
  # Function that selects the ID and DV from a data frame, before calculating the z score of the DV and arranging by ID 
  
  IV <- enquo(IV)
  ID <- enquo(ID)
  DV <- enquo(DV)
  
  
  output <- df %>%
    filter(!!IV == condition) %>%
    mutate(z = (!!DV - mean(!!DV))/sd(!!DV) )  %>%
    arrange(!!ID) %>%
    select(z)
    
  
  return(output)
}

## Aiming


aiming_ID <- Aiming[, 1]

#ggplot(Aiming, aes(x = MT, color = eye_condition)) + geom_density() + labs(title = "MT")
#ggplot(Aiming, aes(x = recip_MT, color = eye_condition)) + geom_density() + labs(title = "recip_MT")

aiming_worse <- z_score(Aiming, eye_condition, condition = "Worse", DV = recip_MT) %>%
  rename("worse_recip_MT_z" = "z")


aiming_better <- z_score(Aiming, eye_condition, condition = "Better", DV = recip_MT) %>%
  rename("better_recip_MT_z" = "z")

aiming_both <- z_score(Aiming, eye_condition, condition = "Both", DV = recip_MT) %>%
  rename("both_recip_MT_z" = "z")

all_aiming <- cbind(aiming_ID, aiming_worse, aiming_better, aiming_both)

test <- data.frame(aiming_mean_z = colMeans(all_aiming[,2:4]))
                   


mean_aiming <- data.frame(ID=all_aiming[,1], aiming_mean_z = rowMeans(all_aiming[,-1]))

## Tracking
tracking_ID <- Tracking[, 1]

# NG_Slow
tracking_NG_slow_worse <- z_score(df = Tracking, condition = "Trkng_NG_Slow_Worse", DV = recip_RMSE) %>%
  rename("NG_slow_worse_RMSE" = "z")

tracking_NG_slow_better <- z_score(df = Tracking, condition = "Trkng_NG_Slow_Better", DV = recip_RMSE) %>%
  rename("NG_slow_better_RMSE" = "z")

tracking_NG_slow_both <- z_score(df = Tracking, condition = "Trkng_NG_Slow_Both", DV = recip_RMSE)%>%
  rename("NG_slow_both_RMSE" = "z")


# NG_Medi
tracking_NG_medium_worse <- z_score(df = Tracking, condition = "Trkng_NG_Medi_Worse", DV = recip_RMSE) %>%
  rename("NG_medium_worse_RMSE" = "z")

tracking_NG_medium_better <- z_score(df = Tracking, condition = "Trkng_NG_Medi_Better", DV = recip_RMSE) %>%
  rename("NG_medium_better_RMSE" = "z")

tracking_NG_medium_both <- z_score(df = Tracking, condition = "Trkng_NG_Medi_Both", DV = recip_RMSE)%>%
  rename("NG_medium_both_RMSE" = "z")


# NG_Fast
tracking_NG_fast_worse <- z_score(df = Tracking, condition = "Trkng_NG_Fast_Worse", DV = recip_RMSE) %>%
  rename("NG_fast_worse_RMSE" = "z")

tracking_NG_fast_better <- z_score(df = Tracking, condition = "Trkng_NG_Fast_Better", DV = recip_RMSE) %>%
  rename("NG_fast_better_RMSE" = "z")

tracking_NG_fast_both <- z_score(df = Tracking, condition = "Trkng_NG_Fast_Both", DV = recip_RMSE)%>%
  rename("NG_fast_both_RMSE" = "z")




# WG_Slow
tracking_WG_slow_worse <- z_score(df = Tracking, condition = "Trkng_WG_Slow_Worse", DV = recip_RMSE) %>%
  rename("WG_slow_worse_RMSE" = "z")

tracking_WG_slow_better <- z_score(df = Tracking, condition = "Trkng_WG_Slow_Better", DV = recip_RMSE) %>%
  rename("WG_slow_better_RMSE" = "z")

tracking_WG_slow_both <- z_score(df = Tracking, condition = "Trkng_WG_Slow_Both", DV = recip_RMSE)%>%
  rename("WG_slow_both_RMSE" = "z")


# WG_Medi
tracking_WG_medium_worse <- z_score(df = Tracking, condition = "Trkng_WG_Medi_Worse", DV = recip_RMSE) %>%
  rename("WG_medium_worse_RMSE" = "z")

tracking_WG_medium_better <- z_score(df = Tracking, condition = "Trkng_WG_Medi_Better", DV = recip_RMSE) %>%
  rename("WG_medium_better_RMSE" = "z")

tracking_WG_medium_both <- z_score(df = Tracking, condition = "Trkng_WG_Medi_Both", DV = recip_RMSE)%>%
  rename("WG_medium_both_RMSE" = "z")


# WG_Fast
tracking_WG_fast_worse <- z_score(df = Tracking, condition = "Trkng_WG_Fast_Worse", DV = recip_RMSE) %>%
  rename("WG_fast_worse_RMSE" = "z")

tracking_WG_fast_better <- z_score(df = Tracking, condition = "Trkng_WG_Fast_Better", DV = recip_RMSE) %>%
  rename("WG_fast_better_RMSE" = "z")

tracking_WG_fast_both <- z_score(df = Tracking, condition = "Trkng_WG_Fast_Both", DV = recip_RMSE)%>%
  rename("WG_fast_both_RMSE" = "z")

all_tracking <- cbind(tracking_ID, 
                      tracking_NG_slow_worse, tracking_NG_slow_better, tracking_NG_slow_both,
                      tracking_NG_medium_worse, tracking_NG_medium_better, tracking_NG_medium_both,
                      tracking_NG_fast_worse, tracking_NG_fast_better, tracking_NG_fast_both,
                      tracking_WG_slow_worse, tracking_WG_slow_better, tracking_WG_slow_both,
                      tracking_WG_medium_worse, tracking_WG_medium_better, tracking_WG_medium_both,
                      tracking_WG_fast_worse, tracking_WG_fast_better, tracking_WG_fast_both)

mean_tracking <- data.frame(ID=all_tracking[,1], tracking_mean_z = rowMeans(all_tracking[,-1]))

## Steering
