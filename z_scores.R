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

## Aiming
Aiming <- Aiming %>%
  mutate(recip_MT = 1/MT)

#ggplot(Aiming, aes(x = MT, color = eye_condition)) + geom_density() + labs(title = "MT")
#ggplot(Aiming, aes(x = recip_MT, color = eye_condition)) + geom_density() + labs(title = "recip_MT")

aiming_worse <- Aiming %>%
  filter(eye_condition == "Worse") %>%
  select(P_ID_, recip_MT) %>%
  mutate(worse_recip_MT_z = (recip_MT - mean(recip_MT))/sd(recip_MT)) %>%
  arrange(P_ID_)

aiming_better <- Aiming %>%
  filter(eye_condition == "Better") %>%
  select(P_ID_, recip_MT) %>%
  mutate(better_recip_MT_z = (recip_MT - mean(recip_MT))/sd(recip_MT)) %>%
  arrange(P_ID_)

aiming_both <- Aiming %>%
  filter(eye_condition == "Both") %>%
  select(P_ID_, recip_MT) %>%
  mutate(both_recip_MT_z = (recip_MT - mean(recip_MT))/sd(recip_MT))%>%
  arrange(P_ID_)

