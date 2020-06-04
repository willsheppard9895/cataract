# This script investigates the effect of removing participants with no interocular difference(IOdiff) on CKAT measures

library(tidyverse)
library(ggpubr)
library(rstatix)

rm(list = ls()) #clears all variables from workspace

# set working directory
setwd("C:/Users/wills/Documents/Cataract/Data")

# import data
all_data <- read.csv("exp1_wide_ALL_MEASURES.csv") 

# Filter participants with no IOdiff
IOdiff_data <- all_data %>%
  filter(LogMAR_EyeDifference != 0)

# data: data frame
# dv: (numeric) the dependent (or outcome) variable name.
# wid: variable name specifying the case/sample identifier.
# within: within-subjects factor or grouping variable


# Aiming
## One-way repeated measures anova
### dv = MT
### wid = P_ID_
### within = visual condition

aiming_data <- all_data %>%
  select(P_ID_, Aiming_Worse:Aiming_Better) %>%
  rename("Worse" = "Aiming_Worse",
         "Better" = "Aiming_Better",
         "Both" = "Aiming_Both")

# gather columns to long format
aiming_data <- aiming_data %>%
  gather(key = "eye_condition", value = "MT", Worse, Both, Better) %>%
  convert_as_factor(P_ID_, eye_condition) %>%
  reorder_levels(eye_condition, order = c("Worse", "Better", "Both"))

# Get summary statistics
aiming_summary_stats <- aiming_data %>%
  group_by(eye_condition) %>%
  get_summary_stats(MT, type = "mean_sd")

# Group boxplot
aiming_bp <- ggplot(aiming_data, aes(x = eye_condition, y = MT)) +
  geom_boxplot() +
  geom_point()

# Check normality
aiming_norm <-aiming_data %>%
  group_by(eye_condition) %>%
  shapiro_test(MT)

aiming_qq <- ggqqplot(aiming_data, "MT", facet.by = "eye_condition") # happy that data is normally distributed

# Run ANOVA
aiming.aov <- anova_test(data = aiming_data, 
                         dv = MT, 
                         wid = P_ID_, 
                         within = eye_condition)
get_anova_table(aiming.aov)

# pairwise comparison
aiming_pwc <- aiming_data %>%
  pairwise_t_test(
    MT ~ eye_condition, paired = TRUE,
    p.adjust.method = "holm"
  )
aiming_cohens_d <- aiming_data %>%
  cohens_d(MT ~ eye_condition, paired = TRUE)

# Steering
## Two-way repeated measures anova
### dv = pPA
### wid = P_ID_
### within = visual condition, shape
steeringing_norm <- shapiro_test()


# Tracking
## Three-way repeated measures anova
### dv = RMSE
### wid = P_ID_
### within = visual condition, speed, guide
tracking_norm <- shapiro_test()

