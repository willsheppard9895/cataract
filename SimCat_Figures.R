# to make figures
# still need to work out how to remove legend as it's not helpful, but retain dots/markers


library(plyr)
library(tidyverse)
library(cowplot)
library (ggpubr)

rm(list = ls()) #clears all variables from workspace

#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Simulated_cataracts/Data/")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Simulated_cataracts/Data/")
setwd("C:/Users/wills/Documents/Cataract/Data")

#reading the data
all_data <- read.csv("AllData_forJASP.csv")

#all_data$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("Worse","Better","Both"))

#convert data to long format for making figures. at some point need to start using pivot_longer instead of gather

Aiming_data_long <- all_data %>%
  select(c(P_ID, Aim_NoFilter_Mean:Aim_2_Filter_Mean)) %>%
  gather(condition_comb, DV, Aim_NoFilter_Mean:Aim_2_Filter_Mean) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 5, end = 12))
  )

VA_data_long <- all_data %>%
  select(c(P_ID, VA_NoFilter:VA_2_Filter)) %>%
  gather(condition_comb, DV, VA_NoFilter:VA_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  )

CS_data_long <- all_data %>%
  select(c(P_ID, CS_NoFilter:CS_2_Filter)) %>%
  gather(condition_comb, DV, CS_NoFilter:CS_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 4, end = 11))
  )


Stereo_data_long <- all_data %>%
  select(c(P_ID, Stereo_NoFilter:Stereo_2_Filter)) %>%
  gather(condition_comb, DV, Stereo_NoFilter:Stereo_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 15))
  )

Pegboard_data_long <- all_data %>%
  select(c(P_ID, Pegb_NoFilter:Pegb_2_Filter)) %>%
  gather(condition_comb, DV, Pegb_NoFilter:Pegb_2_Filter) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 6, end = 13))
  )

WaterTime_data_long <- all_data %>%
  select(c(P_ID, Water_NoFilter_Time:Water_2_Filter_Time)) %>%
  gather(condition_comb, DV, Water_NoFilter_Time:Water_2_Filter_Time) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )

WaterAcc_data_long <- all_data %>%
  select(c(P_ID, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_Accuracy:Water_2_Filter_Accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )

WaterTimebyAcc_data_long <- all_data %>%
  select(c(P_ID, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy)) %>%
  gather(condition_comb, DV, Water_NoFilter_time.accuracy:Water_2_Filter_time.accuracy) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 3)) 
  )%>%
  mutate(eye_condition = as.factor(str_sub(condition_comb, start = 7, end = 14))
  )


#obtain summary stats (eye_condition means) for plotting
#change test_data_summary and test_data_long to test you want

summary <- function(df_long, grouping_var){
  # Summarises long form data around a chosen grouping variable, returns the mean, sd and standard erorr.
  # When writing your funciton you must put quo() around the grouping variable 
  # group_by(!!...) then removes the qutation, refered to as bang bang formating, without this the code will look for something called grouping_var

  summary_df <- df_long %>%
  group_by(!!grouping_var)%>%
    summarise(mean_DV = mean(DV), 
              sd = sd(DV), 
              n = n(), 
              se = sd/sqrt(n))%>%
    ungroup()
  
  return(summary_df)
}

Aiming_data_summary <- summary(Aiming_data_long, quo(eye_condition))
VA_data_summary <- summary(VA_data_long, quo(eye_condition))
CS_data_summary <- summary(CS_data_long, quo(eye_condition))
Stereo_data_summary <- summary(Stereo_data_long, quo(eye_condition))
Pegboard_data_summary <- summary(Pegboard_data_long, quo(eye_condition))
WaterTime_data_summary <- summary(WaterTime_data_long, quo(eye_condition))
WaterAcc_data_summary <- summary(WaterAcc_data_long, quo(eye_condition))
WaterTimebyAcc_data_summary <- summary(WaterTimebyAcc_data_long, quo(eye_condition))


Aiming_data_summary$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
VA_data_summary$eye_condition <- factor(VA_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
CS_data_summary$eye_condition <- factor(CS_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
Stereo_data_summary$eye_condition <- factor(Stereo_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
Pegboard_data_summary$eye_condition <- factor(Pegboard_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
WaterTime_data_summary$eye_condition <- factor(WaterTime_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
WaterAcc_data_summary$eye_condition <- factor(WaterAcc_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))
WaterTimebyAcc_data_summary$eye_condition <- factor(WaterTimebyAcc_data_summary$eye_condition, levels=c("NoFilter","1_Filter","2_Filter"))


# the next bunch of lines contain the formatting details for the plotting. you can embed this within the code for each plot, 
# but that would be repetitive as we're using the same formatting across the plots.

pd = position_dodge(0) # dodging of points for the mean points/error bars (off set from each other slightly). Set at 0 but feel free to wiggle it.

ps = 2 # point size
ls = 1 # line size
es = 0.5 # error bar size

# I've used the cowplot theme, as its clean-looking.
calc <- theme_cowplot() + theme(axis.title = element_text(size = 11),
                                axis.text = element_text(size = 9),
                                axis.text.x = element_text(margin = margin(5, 0, 2, 0)), 
                                axis.ticks.y = element_line(color = "black"), # adds some scale markers to y axis
                                axis.ticks.x = element_line(color = "black"),
                                #legend.title = element_blank(),
                                #legend.text = element_text(size = 11),
                                strip.text.x = element_text(size = 11, margin = margin(.2,0,.2,0, "cm")),
                                strip.background =element_rect(fill="white"),
                                legend.position = "none",
                                #legend.position = c(0.7, 0.2), # chooses where to put the legend. use 0.7,0.9 for Expt 1 and 0.7,0.2 for Expt 2.
                                #legend.direction = "vertical",
                                axis.line = element_line(color="black"),
                                panel.spacing = unit(0.2, "cm"),
                                #panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_blank(),
                                #panel.grid.major.y = element_line(linetype = "dashed", color = "gray80"),
                                panel.border = element_rect(color = "white"), # hides panel border, tho its still visible.
                                plot.background = element_rect(fill = "transparent", color = "transparent"),
)

theme_set(calc)


#Change to correct test_data_summary file in first line below
#change coord_cartesian limits (scale) for each test
#change labels for each test

SumPlot <- function(df, min = 0, max = 20, t = "MT by Visual Condition Plot", x = "blank"){
  # Creates summary plots, min, max and title defined ot allow quick examination
  plot <-  ggplot(data = df, aes(x=eye_condition, y=mean_DV, fill=eye_condition)) +
    geom_bar(stat = "identity", width=0.5, color = "black", fill = "white") +
    coord_cartesian(ylim = c(min , max)) + #change coordinates for each test
    scale_y_continuous(expand = c(0, 0)) +
    geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                  aes(ymax = mean_DV + se, 
                      ymin = mean_DV - se)) +
    geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
    scale_linetype_manual(values = c(1,2)) +
    scale_fill_grey(start = .05, end = .5) +
    scale_color_grey(start = .05, end = .5) +
    scale_shape_manual(values = c(21, 24)) +
    labs(title = t, x = "Visual Condition", y= "mean MT (s)") +
    geom_hline(linetype = x, yintercept = 0)
  
  ggsave(filename = paste(t,"_exp2.png", sep = ""), dpi = 800, height = 5, width = 6)
  
  return(plot)
}

Aiming_Plot <- SumPlot(Aiming_data_summary, 0.5, 1.5, "Aiming")
VA_Plot <- SumPlot(VA_data_summary, -0.1, .4, "VisualAcuity", x = "solid")
CS_Plot <- SumPlot(CS_data_summary, 7.5, 15, "ConstrastSensitivity")
Stereo_Plot <- SumPlot(Stereo_data_summary, 0, 8, "Steroacuity")
PB_Plot <- SumPlot(Pegboard_data_summary, 12, 16, "Pegboard")
WT_Plot <- SumPlot(WaterTime_data_summary, 12, 18, "WaterPouring-Time")
WA_Plot <- SumPlot(WaterAcc_data_summary, 5, 25, "WaterPouringAccuracy") +
  labs( y = "Accuracy (ml)")
WTA_Plot <- SumPlot(WaterTimebyAcc_data_summary, 100, 350, "WaterpouringTimebyaccuracy") +
  labs(y = "Time x Accuracy (s ml)")

#show(WTA_Plot)

SumPlot_NB <- function(df, min = 0, max = 20, t = "MT by Visual Condition Plot", x = "blank"){
  # Creates summary plots, min, max and title defined ot allow quick examination
  plot <-  ggplot(data = df, aes(x=eye_condition, y=mean_DV, fill=eye_condition)) +geom_bar(stat = "identity", width=0.5, color = "black", fill = "white") +
    coord_cartesian(ylim = c(min , max)) + #change coordinates for each test
    scale_y_continuous(expand = c(0, 0)) +
    geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                  aes(ymax = mean_DV + se, 
                      ymin = mean_DV - se)) +
    geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
    scale_linetype_manual(values = c(1,2)) +
    scale_fill_grey(start = .05, end = .5) +
    scale_color_grey(start = .05, end = .5) +
    scale_shape_manual(values = c(21, 24)) +
    labs(title = t, x = "Visual Condition", y= "mean MT (s)") +
    geom_hline(linetype = x, yintercept = 0)
  
  ggsave(filename = paste(t,"_exp2.png", sep = ""), dpi = 800, height = 5, width = 6)
  
  return(plot)
}

Aiming_Plot_NB <- SumPlot_NB(Aiming_data_summary, 0.5, 1.5, "Aiming")
VA_Plot_NB <- SumPlot_NB(VA_data_summary, -0.1, .4, "VisualAcuity", x = "solid")
CS_Plot_NB <- SumPlot_NB(CS_data_summary, 7.5, 15, "ConstrastSensitivity")
Stereo_Plot_NB <- SumPlot_NB(Stereo_data_summary, 0, 8, "Steroacuity")
PB_Plot_NB <- SumPlot_NB(Pegboard_data_summary, 12, 16, "Pegboard")
WT_Plot_NB <- SumPlot_NB(WaterTime_data_summary, 12, 18, "WaterPouring-Time")
WA_Plot_NB <- SumPlot_NB(WaterAcc_data_summary, 5, 25, "WaterPouringAccuracy") +
  labs( y = "Accuracy (ml)")
WTA_Plot_NB <- SumPlot_NB(WaterTimebyAcc_data_summary, 100, 350, "WaterpouringTimebyaccuracy") +
  labs(y = "Time x Accuracy (s ml)")


setwd("C:/Users/wills/Documents/Cataract/Figures")
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Simulated_cataracts/Data/Figures")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Simulated_cataracts/Data/Figures")
ggsave("2Aiming.png", dpi = 800, height = 5, width = 6)


