
# this script takes CKAT data seperates into tracking, aiming and steering
# and formats first for exporting to JASP then makes figures
# line 145 need to work out why tracking data RMSE means aren't calculating (median is fine)
# then why tracking plot wont work "Error in mean_RMSE + se : non-numeric argument to binary operator"

library(plyr)
library(tidyverse)
library(cowplot)
library (ggpubr)


rm(list = ls()) #clears all variables from workspace
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
setwd("C:/Users/wills/Documents/Cataract/Data")

#reading the data
all_data <- read.csv("All_Ps_GD_PP_EY_PbyROW.csv") #should change this to All_Ps_GD_PP_EY_PbyRow

# removing Ps 29 and 31 as they don't have Both Eye condition data for any task
all_data <- all_data[-c(28,29,33,34),]

#selecting just the columns we're interested in.
Tracking <- all_data %>%
  select(P_ID_, Eye_order_2_,Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)

# Select only the first 50 aiming trials, then only PL_T columns, calculate mean and median of PLT for aiming
#then calculate the mean 
Aiming <- all_data %>%
  select(P_ID_, Eye_order_2_, aim_1_base_PL_T_1:aim_50_base_PL_T_50)

Aiming <- Aiming %>%
  select(P_ID_, Eye_order_2_, grep("PL_T_", names(Aiming)))%>%
  mutate(Aim_Mean = rowMeans(select(., starts_with("aim")), na.rm = TRUE),) %>%
  select(P_ID_, Eye_order_2_, Aim_Mean)


pPA <- function(PA, PL){
  PA *(1+((PL -36)/36))
}

Steering <- all_data %>%
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

df_wide_Tracking <- Tracking %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)
  )

df_wide_Aiming <- Aiming %>% # converts the data to an analysable format
  spread(Eye_order_2_, Aim_Mean)
#df_wide_Aiming$DV <- rep("Aiming_mean",nrow(df_wide_Aiming)) #this just adds new column saying this is PLT_mean

df_wide_Steering<- Steering %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(ShapeA_mean_pPA, ShapeB_mean_pPA)
  )

# reorders each by P_ID so can be combined in next step
df_wide_Tracking<- df_wide_Tracking %>%
  arrange(P_ID_)
df_wide_Aiming <- df_wide_Aiming %>%
  arrange(P_ID_)
df_wide_Steering <- df_wide_Steering %>%
  arrange(P_ID_)

# combines all wide dfs together - need to reorder by P_ID first first if individual files have PID in different orders?
df_wide_All_CKAT <- cbind(df_wide_Tracking, df_wide_Aiming, df_wide_Steering) # combines all tasks

colnames(df_wide_All_CKAT) <- make.unique(names(df_wide_All_CKAT))

# remove extra P_ID cols
df_wide_All_CKAT <- df_wide_All_CKAT %>%
  select(-c(P_ID_.1, P_ID_.2))

# rename columns to something sensible
df_wide_All_CKAT <- dplyr::rename(df_wide_All_CKAT, "Trkng_NG_Slow_Both" = "Trkng_NG_Slow_F_track_error_RMS_1_Both", "Trkng_NG_Slow_Worse" = "Trkng_NG_Slow_F_track_error_RMS_1_Bad", "Trkng_NG_Slow_Better" = "Trkng_NG_Slow_F_track_error_RMS_1_Good",
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

Tracking_data_long <- df_wide_All_CKAT[-c(16),] %>% # removed P44 (row 16) as no bad eye tracking data
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
                        

Aiming_data_long <- df_wide_All_CKAT %>%
  select(c(P_ID_, Aiming_Worse:Aiming_Better)) %>%
  gather(condition_comb, MT, Aiming_Worse:Aiming_Better) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 6)) 
         )%>%
      mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 13))
            )

Steering_data_long <- df_wide_All_CKAT %>%
  select(c(P_ID_, SteeringA_Worse:SteeringB_Both)) %>%
  gather(condition_comb, pPA, SteeringA_Worse:SteeringB_Both) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 8))
        )%>%
      mutate(shape = as.factor(str_sub(condition_comb, start = 9, end = 9))
            )%>%
          mutate(eye_condition = as.factor(str_sub(condition_comb, start = 11, end = 16))
                )

# obtain group summary stats to make figures #### throwing error ####


# Tracking
Tracking_data_summary <- Tracking_data_long %>% 
  group_by(eye_condition, guide, speed) %>%
  dplyr::summarise(mean_RMSE = mean(RMSE), #why wont mean work but median will?
            sd = sd(RMSE), 
            n = dplyr::n(), 
            se = sd/sqrt(n))%>%
  ungroup() %>%
  rename("Speed" = "speed")

# the below lines rename the levels, and reorder them (as R defaults to alphabetical order)
Tracking_data_summary$speed <- revalue(Tracking_data_summary$Speed,
                                       c("Medi" = "Medium"))
Tracking_data_summary$guide <- revalue(Tracking_data_summary$guide,
                                       c("NG" = "No Guide", "WG" = "With Guide"))
Tracking_data_summary$speed <- factor(Tracking_data_summary$Speed, levels=c("Slow","Medium","Fast")) # this says reorder to slow, med, fast
Tracking_data_summary$eye_condition <- factor(Tracking_data_summary$eye_condition, levels=c("Worse","Better","Both"))

Tracking_data_NG <- Tracking_data_summary  %>%
  filter(guide=="No Guide")

Tracking_data_WG <- Tracking_data_summary %>%
  filter(guide=="With Guide")

# Steering

Steering_data_summary <- Steering_data_long%>%
  group_by(eye_condition,shape)%>%
  dplyr::summarise(mean_pPA = mean(pPA), 
                   sd = sd(pPA), 
                   n = dplyr::n(), 
                   se = sd/sqrt(n))%>%
  ungroup() %>%
  rename("Shape" = "shape") %>%
  mutate(Shape = ifelse(as.character(Shape) == "A", "Path A", "Path B"))

Steering_data_summary$eye_condition <- factor(Steering_data_summary$eye_condition, levels=c("Worse","Better","Both"))


# Aiming

Aiming_data_summary <- Aiming_data_long%>%
  group_by(eye_condition)%>%
  dplyr::summarise(mean_MT = mean(MT), 
            sd = sd(MT), 
            n = (dplyr::n()), 
            se = sd/sqrt(n))%>%
  ungroup()


Aiming_data_summary$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("Worse","Better","Both"))

# the next bunch of lines contain the formatting details for the plotting. you can embed this within the code for each plot, 
# but that would be repetitive as we're using the same formatting across the plots.

pd = position_dodge(0) # dodging of points for the mean points/error bars (off set from each other slightly). Set at 0 but feel free to wiggle it.

ps = 2.5 # point size
ls = 1 # line size
es = 0.5 # error bar size

# I've used the cowplot theme, as its clean-looking.
calc <- theme_cowplot() + theme(axis.title = element_text(size = 11),
                                axis.text = element_text(size = 9),
                                axis.text.x = element_text(margin = margin(5, 0, 2, 0)), 
                                axis.ticks.y = element_line(color = "black"), # adds some scale markers to y axis
                                axis.ticks.x = element_line(color = "black"),
                                legend.title = element_blank(),
                                legend.text = element_text(size = 11),
                                strip.text.x = element_text(size = 11, margin = margin(.2,0,.2,0, "cm")),
                                strip.background =element_rect(fill="white"),
                                legend.position = c(0.7, 0.2), # chooses where to put the legend. use 0.7,0.9 for Expt 1 and 0.7,0.2 for Expt 2.
                                legend.direction = "vertical",
                                axis.line = element_line(color="black"),
                                panel.spacing = unit(0.2, "cm"),
                                #panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_blank(),
                                #panel.grid.major.y = element_line(linetype = "dashed", color = "gray80"),
                                panel.border = element_rect(color = "white"), # hides panel border, tho its still visible.
                                plot.background = element_rect(fill = "transparent", color = "transparent")
)

theme_set(calc)


## Tracking plots 
plot1 <- ggplot(Tracking_data_NG, aes(x = eye_condition, y = mean_RMSE,
                                     shape = Speed, color = Speed, fill = Speed, group = Speed)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = Speed), alpha = .8, position = pd, size = ls) +  # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0,30)) + # this is the y axis c(0,1) means zero to 1.
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_RMSE + se, ymin = mean_RMSE - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2,3)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = "Visual Condition", y= "mean RMSE", tag = "No Guide") + 
  theme(legend.position = c(0.7, 0.85), legend.title = element_text(size = 11),
        plot.tag = element_text(size = 11), plot.tag.position = c(0.2, 1))
#show(plot1)


plot2 <- ggplot(Tracking_data_WG, aes(x = eye_condition, y = mean_RMSE,
                                     shape = Speed, color = Speed, fill = Speed, group = Speed)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = Speed), alpha = .8, position = pd, size = ls) +  # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0,30)) + # this is the y axis c(0,1) means zero to 1.
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_RMSE + se, ymin = mean_RMSE - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2,3)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = "Visual Condition", y= "mean RMSE", tag = "With Guide") +
  theme(legend.position = "none", plot.tag = element_text(size = 11), plot.tag.position = c(0.1, 1))
#show(plot2)


## ----mb_trackinging_plot--------
# this pastes the 2 tracking plots together on one figure
tracking_plot <- ggarrange(plot1, plot2 + rremove("ylab") + rremove("y.text") + rremove("legend"),
                    #labels = c("No Guide", "With Guide"), hjust = -1.0, vjust = 1.0, # hjust and vjust move the position of the label (horiz and vertical). Smaller numbers mean further right and down
                    font.label = list(size = 11, color= "black"),
                    ncol = 2, nrow = 1,
                    align = "h"
                    ) 

#common.legend = TRUE, legend = "bottom") # also need to change legend to "horizontal" in calc section
show(tracking_plot)


#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
setwd("C:/Users/wills/Documents/Cataract/Figures/General/Monoc_Binoc")
ggsave("Tracking.png", dpi = 800, height = 4, width = 6)

## ----mb_steering_plot--------


#Steering
Steering_plot <- ggplot(Steering_data_summary, aes(x = eye_condition, y = mean_pPA,
                                                   shape = Shape, color = Shape, fill = Shape, group = Shape)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = Shape), alpha = .8, position = pd) + # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0.7,1.0)) + # this is the y axis c(0,1) means zero to 1. 
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_pPA + se, 
                    ymin = mean_pPA - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "mean pPA") + 
  theme(legend.position = c(0.7, 0.7), legend.title = element_text(size = 11),
        legend.text = element_text(size = 11))

show(Steering_plot)


setwd("C:/Users/wills/Documents/Cataract/Figures/General/Monoc_Binoc")
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Will_Paper/Figures")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
ggsave("Steering.png", dpi = 800, height = 4, width = 6)

## ----mb_aiming_plot--------

#Aiming
Aiming_Plot <- ggplot(data = Aiming_data_summary, aes(x=eye_condition, y=mean_MT, fill=eye_condition)) +
  geom_bar(stat = "identity", width=0.5, color = "black", fill = "white") +
  coord_cartesian(ylim = c(0.5,1.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_MT + se, 
                    ymin = mean_MT - se)) +
  geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "mean MT (s)") +
  theme(legend.position = "none")

#show(Aiming_Plot)

ggsave("Aiming_Plot", dpi = 800, height = 4, width = 6)

Aiming_Plot_no_bar <- ggplot(data = Aiming_data_summary, aes(x=eye_condition, y=mean_MT, fill=eye_condition)) +
  coord_cartesian(ylim = c(0.5,1.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_MT + se, 
                    ymin = mean_MT - se)) +
  geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "mean MT (s)") +
  theme(legend.position = "none")

show(Aiming_Plot_no_bar)

setwd("C:/Users/wills/Documents/Cataract/Figures")
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Will_Paper/Figures")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
ggsave("Aiming_No_Bar.png", dpi = 800, height = 4, width = 6)


# this script takes CKAT data seperates into tracking, aiming and steering
# and formats first for exporting to JASP then makes figures
# line 145 need to work out why tracking data RMSE means aren't calculating (median is fine)
# then why tracking plot wont work "Error in mean_RMSE + se : non-numeric argument to binary operator"

library(plyr)
library(tidyverse)
library(cowplot)
library (ggpubr)


rm(list = ls()) #clears all variables from workspace
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Monocular_Binocular_Expt/StudentData")
setwd("C:/Users/wills/Documents/Cataract/Data")

#reading the data
all_data <- read.csv("All_Ps_GD_PP_EY_PbyROW.csv") #should change this to All_Ps_GD_PP_EY_PbyRow

# removing Ps 29 and 31 as they don't have Both Eye condition data for any task
all_data <- all_data[-c(28,29,33,34),]

#selecting just the columns we're interested in.
Tracking <- all_data %>%
  select(P_ID_, Eye_order_2_,Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)

# Select only the first 50 aiming trials, then only PL_T columns, calculate mean and median of PLT for aiming
#then calculate the mean 
Aiming <- all_data %>%
  select(P_ID_, Eye_order_2_, aim_1_base_PL_T_1:aim_50_base_PL_T_50)

Aiming <- Aiming %>%
  select(P_ID_, Eye_order_2_, grep("PL_T_", names(Aiming)))%>%
  mutate(Aim_Mean = rowMeans(select(., starts_with("aim")), na.rm = TRUE),) %>%
  select(P_ID_, Eye_order_2_, Aim_Mean)


pPA <- function(PA, PL){
  PA *(1+((PL -36)/36))
}

Steering <- all_data %>%
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

df_wide_Tracking <- Tracking %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(Trkng_NG_Slow_F_track_error_RMS_1, Trkng_NG_Med_F_track_error_RMS_2, Trkng_NG_Fst_F_track_error_RMS_3, Trkng_WG_Slow_F_track_error_RMS_1, Trkng_WG_Med_F_track_error_RMS_2, Trkng_WG_Fst_F_track_error_RMS_3)
  )

df_wide_Aiming <- Aiming %>% # converts the data to an analysable format
  spread(Eye_order_2_, Aim_Mean)
#df_wide_Aiming$DV <- rep("Aiming_mean",nrow(df_wide_Aiming)) #this just adds new column saying this is PLT_mean

df_wide_Steering<- Steering %>%
  tidyr::pivot_wider(
    names_from  = c(Eye_order_2_), # Can accommodate more variables, if needed.
    values_from = c(ShapeA_mean_pPA, ShapeB_mean_pPA)
  )

# reorders each by P_ID so can be combined in next step
df_wide_Tracking<- df_wide_Tracking %>%
  arrange(P_ID_)
df_wide_Aiming <- df_wide_Aiming %>%
  arrange(P_ID_)
df_wide_Steering <- df_wide_Steering %>%
  arrange(P_ID_)

# combines all wide dfs together - need to reorder by P_ID first first if individual files have PID in different orders?
df_wide_All_CKAT <- cbind(df_wide_Tracking, df_wide_Aiming, df_wide_Steering) # combines all tasks

colnames(df_wide_All_CKAT) <- make.unique(names(df_wide_All_CKAT))

# remove extra P_ID cols
df_wide_All_CKAT <- df_wide_All_CKAT %>%
  select(-c(P_ID_.1, P_ID_.2))

# rename columns to something sensible
df_wide_All_CKAT <- dplyr::rename(df_wide_All_CKAT, "Trkng_NG_Slow_Both" = "Trkng_NG_Slow_F_track_error_RMS_1_Both", "Trkng_NG_Slow_Worse" = "Trkng_NG_Slow_F_track_error_RMS_1_Bad", "Trkng_NG_Slow_Better" = "Trkng_NG_Slow_F_track_error_RMS_1_Good",
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

Tracking_data_long <- df_wide_All_CKAT[-c(16),] %>% # removed P44 (row 16) as no bad eye tracking data
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
                        

Aiming_data_long <- df_wide_All_CKAT %>%
  select(c(P_ID_, Aiming_Worse:Aiming_Better)) %>%
  gather(condition_comb, MT, Aiming_Worse:Aiming_Better) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 6)) 
         )%>%
      mutate(eye_condition = as.factor(str_sub(condition_comb, start = 8, end = 13))
            )

Steering_data_long <- df_wide_All_CKAT %>%
  select(c(P_ID_, SteeringA_Worse:SteeringB_Both)) %>%
  gather(condition_comb, pPA, SteeringA_Worse:SteeringB_Both) %>%
  mutate(test = as.factor(str_sub(condition_comb,start = 1,end = 8))
        )%>%
      mutate(shape = as.factor(str_sub(condition_comb, start = 9, end = 9))
            )%>%
          mutate(eye_condition = as.factor(str_sub(condition_comb, start = 11, end = 16))
                )

# obtain group summary stats to make figures #### throwing error ####



Tracking_data_summary <- Tracking_data_long%>% 
  group_by(eye_condition,guide,speed)%>%
  dplyr::summarise(mean_RMSE = mean(RMSE), #why wont mean work but median will?
            sd = sd(RMSE), 
            n = dplyr::n(), 
            se = sd/sqrt(n))%>%
  ungroup()

Aiming_data_summary <- Aiming_data_long%>%
  group_by(eye_condition)%>%
  dplyr::summarise(mean_MT = mean(MT), 
            sd = sd(MT), 
            n = (dplyr::n()), 
            se = sd/sqrt(n))%>%
  ungroup()

Steering_data_summary <- Steering_data_long%>%
  group_by(eye_condition,shape)%>%
  dplyr::summarise(mean_pPA = mean(pPA), 
            sd = sd(pPA), 
            n = dplyr::n(), 
            se = sd/sqrt(n))%>%
  ungroup()

### not doing this anymore for some reason
# R was treating eye_condition as a numerical variable so we had to convert it to a factor
#Tracking_data_summary$eye_condition <- as.factor(Tracking_data_summary$eye_condition)
#Aiming_data_summary$eye_condition <- as.factor(Aiming_data_summary$eye_condition)
#Steering_data_summary$eye_condition <- as.factor(Steering_data_summary$eye_condition)

# the below lines rename the levels, and reorder them (as R defaults to alphabetical order)
Tracking_data_summary$speed <- revalue(Tracking_data_summary$speed,
                                       c("Medi" = "Medium"))
Tracking_data_summary$guide <- revalue(Tracking_data_summary$guide,
                                       c("NG" = "No Guide", "WG" = "With Guide"))
Tracking_data_summary$speed <- factor(Tracking_data_summary$speed, levels=c("Slow","Medium","Fast")) # this says reorder to slow, med, fast
Tracking_data_summary$eye_condition <- factor(Tracking_data_summary$eye_condition, levels=c("Worse","Better","Both"))

Steering_data_summary$shape <- revalue(Steering_data_summary$shape,
                                       c("A" = "Path_A", "B" = "Path_B"))
Steering_data_summary$eye_condition <- factor(Steering_data_summary$eye_condition, levels=c("Worse","Better","Both"))

Aiming_data_summary$eye_condition <- factor(Aiming_data_summary$eye_condition, levels=c("Worse","Better","Both"))


# the next bunch of lines contain the formatting details for the plotting. you can embed this within the code for each plot, 
# but that would be repetitive as we're using the same formatting across the plots.

pd = position_dodge(0) # dodging of points for the mean points/error bars (off set from each other slightly). Set at 0 but feel free to wiggle it.

ps = 2.5 # point size
ls = 1 # line size
es = 0.5 # error bar size

# I've used the cowplot theme, as its clean-looking.
calc <- theme_cowplot() + theme(axis.title = element_text(size = 11),
                                axis.text = element_text(size = 9),
                                axis.text.x = element_text(margin = margin(5, 0, 2, 0)), 
                                axis.ticks.y = element_line(color = "black"), # adds some scale markers to y axis
                                axis.ticks.x = element_line(color = "black"),
                                legend.title = element_blank(),
                                legend.text = element_text(size = 11),
                                strip.text.x = element_text(size = 11, margin = margin(.2,0,.2,0, "cm")),
                                strip.background =element_rect(fill="white"),
                                legend.position = c(0.7, 0.2), # chooses where to put the legend. use 0.7,0.9 for Expt 1 and 0.7,0.2 for Expt 2.
                                legend.direction = "vertical",
                                axis.line = element_line(color="black"),
                                panel.spacing = unit(0.2, "cm"),
                                #panel.grid.major.x = element_blank(),
                                panel.grid.major.y = element_blank(),
                                #panel.grid.major.y = element_line(linetype = "dashed", color = "gray80"),
                                panel.border = element_rect(color = "white"), # hides panel border, tho its still visible.
                                plot.background = element_rect(fill = "transparent", color = "transparent")
)

theme_set(calc)

#this is the code that draws the plot itself
Tracking_data_NG <- Tracking_data_summary  %>%
  filter(guide=="No Guide")

Tracking_data_WG <- Tracking_data_summary %>%
  filter(guide=="With Guide")


## create plots

plot1 <- ggplot(Tracking_data_NG, aes(x = eye_condition, y = mean_RMSE,
                                     shape = speed, color = speed, fill = speed, group = speed)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = speed), alpha = .8, position = pd, size = ls) +  # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0,30)) + # this is the y axis c(0,1) means zero to 1.
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_RMSE + se, ymin = mean_RMSE - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2,3)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = "Visual Condition", y= "Mean RMSE") + 
  theme(legend.position = c(0.7, 0.85), legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))
#show(plot1)


plot2 <- ggplot(Tracking_data_WG, aes(x = eye_condition, y = mean_RMSE,
                                     shape = speed, color = speed, fill = speed, group = speed)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = speed), alpha = .8, position = pd, size = ls) +  # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0,30)) + # this is the y axis c(0,1) means zero to 1.
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_RMSE + se, ymin = mean_RMSE - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2,3)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = "Visual Condition", y= "Mean RMSE") + 
  theme(legend.position = c(0.7, 0.5), legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
#show(plot2)


## ----mb_trackinging_plot--------
# this pastes the 2 tracking plots together on one figure
tracking_plot <- ggarrange(plot1, plot2 + rremove("ylab") + rremove("y.text") + rremove("legend"),
                    labels = c("No Guide", "With Guide"), hjust = -1.0, vjust = 1.0, # hjust and vjust move the position of the label (horiz and vertical). Smaller numbers mean further right and down
                    font.label = list(size = 11, color= "black"),
                    ncol = 2, nrow = 1,
                    align = "h"
                    )#+ theme(legend.position = c(0, 0.5))

#common.legend = TRUE, legend = "bottom") # also need to change legend to "horizontal" in calc section
#show(figure)


#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
setwd("C:/Users/wills/Documents/Cataract/Figures")
ggsave("Tracking.png", dpi = 800, height = 8, width = 10)

## ----mb_steering_plot--------

#Steering
Steering_plot <- ggplot(Steering_data_summary, aes(x = eye_condition, y = mean_pPA,
                                                   shape = shape, color = shape, fill = shape, group = shape)) + # add a + to the end of this line if not naming plots and remove plot <- and plot1 <- from next line
  geom_line(aes(linetype = shape), alpha = .8, position = pd) + # if you want to remove the lines, hash out this line
  coord_cartesian(ylim = c(0.7,1.0)) + # this is the y axis c(0,1) means zero to 1. 
  scale_y_continuous(expand = c(0, 0)) + # this means bars start at 0 without a weird gap at the bottom
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_pPA + se, 
                    ymin = mean_pPA - se)) +
  geom_point(position = pd, size = ps, color = "black") +
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "Mean penalised path accuracy") + 
  theme(legend.position = c(0.7, 0.7), legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

show(Steering_plot)


setwd("C:/Users/wills/Documents/Cataract/Figures")
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Will_Paper/Figures")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
ggsave("Steering.png", dpi = 800, height = 4, width = 6)

## ----mb_aiming_plot--------

#Aiming
Aiming_Plot <- ggplot(data = Aiming_data_summary, aes(x=eye_condition, y=mean_MT, fill=eye_condition)) +
  geom_bar(stat = "identity", width=0.5, color = "black", fill = "white") +
  coord_cartesian(ylim = c(0.5,1.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_MT + se, 
                    ymin = mean_MT - se)) +
  geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "mean MT (s)") +
  theme(legend.position = "none")

#show(Aiming_Plot)

ggsave("Aiming_Plot", dpi = 800, height = 4, width = 6)

Aiming_Plot_no_bar <- ggplot(data = Aiming_data_summary, aes(x=eye_condition, y=mean_MT, fill=eye_condition)) +
  coord_cartesian(ylim = c(0.5,1.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_errorbar(width = 0.2, position = pd, size = es, alpha = .8, color = "black",
                aes(ymax = mean_MT + se, 
                    ymin = mean_MT - se)) +
  geom_point(position = pd, size = ps, color = "black") + #need to work out how to include this without weird legend
  scale_linetype_manual(values = c(1,2)) +
  scale_fill_grey(start = .05, end = .5) +
  scale_color_grey(start = .05, end = .5) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Visual Condition", y= "mean MT (s)") +
  theme(legend.position = "none")

#show(Aiming_Plot_no_bar)

setwd("C:/Users/wills/Documents/Cataract/Figures")
#setwd("~/OneDrive - University of Leeds/RESEARCH/Cataract/Will_Paper/Figures")
#setwd("C:/Users/fbsrc/OD/RESEARCH/Cataract/Will_Paper/Figures")
ggsave("Aiming_No_Bar.png", dpi = 800, height = 4, width = 6)

>>>>>>> ce420acba330a868d71aa226ce2d895b31295bc2
