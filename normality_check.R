library("dplyr")
library("ggpubr")

############ experiment 1 ###############



############ experiment 2 ###############


exp2_change_data <- read.csv("C:/Users/wills/Documents/Cataract/Data/exp2_ALL_CHANGE.csv")

VA_den <- ggdensity(exp2_change_data$VA,
          main = "Density plot of VA",
          xlab = "VA")

VA_shapiro <- shapiro.test(exp2_change_data$VA)

CS_den <- ggdensity(exp2_change_data$CS,
                    main = "Density plot of CS",
                    xlab = "CS")
CS_shapiro <- shapiro.test(exp2_change_data$CS)

stereo_den <- ggdensity(exp2_change_data$stereoacuity,
                    main = "Density plot of stereoacuity",
                    xlab = "Stereoacuity")

stereo_shapiro <- shapiro.test(exp2_change_data$stereoacuity)
