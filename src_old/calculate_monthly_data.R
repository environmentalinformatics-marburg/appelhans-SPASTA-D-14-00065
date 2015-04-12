library(dplyr)

setwd("/media/memory01/data/casestudies/kilimanjaro_ta200_interp_ephraim")

ta.past <- read.csv("/media/memory01/ei_ki_pastprocessing/processing/plots/ki/Ta_200_0310_monthly_mean.dat",
                    stringsAsFactors = FALSE)
ta.ki <- read.csv("/media/memory01/ei_data_kilimanjaro/processing/plots/ki/Ta_200_0310_monthly_mean.dat",
                  stringsAsFactors = FALSE)

data <- as.data.frame(rbind(ta.past, ta.ki))

dat <- data %>%
  group_by(PlotId, StationId, Yearmon) %>%
  summarise(Ta_200 = mean(Ta_200, na.rm = TRUE),
            Ta_200_min = mean(Ta_200_min, na.rm = TRUE),
            Ta_200_max = mean(Ta_200_max, na.rm = TRUE))

dat <- as.data.frame(dat)
dat <- dat[complete.cases(dat), ]

write.csv(dat, "sp1_monthly_Ta_200_2011_2014.csv", row.names = FALSE)
