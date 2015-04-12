library(dplyr)

krig_eval <- read.csv("krig_variable_selection/krig_variable_selection_overview_ndvi_transformed.csv",
                      stringsAsFactors = FALSE)

krig_eval_agg <- krig_eval %>%
  group_by(fmla) %>%
  summarise(min_err = mean(krig_min - obs_min),
            max_err = mean(krig_max - obs_max))

krig_eval_agg[order(krig_eval_agg$min_err), ]
