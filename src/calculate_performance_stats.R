library(dplyr)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

fls_rslts <- list.files("results_server", pattern = glob2rx("ki_*.csv"),
                        full.names = TRUE, recursive = TRUE)

fls_rslts <- fls_rslts[-c(8, 9)]

indx_krig <- grep(glob2rx("*krig*"), fls_rslts)
fls_rslts_krig <- fls_rslts[indx_krig]

fls_rslts <- fls_rslts[-indx_krig]

### ML stats
rslts <- do.call("rbind", lapply(fls_rslts, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotId", "StationId", "ta.obs", 
                   "ta.pred", "elevation", "method", "iteration")
  return(tmp)
}))

stats <- rslts %>%
  group_by(method, iteration) %>%
  summarise(ME = mean(ta.pred - ta.obs, na.rm = TRUE),
            MAE =mean(abs(ta.pred - ta.obs), na.rm = TRUE),
            RMSE = sqrt(mean((ta.pred - ta.obs)^2, na.rm = TRUE)),
            R = cor(ta.pred, ta.obs, use = "complete.obs"),
            Rsq = R * R)

write.csv(stats, "results_server/ki_ta200_interp_perf_sats_ml.csv", row.names = FALSE)

### krgiging stats
rslts_krig <- do.call("rbind", lapply(fls_rslts_krig, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("iteration", "year", "month", "PlotId", "StationId", 
                  "ta.obs", "ta.pred", "elevation", "theo.model")
  return(tmp)
}))

stats_krig <- rslts_krig %>%
  group_by(iteration) %>%
  summarise(ME = mean(ta.pred - ta.obs, na.rm = TRUE),
            MAE =mean(abs(ta.pred - ta.obs), na.rm = TRUE),
            RMSE = sqrt(mean((ta.pred - ta.obs)^2, na.rm = TRUE)),
            R = cor(ta.pred, ta.obs, use = "complete.obs"),
            Rsq = R * R)

write.csv(stats_krig, "results_server/ki_ta200_interp_perf_sats_krig.csv", row.names = FALSE)
