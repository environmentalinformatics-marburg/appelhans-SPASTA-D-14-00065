library(Rsenal)
library(latticeExtra)
library(dplyr)

path <- "~/tappelhans/uni/temp"
setwd(path)

fls_rslts <- list.files("results", pattern = glob2rx("ki_*.csv"),
                        full.names = TRUE, recursive = TRUE)

#fls_rslts <- fls_rslts[-c(8, 9)]

findMatch <- function(ptrn) {
  function(strng) grep(ptrn, strng)
}

### kriging results amalgamation
indx_ln <- do.call("c", lapply(c("earth", "glm", "pcr", "pls", "svmLinear"),
                               function(i) {
                                 f <- findMatch(i)
                                 f(fls_rslts)[1]
                               }))
indx_krigD <- grep(glob2rx("*krigD_*"), fls_rslts)
indx_krigDN <- grep(glob2rx("*krigDN_*"), fls_rslts)
indx_krigDSN <- grep(glob2rx("*krigDSN_*"), fls_rslts)
indx_krig <- grep(glob2rx("*krig_*"), fls_rslts)

# fls_rslts_krigD <- fls_rslts[indx_krigD]
# fls_rslts_krigDN <- fls_rslts[indx_krigDN]
# fls_rslts_krigDSN <- fls_rslts[indx_krigDSN]

fls_rslts_krig <- fls_rslts[c(indx_krigD, indx_krigDN, indx_krigDSN)]

fls_rslts <- fls_rslts[-c(indx_krig, indx_krigD, indx_krigDN,
                          indx_krigDSN, indx_ln)]

rslts_krig <- do.call("rbind", lapply(fls_rslts_krig, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotId", "StationId", "Ta_obs",
                  "Ta_pred", "method", "iteration", "method")
  tmp <- tmp[, c(1:8)]
  return(tmp)
}))
rslts_krig <- rslts_krig[order(rslts_krig$iteration), ]

### rfe results
rslts_rfe <- do.call("rbind", lapply(fls_rslts, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotId", "StationId", "Ta_obs",
                   "Ta_pred_varsel", "Ta_pred", "method", "iteration")
  tmp <- tmp[, c(1:5, 7:9)]
  return(tmp)
}))

rslts <- rbind(rslts_rfe, rslts_krig)

stats_rfe <- rslts %>%
  group_by(method, iteration) %>%
  summarise(ME = mean(Ta_pred - Ta_obs, na.rm = TRUE),
            MAE =mean(abs(Ta_pred - Ta_obs), na.rm = TRUE),
            RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)),
            R = cor(Ta_pred, Ta_obs, use = "complete.obs"),
            Rsq = R * R)

write.csv(stats_rfe, "results/res_ki_ta200_interp_perf_sats_rfe_cv.csv",
          row.names = FALSE)


### variable selection results
rslts_varsel <- do.call("rbind", lapply(fls_rslts, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotId", "StationId", "Ta_obs",
                  "Ta_pred", "Ta_pred_rfe", "method", "iteration")
  tmp <- tmp[, c(1:6, 8:9)]
  return(tmp)
}))

rslts <- rbind(rslts_varsel, rslts_krig)

stats_varsel <- rslts %>%
  group_by(method, iteration) %>%
  summarise(ME = mean(Ta_pred - Ta_obs, na.rm = TRUE),
            MAE =mean(abs(Ta_pred - Ta_obs), na.rm = TRUE),
            RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)),
            R = cor(Ta_pred, Ta_obs, use = "complete.obs"),
            Rsq = R * R)

write.csv(stats_varsel, "results/res_ki_ta200_interp_perf_sats_varsel_cv.csv",
          row.names = FALSE)

# ### krgiging stats
# rslts_krig <- do.call("rbind", lapply(fls_rslts_krig, function(i) {
#   tmp <- read.csv(i, stringsAsFactors = FALSE)
#   names(tmp) <- c("iteration", "year", "month", "PlotId", "StationId",
#                   "ta.obs", "ta.pred", "elevation", "theo.model")
#   return(tmp)
# }))
#
# stats_krig <- rslts_krig %>%
#   group_by(iteration) %>%
#   summarise(ME = mean(ta.pred - ta.obs, na.rm = TRUE),
#             MAE =mean(abs(ta.pred - ta.obs), na.rm = TRUE),
#             RMSE = sqrt(mean((ta.pred - ta.obs)^2, na.rm = TRUE)),
#             R = cor(ta.pred, ta.obs, use = "complete.obs"),
#             Rsq = R * R)
#
# write.csv(stats_krig, "results_server/ki_ta200_interp_perf_sats_krig.csv", row.names = FALSE)
