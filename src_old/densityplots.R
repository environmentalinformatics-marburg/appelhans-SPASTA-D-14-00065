library(dplyr)
library(latticeExtra)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

fls_rslts <- list.files("results_server", pattern = glob2rx("ki_*.csv"),
                        full.names = TRUE, recursive = TRUE)

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


### by iteration
rslts_iters <- rslts %>%
  group_by(iteration, method)

rslts_iters_top4 <- rslts_iters[rslts_iters$method == "avNNet" |
                                  rslts_iters$method == "cubist" |
                                  rslts_iters$method == "nnet" |
                                  rslts_iters$method == "rf", ]

for (i in seq(250)) {
  
  rslts_iters_sub <- subset(rslts_iters_top4, iteration == i)
  
  clrs_hcl <- function(n) {
    hcl(h = seq(360, 0, length.out = n), 
        c = 60, l = 60, fixup = TRUE)
  }
  
  clrs_spec <- colorRampPalette(brewer.pal(11, "Spectral"))
  
  dp_obs <- densityplot(~ta.obs, plot.points = FALSE, lty = 2, asp = 1,
                        data = rslts_iters_sub, col = "grey20", lwd = 1) 
  dp_pred <- densityplot(~ta.pred, plot.points = FALSE,
                         col = clrs_spec(4), lwd = 2, asp = 1,
                         data = rslts_iters_sub, groups = method,
                         auto.key = list(space = "right", 
                                         col = clrs_spec(4),
                                         lines = FALSE),
                         ylim = c(-0.002, 0.082))
  
  nm <- paste("density_top4_", i, sep = "")
  
  png(paste(nm, "png", sep = "."), height = 10, width = 12, units = "cm", res = 300)
  print(dp_pred + as.layer(dp_obs))
  dev.off()
}


#### by method
rslts_mthds <- rslts %>%
  group_by(method)

rslts_mthds_top4 <- rslts_mthds[rslts_mthds$method == "avNNet" |
                                  rslts_mthds$method == "cubist" |
                                  rslts_mthds$method == "nnet" |
                                  rslts_mthds$method == "rf", ]

dp_obs <- densityplot(~ ta.obs, plot.points = FALSE, lty = 2, asp = 1,
                      data = rslts_mthds_top4, col = "grey20", lwd = 1) 
dp_pred <- densityplot(~ ta.pred, plot.points = FALSE,
                       col = clrs_spec(4), lwd = 2, asp = 1,
                       data = rslts_mthds_top4, groups = method,
                       auto.key = list(space = "right", 
                                       col = clrs_spec(4),
                                       lines = FALSE),
                       ylim = c(-0.002, 0.082))

print(dp_pred + as.layer(dp_obs))
