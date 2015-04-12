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

# ### kriging results amalgamation
# indx_krig <- grep(glob2rx("*krig*"), fls_rslts)
# fls_rslts_krig <- fls_rslts[indx_krig]
# fls_rslts <- fls_rslts[-indx_krig]

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

# ### rfe results
# rslts_rfe <- do.call("rbind", lapply(fls_rslts, function(i) {
#   tmp <- read.csv(i, stringsAsFactors = FALSE)
#   names(tmp) <- c("year", "month", "PlotId", "StationId", "Ta_obs",
#                   "Ta_pred_varsel", "Ta_pred", "method", "iteration")
#   tmp <- tmp[, c(1:5, 7:9)]
#   return(tmp)
# }))
#
# rslts_rfe <- rbind(rslts_rfe, rslts_krig)
# rslts_rfe$lg <- substr(rslts_rfe$PlotId, 1, 3)
#
# lu_stats_rfe <- rslts_rfe %>%
#   group_by(method, lg) %>%
#   summarise(ME = mean(Ta_pred - Ta_obs, na.rm = TRUE),
#             MAE =mean(abs(Ta_pred - Ta_obs), na.rm = TRUE),
#             RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)),
#             R = cor(Ta_pred, Ta_obs, use = "complete.obs"),
#             Rsq = R * R,
#             N = length(Ta_obs))
#
# data("kili12")
#
# kili12$Habitat <- factor(gsub("hel", "hel*", kili12$Habitat),
#                          levels = gsub("hel", "hel*",
#                                        levels(kili12$Habitat)))
# lu_stats_rfe$lg <- gsub("hel", "hel*", lu_stats_rfe$lg)
#
# lu_stats_rfe$lg <- factor(lu_stats_rfe$lg,
#                           levels = levels(kili12$Habitat))
# #lu_stats_rfe <- lu_stats_rfe[lu_stats_rfe$N >= 20, ]
#
# ### RMSE
# ### rfe resuslts
# std <- function(x) sd(x)/sqrt(length(x)) ## standard error
# stats_rfe <- read.csv("results/res_ki_ta200_interp_perf_sats_rfe_cv.csv")
#
# ave_rmse_rfe <- stats_rfe %>%
#   group_by(method) %>%
#   summarize(rmse = mean(RMSE),
#             rmse.se = std(RMSE),
#             rmse.min = min(RMSE),
#             rmse.max = max(RMSE))
#
# ave_rmse_rfe <- ave_rmse_rfe[order(ave_rmse_rfe$rmse), ]
# ave_rmse_rfe$rank <- 1:nrow(ave_rmse_rfe)
#
# stats_rfe <- merge(stats_rfe, ave_rmse_rfe[, -2], all.x = TRUE)
#
# stats_rfe$method <- factor(stats_rfe$method,
#                            levels = ave_rmse_rfe$method[order(ave_rmse_rfe$rmse)])
#
# lims <- c(floor(min(lu_stats_rfe$RMSE)),
#           ceiling(max(lu_stats_rfe$RMSE)))
# xpos_txt <- lims[2] - 0.15
# ypos_txt <- length(unique(lu_stats_rfe$lg))
#
# dp <- lapply(levels(stats_rfe$method), function(i) {
#   tmp <- lu_stats_rfe[lu_stats_rfe$method == i, ]
#   dotplot(tmp$lg ~ tmp$RMSE, xlim = lims, asp = 1,
#           col = "grey20", type = c("g", "p", "h"),
#           xlab = "RMSE", ylab = "Habitat",
#           xscale.components = xscale.components.subticks,
#           panel = function(...) {
#             panel.dotplot(...)
#             panel.text(x = xpos_txt, y = 1, labels = i,
#                        adj = c(1, 0))
#           })
# })
#
# pdf("graphs/rmse_by_habitat_rfe.pdf",
#     width = 17 / 2.54, height = 27 / 2.54)
# latticeCombineGrid(dp, layout = c(3, 6), x.same = TRUE, y.same = TRUE)
# dev.off()
#


### variable selection results
rslts_varsel <- do.call("rbind", lapply(fls_rslts, function(i) {
  tmp <- read.csv(i, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotId", "StationId", "Ta_obs",
                  "Ta_pred", "Ta_pred_rfe", "method", "iteration")
  tmp <- tmp[, c(1:6, 8:9)]
  return(tmp)
}))

rslts_varsel <- rbind(rslts_varsel, rslts_krig)
rslts_varsel$lg <- substr(rslts_varsel$PlotId, 1, 3)

lu_stats_varsel <- rslts_varsel %>%
  group_by(method, lg) %>%
  summarise(ME = mean(Ta_pred - Ta_obs, na.rm = TRUE),
            MAE =mean(abs(Ta_pred - Ta_obs), na.rm = TRUE),
            RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)),
            R = cor(Ta_pred, Ta_obs, use = "complete.obs"),
            Rsq = R * R,
            N = length(Ta_obs))

data("kili12")

# kili12$Habitat <- factor(gsub("hel", "hel*", kili12$Habitat),
#                          levels = gsub("hel", "hel*",
#                                        levels(kili12$Habitat)))
# lu_stats_varsel$lg <- gsub("hel", "hel*", lu_stats_varsel$lg)

lu_stats_varsel$lg <- factor(lu_stats_varsel$lg,
                             levels = levels(kili12$Habitat))
### RMSE
### rfe resuslts
std <- function(x) sd(x)/sqrt(length(x)) ## standard error
stats_varsel <- read.csv("results/res_ki_ta200_interp_perf_sats_varsel_cv.csv")

ave_rmse_varsel <- stats_varsel %>%
  group_by(method) %>%
  summarize(rmse = mean(RMSE),
            rmse.se = std(RMSE),
            rmse.min = min(RMSE),
            rmse.max = max(RMSE))

ave_rmse_varsel <- ave_rmse_varsel[order(ave_rmse_varsel$rmse), ]
ave_rmse_varsel$rank <- 1:nrow(ave_rmse_varsel)

stats_varsel <- merge(stats_varsel, ave_rmse_varsel[, -2], all.x = TRUE)

stats_varsel$method <- factor(stats_varsel$method,
                           levels = ave_rmse_varsel$method[order(ave_rmse_varsel$rmse)])

clrs <- colorRampPalette(brewer.pal(10, "Paired"))(10)
line.settings <- list(superpose.line = list(col = clrs, lwd = 1.5))

lims <- c(floor(min(lu_stats_varsel$RMSE)),
          ceiling(max(lu_stats_varsel$RMSE)))
xpos_txt <- lims[2] - 0.15
ypos_txt <- length(unique(lu_stats_varsel$lg))

dp <- lapply(rev(levels(stats_varsel$method)), function(i) {
  tmp <- lu_stats_varsel[lu_stats_varsel$method == i, ]
  dotplot(tmp$lg ~ tmp$RMSE, xlim = lims, asp = 1,
          col = "grey20", type = c("g", "p", "h"),
          xlab = "RMSE", ylab = "Habitat",
          xscale.components = xscale.components.subticks,
          panel = function(...) {
            panel.dotplot(...)
            panel.text(x = xpos_txt, y = 1, labels = i,
                       adj = c(1, 0))
          })
})


legend_df <- data.frame(c("hel", "fpd", "fpo", "foc", "fod", "flm"),
                        c("Helychrysum", "Podocarpus forest dist.",
                          "Podocarpus forest", "Ocotea forest",
                          "Ocotea forest disturbed", "Lower montane forest"),
                        c("gra", "hom", "cof", "sav", "mai", ""),
                        c("Grassland", "Chagga homegarden",
                          "Coffee plantation", "Savanna", "Maize field", ""))

legend <- grid.table(legend_df, show.colnames = FALSE, show.rownames = FALSE,
                     col.just = "right", core.just = "left", equal.width = FALSE,
                     h.odd.alpha = 0, h.even.alpha = 0, padding.h = unit(10, "mm"),
                     v.odd.alpha = 0, v.even.alpha = 0)

pdf("graphs/rmse_habitat.pdf", onefile = FALSE,
    width = 17 / 2.54, height = 27 / 2.54)
grid.newpage()
latticeCombineGrid(dp, layout = c(4, 5), x.same = TRUE, y.same = TRUE,
                   as.table = TRUE)
downViewport(trellis.vpname("figure"))

vp1 <- viewport(x = 1, y = 0,
                width = 0.745, height = 0.19,
                just = c("right", "bottom"),
                name = "legend_vp")
pushViewport(vp1)

grid.table(legend_df, show.colnames = FALSE, show.rownames = FALSE,
           col.just = "right", core.just = "left", equal.width = FALSE,
           h.odd.alpha = 0, h.even.alpha = 0,
           v.odd.alpha = 0, v.even.alpha = 0,
           padding.h = unit(4, "mm"),
           padding.v = unit(2, "mm"),
           gp = gpar(fontsize = 9))

dev.off()

