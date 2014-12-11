library(dplyr)
library(latticeExtra)
library(gridExtra)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

### ML resuslts
stats <- read.csv("results_server/ki_ta200_interp_perf_sats_ml.csv")
stats_krig <- read.csv("results_server/ki_ta200_interp_perf_sats_krig.csv")
stats_krig$method <- "kriging"
stats_krig <- stats_krig[, c(7, 1:6)]

#stats <- rbind(stats, stats_krig)

std <- function(x) sd(x)/sqrt(length(x)) ## standard error
### RMSE
ave_rmse_ml <- stats %>%
  group_by(method) %>%
  summarize(rmse = mean(RMSE),
            rmse.se = std(RMSE),
            rmse.min = min(RMSE),
            rmse.max = max(RMSE))

ave_rmse_krig <- stats_krig %>%
  group_by(method) %>%
  summarize(rmse = mean(RMSE),
            rmse.se = std(RMSE),
            rmse.min = min(RMSE),
            rmse.max = max(RMSE))

ave_rmse_ml <- ave_rmse_ml[order(ave_rmse_ml$rmse), ]
ave_rmse_ml$rank <- 1:nrow(ave_rmse_ml)

stats <- merge(stats, ave_rmse_ml[, -2], all.x = TRUE)

stats$method <- factor(stats$method, 
                       levels = ave_rmse_ml$method[order(ave_rmse_ml$rmse)])

dp_rmse <- dotplot(stats$rank ~ stats$RMSE, groups = stats$iteration,
                   as.table = FALSE, type = "l", col = "grey30", alpha = 0.3,
                   scales = list(y = list(labels = levels(stats$method)),
                                 tck = c(1, 1)),
                   ylab = "Method", xlab = "RMSE", asp = 1,
                   xlim = c(0.35, 2.25),
                   xscale.components = xscale.components.subticks)
dp_rmse

# lwr <- ave_rmse_ml$rmse.min# - ave_rmse$rmse.se
# mid <- ave_rmse_ml$rmse
# upr <- ave_rmse_ml$rmse.max# + ave_rmse$rmse.se
# 
# sp_rmse <- segplot(ave_rmse_ml$rank ~ lwr + upr, draw.bands = FALSE, centers = mid,
#                    col = "black", asp = 1, cex = 1,
#                    xscale.components = xscale.components.subticks)
# #sp_rmse 

bw_rmse <- bwplot(stats$rank ~ stats$RMSE, 
                   col = "black", asp = 1, cex = 1,
                   xscale.components = xscale.components.subticks,
                  panel = function(x, y, ...) {
                    panel.bwplot(x, y, box.ratio = 0.3)
                    })

bw.theme <- trellis.par.get()
bw.theme$box.dot$pch <- "|"
bw.theme$box.rectangle$col <- "black"
bw.theme$box.rectangle$lwd <- 2
bw.theme$box.rectangle$fill <- "white"
bw.theme$box.rectangle$alpha <- 0.6
bw.theme$box.umbrella$lty <- 1
bw.theme$box.umbrella$col <- "black"
bw.theme$plot.symbol$col <- "black"
bw.theme$plot.symbol$pch <- "*"
bw.theme$plot.symbol$cex <- 2

bw_rmse <- update(bw_rmse, par.settings = bw.theme)

rmse_both <- dp_rmse + as.layer(bw_rmse)

lwr_krig <- ave_rmse_krig$rmse.min# - ave_rmse$rmse.se
mid_krig <- ave_rmse_krig$rmse
upr_krig <- ave_rmse_krig$rmse.max# + ave_rmse$rmse.se

sp_rmse_krig <- segplot(as.factor(ave_rmse_krig$method) ~ lwr_krig + upr_krig, 
                        draw.bands = FALSE, centers = mid_krig,
                        col = "black", asp = 1/10, cex = 1,
                        xlim = c(0.35, 2.25),
                        xscale.components = xscale.components.subticks)

bw_rmse_krig <- bwplot(as.factor(stats_krig$method) ~ stats_krig$RMSE, 
                  col = "black", asp = 1, cex = 1,
                  xscale.components = xscale.components.subticks,
                  panel = function(x, y, ...) {
                    panel.bwplot(x, y, box.ratio = 0.3)
                  })
bw_rmse_krig <- update(bw_rmse_krig, par.settings = bw.theme)

outLayout <- function(x, y) {
  update(c(x, y, x.same = TRUE,
           layout = c(1, 2)), 
         between = list(y = 0.3, x = 0.3),
         as.table = TRUE)
}

out_rmse <- resizePanels(Reduce(outLayout, list(rmse_both, bw_rmse_krig)), 
                    h = c(1, 1/10))

out_rmse$y.scales$labels <- list(levels(stats$method), "kriging")
print(out_rmse)

## t.test of means
# m1 <- subset(stats, method == "gbm")
# m2 <- subset(stats, method == "earth")
# # indsvmr <- sample(nrow(svmr), 222)
# # svmr <- svmr[indsvmr, ]
# t.test(m1$RMSE, m2$RMSE)



### Rsq
ave_rsq <- stats %>%
  group_by(method) %>%
  summarize(rsq = mean(Rsq))

ave_rsq <- ave_rsq[rev(order(ave_rsq$rsq)), ]
ave_rsq$rank <- 1:nrow(ave_rsq)

stats <- merge(stats, ave_rsq[, -2], all.x = TRUE)

stats$method <- factor(stats$method, 
                       levels = ave_rsq$method[rev(order(ave_rsq$rsq))],
                       order = TRUE)

dp_rsq <- dotplot(stats$rank ~ stats$Rsq, groups = stats$iteration,
                   as.table = FALSE, type = "l", col = "grey30", alpha = 0.3,
                   scales = list(y = list(labels = levels(stats$method)),
                                 tck = c(1, 1)),
                   ylab = "Method", xlab = "Rsq", asp = 1,
                   xscale.components = xscale.components.subticks)
dp_rsq

# lwr <- ave_rsq$rsq - ave_rsq$rsq.se
# mid <- ave_rsq$rsq
# upr <- ave_rsq$rsq + ave_rsq$rsq.se
# 
# sp_rsq <- segplot(ave_rsq$rank ~ lwr + upr, draw.bands = FALSE, centers = mid,
#                    col = "red2", asp = 1, cex = 1,
#                    xscale.components = xscale.components.subticks)
# #sp_rmse 

bw_rsq <- bwplot(stats$rank ~ stats$Rsq, 
                  col = "black", asp = 1, cex = 1,
                  xscale.components = xscale.components.subticks,
                  panel = function(x, y, ...) {
                    panel.bwplot(x, y, box.ratio = 0.3)
                  })

bw.theme <- trellis.par.get()
bw.theme$box.dot$pch <- "|"
bw.theme$box.rectangle$col <- "black"
bw.theme$box.rectangle$lwd <- 2
bw.theme$box.rectangle$fill <- "white"
bw.theme$box.rectangle$alpha <- 0.6
bw.theme$box.umbrella$lty <- 1
bw.theme$box.umbrella$col <- "black"
bw.theme$plot.symbol$col <- "black"
bw.theme$plot.symbol$pch <- "*"
bw.theme$plot.symbol$cex <- 2

bw_rsq <- update(bw_rsq, par.settings = bw.theme)

rsq_both <- dp_rsq + as.layer(bw_rsq)

sp_rsq_krig <- segplot(as.factor(ave_rsq_krig$method) ~ lwr_krig + upr_krig, 
                        draw.bands = FALSE, centers = mid_krig,
                        col = "black", asp = 1/10, cex = 1,
                        xlim = c(0.35, 2.25),
                        xscale.components = xscale.components.subticks)

bw_rsq_krig <- bwplot(as.factor(stats_krig$method) ~ stats_krig$Rsq, 
                       col = "black", asp = 1, cex = 1,
                       xscale.components = xscale.components.subticks,
                       panel = function(x, y, ...) {
                         panel.bwplot(x, y, box.ratio = 0.3)
                       })
bw_rsq_krig <- update(bw_rsq_krig, par.settings = bw.theme)

outLayout <- function(x, y) {
  update(c(x, y, x.same = TRUE,
           layout = c(1, 2)), 
         between = list(y = 0.3, x = 0.3),
         as.table = TRUE)
}

out_rsq <- resizePanels(Reduce(outLayout, list(rsq_both, bw_rsq_krig)), 
                    h = c(1, 1/10))

out_rsq$y.scales$labels <- list(levels(stats$method), "kriging")
print(out_rsq)

pdf(paste("stats_ml_run03_", Sys.Date(), ".pdf", sep = ""), 
    width = 15, height = 10)
grid.arrange(out_rmse, out_rsq, ncol = 2)
dev.off()
