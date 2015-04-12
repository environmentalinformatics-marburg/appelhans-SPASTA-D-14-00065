library(Rsenal)
library(latticeExtra)
library(dplyr)

path <- "~/tappelhans/uni/temp"
setwd(path)

std <- function(x) sd(x)/sqrt(length(x)) ## standard error

### rfe resuslts
stats_rfe <- read.csv("results/res_ki_ta200_interp_perf_sats_rfe_cv.csv")

### RMSE
ave_rmse_rfe <- stats_rfe %>%
  group_by(method) %>%
  summarize(rmse = mean(RMSE),
            rmse.se = std(RMSE),
            rmse.min = min(RMSE),
            rmse.max = max(RMSE))

ave_rmse_rfe <- ave_rmse_rfe[order(ave_rmse_rfe$rmse), ]
ave_rmse_rfe$rank <- 1:nrow(ave_rmse_rfe)

stats_rfe <- merge(stats_rfe, ave_rmse_rfe[, -2], all.x = TRUE)

stats_rfe$method <- factor(stats_rfe$method,
                       levels = ave_rmse_rfe$method[order(ave_rmse_rfe$rmse)])

clrs <- colorRampPalette(brewer.pal(10, "Paired"))(10)
line.settings <- list(superpose.line = list(col = clrs, lwd = 1.5))

dp_rmse_rfe <- dotplot(stats_rfe$rank ~ stats_rfe$RMSE, groups = stats_rfe$iteration,
                       as.table = FALSE, type = c("g", "l"),
                       col = clrs, alpha = 0.7,
                       par.settings = line.settings,
                       scales = list(y = list(labels = levels(stats_rfe$method)),
                                     tck = c(1, 1)),
                       auto.key = list(columns = 5,
                                       lines = TRUE,
                                       points = FALSE,
                                       col = "black",
                                       title = "Model iteration",
                                       cex.title = 1),
                       ylab = "Method", xlab = "RMSE", asp = 1,
                       xlim = c(0.25, 2.55), autoKey = TRUE,
                       xscale.components = xscale.components.subticks)
dp_rmse_rfe

pdf("graphs/method_performance_rfe.pdf", paper = "a4")
dp_rmse_rfe
dev.off()



### varsel results
stats_varsel <- read.csv("results/res_ki_ta200_interp_perf_sats_varsel_cv.csv")

### RMSE
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

dp_rmse_varsel <- dotplot(stats_varsel$rank ~ stats_varsel$RMSE, groups = stats_varsel$iteration,
                       as.table = FALSE, type = c("g", "l"),
                       col = clrs, alpha = 0.7,
                       par.settings = line.settings,
                       scales = list(y = list(labels = levels(stats_varsel$method)),
                                     tck = c(1, 1)),
                       auto.key = list(columns = 5,
                                       lines = TRUE,
                                       points = FALSE,
                                       col = "black",
                                       title = "Cross validation folds",
                                       cex.title = 1),
                       ylab = "Method", xlab = "RMSE", asp = 1,
                       xlim = c(0.25, 2.55), autoKey = TRUE,
                       xscale.components = xscale.components.subticks)
dp_rmse_varsel

# pdf("graphs/method_performance_varsel.pdf", paper = "a4")
# dp_rmse_varsel
# dev.off()


### boxplots
lwr <- ave_rmse_varsel$rmse.min# - ave_rmse$rmse.se
mid <- ave_rmse_varsel$rmse
upr <- ave_rmse_varsel$rmse.max# + ave_rmse$rmse.se

sp_rmse <- segplot(ave_rmse_varsel$rank ~ lwr + upr, draw.bands = FALSE, centers = mid,
                   col = "black", asp = 1, cex = 1,
                   xscale.components = xscale.components.subticks)
#sp_rmse

bw_rmse <- bwplot(stats_varsel$rank ~ stats_varsel$RMSE,
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

rmse_both <- dp_rmse_varsel + as.layer(bw_rmse)

pdf("graphs/method_performance_varsel_boxplot.pdf", paper = "a4")
print(rmse_both)
dev.off()




# lwr_krig <- ave_rmse_krig$rmse.min# - ave_rmse$rmse.se
# mid_krig <- ave_rmse_krig$rmse
# upr_krig <- ave_rmse_krig$rmse.max# + ave_rmse$rmse.se
#
# sp_rmse_krig <- segplot(as.factor(ave_rmse_krig$method) ~ lwr_krig + upr_krig,
#                         draw.bands = FALSE, centers = mid_krig,
#                         col = "black", asp = 1/10, cex = 1,
#                         xlim = c(0.35, 2.25),
#                         xscale.components = xscale.components.subticks)
#
# bw_rmse_krig <- bwplot(as.factor(stats_varsel_krig$method) ~ stats_varsel_krig$RMSE,
#                   col = "black", asp = 1, cex = 1,
#                   xscale.components = xscale.components.subticks,
#                   panel = function(x, y, ...) {
#                     panel.bwplot(x, y, box.ratio = 0.3)
#                   })
# bw_rmse_krig <- update(bw_rmse_krig, par.settings = bw.theme)
#
# outLayout <- function(x, y) {
#   update(c(x, y, x.same = TRUE,
#            layout = c(1, 2)),
#          between = list(y = 0.3, x = 0.3),
#          as.table = TRUE)
# }
#
# out_rmse <- resizePanels(Reduce(outLayout, list(rmse_both, bw_rmse_krig)),
#                     h = c(1, 1/10))
# #
# out_rmse$y.scales$labels <- list(levels(stats_rfe$method), "kriging")
# print(out_rmse)
#
# ## t.test of means
# # m1 <- subset(stats_rfe, method == "gbm")
# # m2 <- subset(stats_rfe, method == "earth")
# # # indsvmr <- sample(nrow(svmr), 222)
# # # svmr <- svmr[indsvmr, ]
# # t.test(m1$RMSE, m2$RMSE)
#
#
#
# ### Rsq
# ave_rsq <- stats_rfe %>%
#   group_by(method) %>%
#   summarize(rsq = mean(Rsq))
#
# ave_rsq <- ave_rsq[rev(order(ave_rsq$rsq)), ]
# ave_rsq$rank <- 1:nrow(ave_rsq)
#
# stats_rfe <- merge(stats_rfe, ave_rsq[, -2], all.x = TRUE)
#
# stats_rfe$method <- factor(stats_rfe$method,
#                        levels = ave_rsq$method[rev(order(ave_rsq$rsq))],
#                        order = TRUE)
#
# dp_rsq <- dotplot(stats_rfe$rank ~ stats_rfe$Rsq, groups = stats_rfe$iteration,
#                    as.table = FALSE, type = "l", col = "grey30", alpha = 0.3,
#                    scales = list(y = list(labels = levels(stats_rfe$method)),
#                                  tck = c(1, 1)),
#                    ylab = "Method", xlab = "Rsq", asp = 1,
#                    xscale.components = xscale.components.subticks)
# dp_rsq
#
# # lwr <- ave_rsq$rsq - ave_rsq$rsq.se
# # mid <- ave_rsq$rsq
# # upr <- ave_rsq$rsq + ave_rsq$rsq.se
# #
# # sp_rsq <- segplot(ave_rsq$rank ~ lwr + upr, draw.bands = FALSE, centers = mid,
# #                    col = "red2", asp = 1, cex = 1,
# #                    xscale.components = xscale.components.subticks)
# # #sp_rmse
#
# bw_rsq <- bwplot(stats_rfe$rank ~ stats_rfe$Rsq,
#                   col = "black", asp = 1, cex = 1,
#                   xscale.components = xscale.components.subticks,
#                   panel = function(x, y, ...) {
#                     panel.bwplot(x, y, box.ratio = 0.3)
#                   })
#
# bw.theme <- trellis.par.get()
# bw.theme$box.dot$pch <- "|"
# bw.theme$box.rectangle$col <- "black"
# bw.theme$box.rectangle$lwd <- 2
# bw.theme$box.rectangle$fill <- "white"
# bw.theme$box.rectangle$alpha <- 0.6
# bw.theme$box.umbrella$lty <- 1
# bw.theme$box.umbrella$col <- "black"
# bw.theme$plot.symbol$col <- "black"
# bw.theme$plot.symbol$pch <- "*"
# bw.theme$plot.symbol$cex <- 2
#
# bw_rsq <- update(bw_rsq, par.settings = bw.theme)
#
# rsq_both <- dp_rsq + as.layer(bw_rsq)
#
# sp_rsq_krig <- segplot(as.factor(ave_rsq_krig$method) ~ lwr_krig + upr_krig,
#                         draw.bands = FALSE, centers = mid_krig,
#                         col = "black", asp = 1/10, cex = 1,
#                         xlim = c(0.35, 2.25),
#                         xscale.components = xscale.components.subticks)
#
# bw_rsq_krig <- bwplot(as.factor(stats_rfe_krig$method) ~ stats_rfe_krig$Rsq,
#                        col = "black", asp = 1, cex = 1,
#                        xscale.components = xscale.components.subticks,
#                        panel = function(x, y, ...) {
#                          panel.bwplot(x, y, box.ratio = 0.3)
#                        })
# bw_rsq_krig <- update(bw_rsq_krig, par.settings = bw.theme)
#
# outLayout <- function(x, y) {
#   update(c(x, y, x.same = TRUE,
#            layout = c(1, 2)),
#          between = list(y = 0.3, x = 0.3),
#          as.table = TRUE)
# }
#
# out_rsq <- resizePanels(Reduce(outLayout, list(rsq_both, bw_rsq_krig)),
#                     h = c(1, 1/10))
#
# out_rsq$y.scales$labels <- list(levels(stats_rfe$method), "kriging")
# print(out_rsq)
#
# pdf(paste("stats_rfe_run03_", Sys.Date(), ".pdf", sep = ""),
#     width = 15, height = 10)
# grid.arrange(out_rmse, out_rsq, ncol = 2)
# dev.off()
