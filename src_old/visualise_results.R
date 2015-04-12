library(latticeExtra)
library(ggplot2)
library(dplyr)
library(reshape2)

### global settings
# number of best models (according to RMSE)
n.best.mods <- 4
lout <- c(2, 2)

### read data
fls.ml <- list.files("results", pattern = glob2rx("ki_*_yr.csv"), 
                     full.names = TRUE, recursive = TRUE)

res.ml <- lapply(fls.ml, function(i) {
  tmp <- read.csv(i, header = FALSE, stringsAsFactors = FALSE)
  names(tmp) <- c("year", "month", "PlotID", "StationID", "Ta_obs",
                  "Ta_pred", "elevation", "method", "iteration")
  return(tmp)
})

fls.krig <- list.files("results/krig", pattern = glob2rx("ki_*.csv"), 
                       full.names = TRUE, recursive = TRUE)

res.krig <- do.call("rbind", lapply(fls.krig, function(i) {
  tmp <- read.csv(i, header = FALSE, stringsAsFactors = FALSE)
  names(tmp) <- c("iteration", "year", "month", "PlotID", "StationID", 
                  "Ta_obs", "Ta_pred", "elevation", "nstations", "model")
  return(tmp)
}))

res.krig$method <- "kriging"

res <- res.ml
res[[length(res) + 1]] <- res.krig

### calcaulate RMSE
rmse <- unlist(lapply(res, function(i) {
  sqrt(mean((i$Ta_pred - i$Ta_obs)^2, na.rm = TRUE))
}))

res.sort <- res[order(rmse)]

nms <- unlist(lapply(res, function(i) {
  unique(i$method)
}))
nms.sort <- nms[order(rmse)]

### extract n.best.mods models
res.best <- res.sort[1:n.best.mods]

### density scatterplots
scp.list <- lapply(seq(res.best), function(i) {
  panel.name <- toupper(unique(res.best[[i]]$method))
  RMSE <- paste("RMSE =", round(rmse[order(rmse)][i], 3))
  scatter.lattice <- xyplot(res.best[[i]]$Ta_pred ~ res.best[[i]]$Ta_obs, 
                            aspect = "iso",
                            ylim = c(-5, 30), xlim = c(-5, 30),
                            ylab = "Predicted [°C]", 
                            xlab = "Observed [°C]",
                            panel = function(x, y, ...) {
                              panel.smoothScatter(x, y, nbin = 500, 
                                                  raster = TRUE, 
                                                  range.x = list(c(-10, 35),
                                                                 c(-10, 35)),
                                                  ...)
                              lm1 <- lm(y ~ x)
                              lm1sum <- summary(lm1)
                              r2 <- lm1sum$adj.r.squared
                              panel.text(labels = 
                                           bquote(italic(R)^2 == 
                                                    .(format(r2, 
                                                             digits = 3))),
                                         x = 2.5, y = 26, cex = 1)
                              panel.text(labels = panel.name,
                                         x = 2.5, y = 28, cex = 1)
                              panel.text(labels = RMSE,
                                         x = 2.5, y = 24, cex = 1)
                              panel.smoother(x, y, method = "lm", 
                                             col = "red", 
                                             col.se = "red",
                                             alpha.se = 0.3, ...)
                              panel.abline(a = 0, b = 1, 
                                           col = "grey20", lty = 2, ...)
                            },
                            xscale.components = xscale.components.subticks,
                            yscale.components = yscale.components.subticks,
                            as.table = TRUE)
  
  return(scatter.lattice)
  
})

outLayout <- function(x, y) {
  update(c(x, y, 
           layout = lout), 
         between = list(y = 0.3, x = 0.3))
}

out <- Reduce(outLayout, scp.list)

print(out)

### boxplots
##1. stats per method
res.sort <- res[order(rmse)]
stats.iter <- do.call("rbind", lapply(seq(res.sort), function(i) {
  tmp <- res.sort[[i]] %>%
    group_by(iteration) %>%
    summarise(RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)),
              ME = mean(Ta_pred - Ta_obs, na.rm = TRUE),
              MAE = mean(abs(Ta_pred - Ta_obs), na.rm = TRUE),
              Rsq = cor(Ta_pred, Ta_obs, use = "complete.obs")^2)
  tmp2 <- melt(tmp, id.vars = "iteration")
  tmp2$method <- toupper(unique(res.sort[[i]]$method))
  return(tmp2)
}))

stats.iter$method <- factor(stats.iter$method, 
                            levels = unique(stats.iter$method))

bw.iter <- ggplot(stats.iter, aes(x = method, y = value))

bw.iter <- bw.iter + 
  geom_boxplot(fill = "grey80") +
  theme_bw() +
  facet_grid(variable ~ ., scales = "free_y") +
  ylab("Value") + xlab("Method")

print(bw.iter)

##2. rmse per habitat for n best models
stats.habitat <- do.call("rbind", lapply(seq(res.best), function(i) {
  res.best[[i]]$hab <- substr(res.best[[i]]$PlotID, 1, 3)
  tmp <- res.best[[i]] %>%
    group_by(hab) %>%
    summarise(RMSE = sqrt(mean((Ta_pred - Ta_obs)^2, na.rm = TRUE)))
  tmp2 <- melt(tmp, id.vars = "hab")
  tmp2$method <- toupper(unique(res.best[[i]]$method))
  return(tmp2)
}))

stats.habitat$method <- factor(stats.habitat$method, 
                               levels = unique(stats.habitat$method))

bw.hab <- ggplot(stats.habitat, aes(x = hab, y = value))

bw.hab <- bw.hab + 
  geom_boxplot(fill = "grey80") +
  theme_bw() +
  facet_grid(method ~ .) +
  ylab("Value") + xlab("Habitat")

print(bw.hab)

### variable importance
fls.varimp <- list.files("results", pattern = glob2rx("varimp_*_yr.csv"), 
                         full.names = TRUE, recursive = TRUE)

res.varimp <- lapply(nms.sort[!nms.sort == "kriging"], function(i) {
  path <- paste("results", i, sep = "/")
  file <- list.files(path, pattern = glob2rx("varimp_*_yr.csv"), 
                     full.names = TRUE, recursive = TRUE)
  tmp <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
  names(tmp) <- c("method", "variable", "importance", "score", "iteration")
  return(tmp)
})

res.varimp.nbest <- res.varimp[1:n.best.mods]

stats.varimp <- lapply(seq(res.varimp.nbest), function(i) {
  tmp <- res.varimp.nbest[[i]] %>%
    group_by(variable) %>%
    summarise(median = median(score, na.rm = TRUE))
  tmp$method <- toupper(unique(res.varimp.nbest[[i]]$method))
  tmp <- tmp[order(tmp$median, decreasing = TRUE), ]
  tmp$variable <- factor(toupper(tmp$variable),
                         levels = toupper(tmp$variable))
  return(tmp)
})

dot.list <- lapply(seq(stats.varimp), function(i) {
  p <- dotplot(variable ~ median, data = stats.varimp[[i]],
               xlab = "Median score", ylab = "Variable",
               as.table = TRUE,
               panel = function(...) {
                 panel.dotplot(...)
                 panel.text(labels = unique(stats.varimp[[i]]$method),
                            x = 9, y = 9, cex = 1)
               })
  return(p)
})

outLayout <- function(x, y) {
  update(c(x, y, 
           layout = c(1, n.best.mods)), 
         between = list(y = 0.3, x = 0.3))
}

dot.out <- Reduce(outLayout, dot.list)
print(dot.out)
