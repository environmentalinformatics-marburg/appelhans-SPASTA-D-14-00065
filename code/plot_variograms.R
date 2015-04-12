library(automap)
library(latticeExtra)
library(reshape2)

source("set_wd.R")

fls <- list.files("interp_maps",
                  pattern = glob2rx("cubist_krig_err*.rda"),
                  full.names = TRUE)

plts <- lapply(seq(fls), function(i) {

  dat <- seq.Date(as.Date("2014-01-01"), as.Date("2014-12-31"), "months")
  Sys.setlocale("LC_TIME", "C")
  labs <- months(dat, abb = TRUE)

  load(fls[i])

  mod <- as.character(krig_res$var_model$model[2])

  exp <- krig_res$exp_var
  fit <- krig_res$var_model

  fitted <- data.frame(dist = seq(0.01, max(exp$dist), length = 20000))
  fitted$gamma <- variogramLine(fit, dist_vector = fitted$dist)$gamma

  empirical <- melt(exp, id.vars = "dist", measure.vars = "gamma")
  modeled <- melt(fitted, id.vars = "dist", measure.vars = "gamma")

  tmp <- xyplot(value ~ dist, data = empirical, ylim = c(-0.5, 7.5),
                xlim = c(-600, 21000), type = c("g", "p"),
                ylab = "Semivariance", xlab = "Distance [m]",
                panel = function(...) {
                  panel.xyplot(...)
                  panel.text(x = 20000, y = 6,
                             labels = months(dat, abb = TRUE)[i],
                             adj = c(1, 0))
                  }) +
    as.layer(xyplot(gamma ~ dist, data = fitted, type = "l",
                    col = "black"))

  return(tmp)
})

pdf("graphs/variograms.pdf", paper = "a4")
latticeCombineGrid(plts, layout = c(3, 4))
dev.off()
