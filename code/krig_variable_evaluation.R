library(automap)
library(raster)

source("set_wd.R")

scaled <- FALSE

if(!scaled) {
  load("data/ta.pred.complete.unscld.rda")
  load("data/exp_var_stck_sp_unscld.rda")
} else {
  load("data/ta.pred.complete.rda")
  load("data/exp_var_stck_sp.rda")
}

mthd <- "krig"

ta.pred.splt <- split(ta.pred, ta.pred$datetime)


set.seed(144)
ind <- sample(seq(ta.pred.splt), 5)

res <- do.call("rbind", lapply(ind, function(j) {

  k <- which(ind == j)

  cat("EVALUATING RUN ", k, " OF", length(ind))

  train <- ta.pred.splt[[j]]
  train$ndvi_tr <- sinpi(train$ndvi) #* 10000
  newdat <- exp_var_stck_sp[[j]]
  newdat$ndvi_tr <- sinpi(newdat$ndvi) #* 10000

  #newdat@data$ndvi <- newdat@data$ndvi * 10

  ### convert to psatial object
  coordinates(train) <- ~x+y
  proj4string(train) <- CRS(proj4string(newdat))

  indep_vars <- c("slp", "svf", "asp", "ndvi_tr")
  combo_grd <- expand.grid(lapply(seq_along(indep_vars), c, 0))
  names(combo_grd) <- c("slp", "svf", "asp", "ndvi_tr")

  out <- do.call("rbind", lapply(seq(nrow(combo_grd)), function(i) {

    if (sum(combo_grd[i, ]) == 0) {
      fmla <- as.formula("resp ~ dem")
    } else {
      fmla <- as.formula(paste("resp ~ dem + ",
                               paste(indep_vars[combo_grd[i, ] != 0],
                                     collapse = " + ")))
    }

    cat("\n\nevaluating ", as.character(fmla)[3], "\n\n")

    krig_res <- autoKrige(fmla, input_data = train, new_data = newdat)

    pred <- raster(krig_res[[1]])
    print(range(pred[]))

    nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_",
                unique(ta.pred.splt[[j]]$datetime),
                "_dem_", paste(indep_vars[combo_grd[i, ] != 0],
                               collapse = "_"),
                "_utm37s_wgs84", ".tif", sep = "")

    out.dir <- "krig_variable_selection"
    dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

    out.nm <- paste(out.dir, nm, sep = "/")

    writeRaster(pred, out.nm, overwrite = TRUE)

    return(data.frame(date = unique(ta.pred.splt[[j]]$datetime),
                      fmla = paste("resp ~", as.character(fmla)[3]),
                      krig_min = min(pred[]),
                      obs_min = min(train$resp),
                      krig_max = max(pred[]),
                      obs_max = max(train$resp),
                      plot_min = train$plotID[train$resp %in% min(train$resp)],
                      plot_max = train$plotID[train$resp %in% max(train$resp)]))

  }))

  return(out)

}))


write.csv(res,
          "krig_variable_selection/krig_variable_selection_overview_unscld_ndvi_transformed.csv",
          row.names = FALSE)
