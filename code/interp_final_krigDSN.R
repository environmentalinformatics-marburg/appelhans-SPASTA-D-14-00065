library(automap)
library(raster)

source("set_wd.R")

load("data/ta.pred.complete.rda")
load("data/exp_var_stck_sp.rda")

mthd <- "krig"

ta.pred.splt <- split(ta.pred, ta.pred$datetime)

for (j in seq(ta.pred.splt)) {

  train <- ta.pred.splt[[j]]
  train$ndvi_tr <- sinpi(train$ndvi)
  newdat <- exp_var_stck_sp[[j]]
  newdat$ndvi_tr <- sinpi(newdat$ndvi)

  ### convert to psatial object
  coordinates(train) <- ~x+y
  proj4string(train) <- CRS(proj4string(newdat))

  cat("\n========================================================\n",
      "\n", "krigDSN year ", unique(train@data$year),
      " month ", unique(train@data$month), ": run ", j, " of ",
      length(ta.pred.splt), "\n",
      "\n========================================================", "\n\n",
      sep = "")

  fmla <- as.formula(paste("resp ~ dem + svf + ndvi_tr"))

  krig_res <- autoKrige(fmla, input_data = train, new_data = newdat)

  pred <- raster(krig_res[[1]])

  nm <- paste("ki_", "Ta_200", "_", "krigDSN", "_interp_",
              unique(ta.pred.splt[[j]]$datetime),
              "_utm37s_wgs84", ".tif", sep = "")

  out.dir <- paste("interp_maps", "krigDSN", sep = "/")
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

  out.nm <- paste(out.dir, nm, sep = "/")

  writeRaster(pred, out.nm, overwrite = TRUE)

  save(krig_res, file = paste("interp_maps/krigDSN/", mthd,
                              "_interp_final_krigDSN_model_",
                              unique(ta.pred.splt[[j]]$datetime), ".rda",
                              sep = ""))

}
