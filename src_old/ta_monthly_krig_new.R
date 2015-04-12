library(automap)
library(raster)

source("global_prep.R")

### split relevant data to create yearly subsets (yss)
plots.ta.monthly.rug.yss <- split(plots.ta.monthly.rug, 
                                  plots.ta.monthly.rug@data$year)

ndvi.stck.cr.sp <- as(ndvi.stck.cr, "SpatialPixelsDataFrame")
exp.var.stck.cr.sp <- as(exp.var.stck.cr, "SpatialPixelsDataFrame")

for (b in seq(plots.ta.monthly.rug.yss)) {
  
  ### split relevant data to create monthly subsets (mss)
  plots.ta.monthly.rug.yss.mss <- split(plots.ta.monthly.rug.yss[[b]], 
                                        plots.ta.monthly.rug.yss[[b]]@data$Month)
  
  
  ## kriging ----------------------------------------------------------------
  for (m in 1:12) {

    cat("\n", "kriging year", unique(plots.ta.monthly.rug.yss[[b]]@data$year),
        "month", unique(plots.ta.monthly.rug.yss.mss[[m]]@data$Month), "\n")
    
    ta.complete.d <- plots.ta.monthly.rug.yss.mss[[m]]
    
    # identify possible duplicated rug and rad logger on same plot (remove rad)
    if (any(duplicated(ta.complete.d@data$PlotId))) {
      indx <- c(which(duplicated(ta.complete.d@data$PlotId)),
                which(duplicated(ta.complete.d@data$PlotId)) - 1)
      indx.rad <- which(ta.complete.d@data$StationId[indx] == "000rad")
      ta.complete <- ta.complete.d[-indx[indx.rad], ]
    } else {
      ta.complete <- ta.complete.d
    }

    ndvi <- ndvi.stck.cr.sp[m]
    
    for (i in 1:15) {
      set.seed(i)
      cat(" ", "interpolating run", i, sep = " ")
      s <- split(ta.complete, substr(ta.complete@data$PlotId, 1, 3))

      ind <- unlist(lapply(seq(s), function(l) {
        if (nrow(s[[l]]) > 1) {
          v <- rep(TRUE, nrow(s[[l]]))
          v[sample(seq(v), 1)] <- FALSE
          return(v)
        } else {
          if (nrow(s[[l]]) == 1) return(TRUE)
        }
      }))
      
      #   ind <- rep(TRUE, 61)
      #   ind[c(5, 7, 9, 14, 22, 27, 31, 36, 40, 41, 46, 52, 57, 61)] <- FALSE
      ta.pred <- ta.complete[ind, ]
      ta.eval <- ta.complete[!ind, ]
      
      extracted.values <- lapply(seq(nlayers(exp.var.stck.cr)), function(j) {
        exp <- extract(exp.var.stck.cr[[j]], ta.pred)
      })
      extracted.values$ndvi <- extract(ndvi.stck.cr[[m]], ta.pred)
      names(extracted.values) <- nms.grids
      
      for (k in seq(extracted.values)) {
        ta.pred@data[nms.grids[k]] <- extracted.values[[k]]
      }
      
      exp.var.stck.cr.sp@data$ndvi <- ndvi@data[, 1]
      new.sp <- exp.var.stck.cr.sp
      names(new.sp) <- nms.grids
      
      fmla <- as.formula(paste("Ta_200 ~ ", 
                               paste(names(ta.pred)[15:(length(names(ta.pred)) - 1)],
                                     collapse = "+")))
      #fmla <- as.formula(paste("Temp ~ dem"))
      krig.result <- autoKrige(fmla, input_data = ta.pred, new_data = new.sp)
      
      #plot(krig.result)
      
      pred <- raster(krig.result[[1]])
      pred.temp <- extract(pred, ta.eval)
      eval.elev <- extract(exp.var.stck.cr[[1]], ta.eval)
      
      df.out <- data.frame(iteration = i,
                           year = ta.eval@data$year,
                           month = ta.eval@data$Month,
                           PlotID = as.character(ta.eval@data$PlotId),
                           StationID = as.character(ta.eval@data$StationId),
                           Ta_obs = ta.eval@data$Ta_200,
                           Ta_pred = pred.temp,
                           elevation = eval.elev,
                           nfeatures = length(unique(ta.pred@data$PlotId)),
                           model = krig.result$var_model$model[2])
      
      dir.create(paste("results", "krig", sep = "/"), showWarnings = FALSE)
      
      nm.out <- paste("results/krig/ki_krig_ta_dem_slp_svf_ndvi", 
                      unique(ta.eval@data$year),
                      sprintf("%02.f", unique(ta.eval@data$Month)), sep = "_")
      
      write.table(df.out, paste(nm.out, "csv", sep = "."), 
                  row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      
    }
  }
}
cat("... all kriging finished")