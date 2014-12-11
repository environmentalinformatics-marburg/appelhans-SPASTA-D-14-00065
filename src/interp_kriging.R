library(automap)
library(raster)

#source("prep_interp_kriging.R")
source("global_prep.R")

mthd <- "kriging"

### split relevant data to create yearly subsets (yss)
plots.ta.monthly.rug.yss <- split(plots.ta.monthly.rug, 
                                  plots.ta.monthly.rug@data$year)
#plots.ta.monthly.rug.yss <- plots.ta.monthly.rug.yss[-1]

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
    
    for (i in 1) {
      set.seed(i)
      cat(" ", "interpolating run", i, "\n", sep = " ")
      s <- split(ta.complete, substr(ta.complete@data$PlotId, 1, 3))
      
      #       ind <- unlist(lapply(seq(s), function(l) {
      #         if (nrow(s[[l]]) > 1) {
      #           v <- rep(TRUE, nrow(s[[l]]))
      #           v[sample(seq(v), 1)] <- FALSE
      #           return(v)
      #         } else {
      #           if (nrow(s[[l]]) == 1) return(TRUE)
      #         }
      #       }))
      
      #   ind <- rep(TRUE, 61)
      #   ind[c(5, 7, 9, 14, 22, 27, 31, 36, 40, 41, 46, 52, 57, 61)] <- FALSE
      ta.pred <- ta.complete #[ind, ]
      ta.eval <- ta.complete #[!ind, ]
      
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
      
      save(krig.result, file = paste(mthd, "_interp_train_model.rda", sep = ""))
      
      #plot(krig.result)
      
      tmplt <- raster(krig.result[[1]])
      
      nm <- paste("ki_", resp.var, "_", mthd, "_interp_", 
                  unique(ta.pred@data$Datetime), 
                  "_utm37s_wgs84", ".tif", sep = "")
      
      out.dir <- paste("interp_maps", mthd, sep = "/")
      dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
      
      out.nm <- paste(out.dir, nm, sep = "/")
      
      writeRaster(tmplt, out.nm, overwrite = TRUE)
      
    }
  }
}

cat("... all kriging finished")