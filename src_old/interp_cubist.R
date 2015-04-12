source("global_prep.R")

### method settings
mthd <- "cubist"
tunegr <- expand.grid(.committees = seq(20, 100, 20),
                      .neighbors = seq(3, 9, 2))

### formula definition
fmla <- as.formula(paste("resp ~ ", 
                         paste(names(pred_prep)[1:(length(names(pred_prep)) - 1)],
                               collapse = "+")))

### training
model <- train(fmla, data = pred_prep, method = mthd,
               tuneGrid = tunegr)

save(model, file = paste(mthd, "_interp_train_model.rda", sep = ""))

### interpolation data for prediction
for (i in seq(month.indx)) {
  tmp <- data.frame(dem = dem,
                    slp = slp,
                    svf = svf,
                    asp = asp,
                    ndvi = ndvi[[month.indx[i]]],
                    Ta_reg = pred.vars.unique$Ta_reg[i],
                    Ta_reg_min = pred.vars.unique$Ta_reg_min[i],
                    Ta_reg_max = pred.vars.unique$Ta_reg_max[i],
                    x = x,
                    y = y,
                    month = pred.vars.unique$month[i],
                    year = pred.vars.unique$year[i])
  
  fit <- predict(model, tmp)
  
  tmplt <- exp.var.stck.cr[[1]][[1]]
  tmplt[] <- fit
  
  nm <- paste("ki_", resp.var, "_", mthd, "_interp_", 
              pred.vars.unique$datetime[i], 
              "_utm37s_wgs84", ".tif", sep = "")
  
  out.dir <- paste("interp_maps", mthd, sep = "/")
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  
  out.nm <- paste(out.dir, nm, sep = "/")
  
  writeRaster(tmplt, out.nm, overwrite = TRUE)
  
}

cat("...finished...")

# spplot(tmplt, col.regions = colorRampPalette(rev(brewer.pal(11, "Spectral")))(1000),
#        maxpixels = ncell(tmplt), at = seq(3, 33, 0.1))