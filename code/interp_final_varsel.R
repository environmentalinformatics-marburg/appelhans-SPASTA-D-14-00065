### method settings
## First read in the arguments listed at the command line
args = (commandArgs(TRUE))

## args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if (length(args) == 0) {
  cat("No arguments supplied. \nSetting method to default (rf)")
  ##supply default values
  mthd = "rf"
} else {
  for(i in 1:length(args)) {
    eval(parse(text = args[[i]]))
  }
}

cat("\nMethod is :", mthd, "\n\n")

library(automap)
library(gmt)
library(Rsenal)

source("set_wd.R")
source("code/mthd_def_lst.R")
load("data/ta.pred.complete.rda")


### global parameters
n_folds <- 2
n_cvs <- 2


### final data set
pred_prep <- ta.pred[, -which(names(ta.pred) %in% c("plotID", "lg",
                                                    "datetime", "x", "y",
                                                    "month", "year"))]
resp_col <- colnames(pred_prep) %in% "resp"
pred <- pred_prep[ , !resp_col]
resp <- pred_prep[, resp_col]


# ### final rfe model
cv_splits <- createFolds(resp, k = n_folds,
                         returnTrain = TRUE)
n_var <- 2:ncol(pred)

# rfeCntrl <- rfeControl(functions = mthd_def_lst[[mthd]]$fncs,
#                        method = "cv", index = cv_splits,
#                        returnResamp = "all",
#                        seeds = trainSeeds(size = 100,
#                                           number = n_cvs),
#                        verbose = TRUE,
#                        rerank = FALSE)
#
# trCntr <- trainControl(method = "cv", number = n_cvs,
#                        seeds = trainSeeds(size = 100,
#                                           number = n_cvs),
#                        repeats = 1, verbose = TRUE)
#
# if (mthd %in% c("nnet", "avNNet")) {
#   rfe_model <- rfe(x = pred, y = resp,
#                    method = mthd,
#                    metric = "RMSE",
#                    sizes = n_var,
#                    linout = TRUE, trace = FALSE,
#                    rfeControl = rfeCntrl,
#                    trControl = trCntr,
#                    tuneGrid = mthd_def_lst[[mthd]]$tunegr)
# } else {
#   rfe_model <- rfe(x = pred, y = resp,
#                    method = mthd,
#                    metric = "RMSE",
#                    sizes = n_var,
#                    rfeControl = rfeCntrl,
#                    trControl = trCntr,
#                    tuneGrid = mthd_def_lst[[mthd]]$tunegr)
# }
#
# print(rfe_model)
#
# save(rfe_model, file = paste("interp_maps/", mthd,
#                              "_interp_final_rfe_model.rda", sep = ""))
# #plot(rfe_model, type = c("g", "o"))
# #plotModelCV(rfe_model)
#
#
# ### variable importance calculations rfe
# vimp_rfe <- varImp(rfe_model, scale = TRUE)
# if (any(vimp_rfe$Overall == Inf)) {
#   vimp_rfe <- filterVarImp(pred, resp, nonpara = TRUE)
#   vimp_rfe <- vimp_rfe[order(vimp_rfe$Overall, decreasing = TRUE),
#                        , drop = FALSE]
# }
# vimp_rfe$Overall <- vimp_rfe$Overall / max(vimp_rfe$Overall) * 100
#
# v_imp_rfe <- list(importance = data.frame(Overall = vimp_rfe),
#                   model = "loess r-squared",
#                   calledFrom = "varImp")
# class(v_imp_rfe) <- "varImp.train"
# plot(v_imp_rfe)



## extract only relevant variables
vimp <- read.csv("results/mean_weighted_variable_importance_varsel.csv")
vars <- as.character(vimp$variable[vimp$method == mthd])


### final varsel model
trCntr_varsel <- trainControl(method = "cv", index = cv_splits,
                              seeds =
                                trainSeeds(size = 100,
                                           number = n_cvs),
                              repeats = 1, verbose = TRUE)

if (mthd %in% c("nnet", "avNNet")) {
  model_varsel <- train(pred[, vars], resp, method = mthd,
                        trControl = trCntr_varsel,
                        linout = TRUE, trace = FALSE,
                        tuneGrid = mthd_def_lst[[mthd]]$tunegr)
} else {
  if (mthd == "rf") {
    model_varsel <- train(pred[, vars], resp, method = mthd,
                          trControl = trCntr_varsel,
                          importance = TRUE,
                          tuneGrid = mthd_def_lst[[mthd]]$tunegr)
  } else {
    model_varsel <- train(pred[, vars], resp, method = mthd,
                          trControl = trCntr_varsel,
                          tuneGrid = mthd_def_lst[[mthd]]$tunegr)
  }
}

#plot(model_varsel, type = c("g", "o"))

print(model_varsel)

save(model_varsel, file = paste("interp_maps/", mthd,
                                "_interp_final_varsel_model.rda", sep = ""))

### variable importance calculations varsel
vimp_varsel <- varImp(model_varsel, scale = FALSE)
if (any(vimp_varsel$importance$Overall == Inf)) {
  vimp_varsel <- filterVarImp(pred, resp, nonpara = TRUE)
  vimp_varsel <- vimp_varsel[order(vimp_varsel$Overall, decreasing = TRUE),
                             , drop = FALSE]
} else {
  #if (vimp_varsel$Overall[1] == Inf) vimp_varsel$Overall[1] <- 100
  vimp_varsel <- vimp_varsel$importance[order(vimp_varsel$importance,
                                              decreasing = TRUE),
                                        , drop = FALSE]
}
vimp_varsel$Overall <- vimp_varsel$Overall / max(vimp_varsel$Overall) * 100

v_imp_varsel <- list(importance = data.frame(Overall = vimp_varsel),
                     model = "loess r-squared",
                     calledFrom = "varImp")
class(v_imp_varsel) <- "varImp.train"
plot(v_imp_varsel)


### final monthly interpolation
month.indx <- as.numeric(sapply(split(ta.pred, ta.pred$datetime),
                                function(i) unique(i$month)))

load("data/exp.var.stck.cr.rda")
load("data/ndvi.stck.cr.rda")

ta.pred.splt <- split(ta.pred, ta.pred$datetime)

for (i in seq(month.indx)) {

  cat("\n========================================================\n",
      "\n", "interpolating year ", unique(ta.pred.splt[[i]]$year),
      " month ", unique(ta.pred.splt[[i]]$month), ": run ", i, " of ",
      length(ta.pred.splt), "\n",
      "\n========================================================", "\n\n",
      sep = "")

  tmp <- data.frame(dem = exp.var.stck.cr[[1]][],
                    slp = exp.var.stck.cr[[2]][],
                    svf = exp.var.stck.cr[[3]][],
                    asp = exp.var.stck.cr[[4]][],
                    ndvi = ndvi.stck.cr[[month.indx[i]]][],
                    Ta_reg = unique(ta.pred.splt[[i]]$Ta_reg),
                    Ta_reg_lwr = unique(ta.pred.splt[[i]]$Ta_reg_lwr),
                    Ta_reg_upr = unique(ta.pred.splt[[i]]$Ta_reg_upr))

  ### rfe
#   cat("\n\nfitting final rfe model interpolation\n\n")
#   fit_rfe <- predict(rfe_model, tmp)
#
#   tmplt <- exp.var.stck.cr[[1]][[1]]
#   tmplt[] <- fit_rfe
#   names(tmplt) <- "fit"
#
#   nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_rfe_",
#               unique(ta.pred.splt[[i]]$datetime),
#               "_utm37s_wgs84", ".tif", sep = "")
#
#   out.dir <- paste("interp_maps", mthd, sep = "/")
#   dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
#
#   out.nm <- paste(out.dir, nm, sep = "/")
#
#   writeRaster(tmplt, out.nm, overwrite = TRUE)
#
#
#   ### final adaptation using kriged rfe predicition errors
#   newdat <- as(stack(exp.var.stck.cr, ndvi.stck.cr[[month.indx[i]]]),
#                "SpatialGridDataFrame")
#   names(newdat@data) <- c("dem", "slp", "svf", "asp", "ndvi")
#
#   spatial_df <- ta.pred.splt[[i]]
#   coordinates(spatial_df) <- ~ x + y
#   proj4string(spatial_df) <- projection(exp.var.stck.cr)
#
#   pred_vals_rfe <- extract(tmplt, spatial_df, sp = TRUE)
#   pred_vals_rfe$error_rfe <- pred_vals_rfe$resp - pred_vals_rfe$fit
#
#   #fmla <- as.formula("error_rfe ~ dem + slp + svf + asp + ndvi")
#   fmla <- as.formula("error_rfe ~ 1")
#
#   cat("\n\nadapting final rfe prediction with kriged prediction errors\n\n")
#
#   krig_res <- autoKrige(fmla,
#                         input_data = pred_vals_rfe,
#                         new_data = newdat)
#
#   save(krig_res, file = paste("interp_maps/", mthd,
#                               "_krig_err_interp_final_rfe_model_",
#                               unique(ta.pred.splt[[i]]$datetime), ".rda",
#                               sep = ""))
#
#   err_rst <- raster(krig_res[[1]])
#   final_rst <- tmplt + err_rst
#
#   nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_rfeOK_",
#               unique(ta.pred.splt[[i]]$datetime),
#               "_utm37s_wgs84", ".tif", sep = "")
#
#   out.nm <- paste(out.dir, nm, sep = "/")
#
#   err.nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_rfeOK_error_",
#                   unique(ta.pred.splt[[i]]$datetime),
#                   "_utm37s_wgs84", ".tif", sep = "")
#
#   out.err <- paste(out.dir, err.nm, sep = "/")
#
#   writeRaster(err_rst, out.err, overwrite = TRUE)
#   writeRaster(final_rst, out.nm, overwrite = TRUE)


  ### varsel
  cat("\n\nfitting final varsel model interpolation\n\n")
  fit_varsel <- predict(model_varsel, tmp[, vars])

  tmplt <- exp.var.stck.cr[[1]][[1]]
  tmplt[] <- fit_varsel
  names(tmplt) <- "fit"

  nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_varsel_",
              unique(ta.pred.splt[[i]]$datetime),
              "_utm37s_wgs84", ".tif", sep = "")

  out.dir <- paste("interp_maps", mthd, sep = "/")
  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
  out.nm <- paste(out.dir, nm, sep = "/")

  writeRaster(tmplt, out.nm, overwrite = TRUE)


  ### final adaptation using kriged varsel predicition errors
  newdat <- as(stack(exp.var.stck.cr, ndvi.stck.cr[[month.indx[i]]]),
               "SpatialGridDataFrame")
  names(newdat@data) <- c("dem", "slp", "svf", "asp", "ndvi")

  spatial_df <- ta.pred.splt[[i]]
  coordinates(spatial_df) <- ~ x + y
  proj4string(spatial_df) <- projection(exp.var.stck.cr)

  pred_vals_varsel <- extract(tmplt, spatial_df, sp = TRUE)
  pred_vals_varsel$error_varsel <- pred_vals_varsel$resp - pred_vals_varsel$fit

  #fmla <- as.formula("error_varsel ~ dem + slp + svf + asp + ndvi")
  #fmla <- as.formula("error_varsel ~ dem + ndvi")
  fmla <- as.formula("error_varsel ~ 1")

  cat("\n\nadapting final varsel prediction with kriged prediction errors\n\n")

  krig_res <- autoKrige(fmla,
                        input_data = pred_vals_varsel,
                        new_data = newdat)

  save(krig_res, file = paste("interp_maps/", mthd,
                              "_krig_err_interp_final_varsel_model_",
                              unique(ta.pred.splt[[i]]$datetime), ".rda",
                              sep = ""))

  err_rst <- raster(krig_res[[1]])
  final_rst <- tmplt + err_rst

  nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_varselOK_",
              unique(ta.pred.splt[[i]]$datetime),
              "_utm37s_wgs84", ".tif", sep = "")

  out.nm <- paste(out.dir, nm, sep = "/")

  err.nm <- paste("ki_", "Ta_200", "_", mthd, "_interp_varselOK_error_",
              unique(ta.pred.splt[[i]]$datetime),
              "_utm37s_wgs84", ".tif", sep = "")

  out.err <- paste(out.dir, err.nm, sep = "/")

  writeRaster(err_rst, out.err, overwrite = TRUE)
  writeRaster(final_rst, out.nm, overwrite = TRUE)

}

cat("all finished...")

# spplot(tmplt, col.regions = envinmrPalette(1000),
#        maxpixels = ncell(tmplt), at = seq(-10, 35, 0.1))
#
# spplot(final_rst, col.regions = envinmrPalette(1000),
#        maxpixels = ncell(tmplt), at = seq(-10, 35, 0.1))
