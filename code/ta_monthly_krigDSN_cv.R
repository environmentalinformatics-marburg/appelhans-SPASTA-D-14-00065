library(automap)
library(raster)

source("set_wd.R")

load("data/ta.pred.splt.folds.rda")
load("data/exp_var_stck_sp.rda")

for (j in seq(ta.pred.splt.folds)) {

  for (i in seq(ta.pred.splt.folds[[j]]$train)) {

    ### define test, train etc for current run
    test <- ta.pred.splt.folds[[j]]$test[[i]]
    test$ndvi_tr <- sinpi(test$ndvi)
    train <- ta.pred.splt.folds[[j]]$train[[i]]
    train$ndvi_tr <- sinpi(train$ndvi)
    newdat <- exp_var_stck_sp[[j]]
    newdat$ndvi_tr <- sinpi(newdat$ndvi)

    ### convert to psatial object
    coordinates(test) <- ~x+y
    proj4string(test) <- CRS(proj4string(newdat))
    coordinates(train) <- ~x+y
    proj4string(train) <- CRS(proj4string(newdat))

    cat("\n========================================================\n",
        "\n", "krigDSN year ", unique(test@data$year),
        " month ", unique(test@data$month), ": run ", j, "-", i, " of ",
        length(ta.pred.splt.folds), "(10)", "\n",
        "\n========================================================", "\n\n",
        sep = "")

    fmla <- as.formula(paste("resp ~ dem + svf + ndvi_tr"))

    krig_res <- autoKrige(fmla, input_data = train, new_data = newdat)

    pred <- raster(krig_res[[1]])
    pred_temp <- extract(pred, test)

    df_out <- data.frame(year = test@data$year,
                         month = test@data$month,
                         PlotID = test@data$plotID,
                         StationID = "000rug",
                         Ta_obs = test@data$resp,
                         Ta_pred = round(pred_temp, 2),
                         method = "krigDSN",
                         iteration = i,
                         model = as.character(krig_res$var_model$model[2]),
                         stringsAsFactors = FALSE)

    dir.create(paste("results", "krigDSN", sep = "/"), showWarnings = FALSE)

    if(nchar(unique(test@data$month)) == 2) {
      mnth <- unique(test@data$month)
    } else {
      mnth <- paste("0", unique(test@data$month), sep = "")
    }

    nm_out <- paste("results/krigDSN/ki_krigDSN_ta_dem_svf_ndvi",
                    unique(test@data$year), mnth, sep = "_")

    out_path <- paste(nm_out, "csv", sep = ".")

    if (i == 1) {
      write.table(df_out, out_path, row.names = FALSE,
                  col.names = TRUE, append = FALSE, sep = ",")
    } else {
      write.table(df_out, out_path, row.names = FALSE,
                  col.names = FALSE, append = TRUE, sep = ",")
    }

  }

}
