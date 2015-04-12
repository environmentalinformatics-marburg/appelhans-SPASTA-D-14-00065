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


### set path to parent directory and source relevant material
library(Rsenal)

source("set_wd.R")
source("code/mthd_def_lst.R")
#source("code/global_prep_cv.R")
load("data/ta.pred.splt.folds.rda")

### global calc options
n_folds <- 10
n_cvs <- 10
# n_iters <- seq(2)
# train.size <- 0.8
# glob.seed <- 1234


### extract training data
extractTrain <- function(fold) {
  function(dat) dat$train[[fold]]
}

train_lst <- lapply(seq(10), function(i) {

  f <- extractTrain(i)

  folds <- do.call("rbind", lapply(ta.pred.splt.folds, function(j) {
    f(j)
  }))

  return(folds)
})


### extract test data
extractTest <- function(fold) {
  function(dat) dat$test[[fold]]
}

test_lst <- lapply(seq(10), function(i) {

  f <- extractTest(i)

  folds <- do.call("rbind", lapply(ta.pred.splt.folds, function(j) {
    f(j)
  }))

  return(folds)
})



### run model selection etc
for (i in seq(train_lst)) {
  cat("\n========================================================\n",
      "\n", toupper("calculating iteration: "), mthd, i, "\n",
      "\n========================================================", "\n\n")

  #set.seed(glob.seed + i)

  ### prepare data
  pred_prep <- train_lst[[i]][, -which(names(train_lst[[i]]) %in%
                                         c("plotID", "lg", "datetime",
                                           "x", "y", "month", "year"))]
  pred_prep$ndvi_tr <- sinpi(pred_prep$ndvi)
  pred_prep <- pred_prep[, -which(names(pred_prep) %in% "ndvi")]
  pred_prep <- pred_prep[, c(1:4, 9, 5:8)]

  glob.eval <- test_lst[[i]][, -which(names(test_lst[[i]]) %in%
                                        c("plotID", "lg", "datetime",
                                          "x", "y", "month", "year"))]
  glob.eval$ndvi_tr <- sinpi(glob.eval$ndvi)
  glob.eval <- glob.eval[, -which(names(glob.eval) %in% "ndvi")]
  glob.eval <- glob.eval[, c(1:4, 9, 5:8)]

  #save(glob.eval, file = "data/glob_eval.rda")
#   pred_prep <- pred_prep[glob.ind.eval, ]
#   plots.ta.monthly.rug.pred <- plots.ta.monthly.rug[glob.ind.eval, ]
#   plots.ta.monthly.rug.eval <- plots.ta.monthly.rug[-glob.ind.eval, ]

  nms_out <- paste(paste(names(pred_prep)[1:(length(names(pred_prep)) - 1)],
                         collapse = "_"), ".csv", sep = "")


  ### split into predictor and response set
  resp_col <- colnames(pred_prep) %in% "resp"
  pred <- pred_prep[ , !resp_col]
  resp <- pred_prep[, resp_col]


  ### variable selection model
  cv_splits <- createFolds(resp, k = n_folds,
                           returnTrain = TRUE)
  n_var <- 2:ncol(pred)

  rfeCntrl <- rfeControl(functions = mthd_def_lst[[mthd]]$fncs,
                         method = "cv", index = cv_splits,
                         returnResamp = "all",
                         seeds = trainSeeds(size = 100,
                                            number = n_cvs),
                         verbose = TRUE,
                         rerank = FALSE)

  trCntr <- trainControl(method = "cv", number = n_cvs,
                         seeds = trainSeeds(size = 100,
                                            number = n_cvs),
                         repeats = 1, verbose = TRUE)

  if (mthd %in% c("nnet", "avNNet")) {
    rfe_model <- rfe(x = pred, y = resp,
                     method = mthd,
                     metric = "RMSE",
                     sizes = n_var,
                     linout = TRUE, trace = FALSE,
                     rfeControl = rfeCntrl,
                     trControl = trCntr,
                     tuneGrid = mthd_def_lst[[mthd]]$tunegr)
  } else {
    rfe_model <- rfe(x = pred, y = resp,
                     method = mthd,
                     metric = "RMSE",
                     sizes = n_var,
                     rfeControl = rfeCntrl,
                     trControl = trCntr,
                     tuneGrid = mthd_def_lst[[mthd]]$tunegr)
  }


  #plot(rfe_model, type = c("g", "o"))
  #plotModelCV(rfe_model)


  ### variable importance calculations rfe
  vimp_rfe <- varImp(rfe_model, scale = TRUE)
  if (any(vimp_rfe$Overall == Inf)) {
    vimp_rfe <- filterVarImp(pred, resp, nonpara = TRUE)
    vimp_rfe <- vimp_rfe[order(vimp_rfe$Overall, decreasing = TRUE),
                               , drop = FALSE]
  }
  vimp_rfe$Overall <- vimp_rfe$Overall / max(vimp_rfe$Overall) * 100

  v_imp_rfe <- list(importance = data.frame(Overall = vimp_rfe),
                    model = "loess r-squared",
                    calledFrom = "varImp")
  class(v_imp_rfe) <- "varImp.train"
  plot(v_imp_rfe)


  ## extract only relevant variables
  vars <- varsRfeCV(rfe_model)


  ### final model
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


  ### variable importance calculations rfe
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


  ### predict fits from original and final model
  fit_rfe <- predict(rfe_model, glob.eval[, 1:8])
  fit_varsel <- predict(model_varsel, glob.eval[, vars])


  ### create and write prediction table
  dir.create(paste("results", mthd, sep = "/"),
             showWarnings = FALSE, recursive = TRUE)

  df.out <- data.frame(year = test_lst[[i]]$year,
                       month = test_lst[[i]]$month,
                       PlotID = test_lst[[i]]$plotID,
                       StationID = "000rug",
                       Ta_obs = glob.eval$resp,
                       Ta_pred_varsel = round(fit_varsel, 2),
                       Ta_pred_rfe = round(fit_rfe, 2),
                       method = mthd,
                       iteration = i,
                       stringsAsFactors = FALSE)

  out.path <- paste("results", mthd,
                    paste("ki", mthd,
                          nms_out,
                          sep = "_"),
                    sep = "/")

  out_nm_mod_rfe <- paste0("model_rfe_ndvi_tr_", mthd, "_", fixWidth(i, 2), ".rda")
  save(rfe_model, file = paste("results", mthd, out_nm_mod_rfe, sep = "/"))

  out_nm_mod_varsel <- paste0("model_varsel_ndvi_tr_", mthd, "_",
                              fixWidth(i, 2), ".rda")
  save(model_varsel, file = paste("results", mthd,
                                  out_nm_mod_varsel, sep = "/"))

  if (i == 1) {
    write.table(df.out, out.path, row.names = FALSE,
                col.names = TRUE, append = FALSE, sep = ",")
  } else {
    write.table(df.out, out.path, row.names = FALSE,
                col.names = FALSE, append = TRUE, sep = ",")
  }


  ### create and write variable importance table
  vimp_rfe.df <- v_imp_rfe$importance
  vimp_rfe.df$variable <- rownames(vimp_rfe.df)
  vimp_rfe.df$method <- mthd
  vimp_rfe.df$iteration <- i

  vimp_varsel.df <- v_imp_varsel$importance
  vimp_varsel.df$variable <- rownames(vimp_varsel.df)
  vimp_varsel.df$method <- mthd
  vimp_varsel.df$iteration <- i

  imp.path_rfe <- paste("results", mthd,
                        paste("varimp_rfe_ndvi", mthd,
                              nms_out,
                              sep = "_"),
                        sep = "/")

  imp.path_varsel <- paste("results", mthd,
                           paste("varimp_varsel_ndvi", mthd,
                                 nms_out,
                                 sep = "_"),
                           sep = "/")

  if (i == 1) {
    write.table(vimp_rfe.df, imp.path_rfe, row.names = FALSE,
                col.names = TRUE, append = FALSE, sep = ",")
    write.table(vimp_varsel.df, imp.path_varsel, row.names = FALSE,
                col.names = TRUE, append = FALSE, sep = ",")
  } else {
    write.table(vimp_rfe.df, imp.path_rfe, row.names = FALSE,
                col.names = FALSE, append = TRUE, sep = ",")
    write.table(vimp_varsel.df, imp.path_varsel, row.names = FALSE,
                col.names = FALSE, append = TRUE, sep = ",")
  }

  ### print models to console for visual inspection
  cat("\n", toupper("rfe model iteration: "), i, "\n\n")
  print(rfe_model)

  cat("\n", toupper("variable selection model iteration: "), i, "\n\n")
  print(model_varsel)

}

cat("\n\n", mthd, "... all finished\n\n")
