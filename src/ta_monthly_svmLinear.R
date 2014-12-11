source("global_prep.R")

### method settings
mthd <- "svmLinear"
tunegr <- expand.grid(.C = seq(0.25, 1, 0.25))

### global training
for (i in 1:iters) {
  cat("\n\nRUNNING", mthd, ": ITERATION", i, "\n")
  set.seed(i)
  
  ind.eval <- sample(nrow(pred_prep), 
                     nrow(pred_prep) * train.size)
  eval <- pred_prep[-ind.eval, ]
  ta.eval <- plots.ta.monthly.rug[-ind.eval, ]
  pred <- pred_prep[ind.eval, ]
  
  fmla <- as.formula(paste("resp ~ ", 
                           paste(names(pred)[1:(length(names(pred)) - 1)],
                                 collapse = "+")))
  
  model <- train(fmla, data = pred, method = mthd,
                 tuneGrid = tunegr)
  
  vimp <- varImp(model, scale = TRUE)
  imp_month <- mean(vimp$importance[grep(glob2rx("month*"), 
                                         rownames(vimp$importance)), ],
                    na.rm = TRUE)
  imp_year <- mean(vimp$importance[grep(glob2rx("year*"), 
                                        rownames(vimp$importance)), ],
                   na.rm = TRUE)
  imp_rest <- vimp$importance[-c(grep(glob2rx("month*"), 
                                      rownames(vimp$importance)),
                                 grep(glob2rx("year*"), 
                                      rownames(vimp$importance))), ]
  vimp.df <- data.frame(method = mthd,
                        variable = names(pred)[1:(length(names(pred)) - 1)],
                        importance = c(imp_rest, imp_month, imp_year))
  vimp.df <- vimp.df[order(vimp.df$importance, decreasing = TRUE), ]
  vimp.df$imp.score <- 1:nrow(vimp.df)
  vimp.df$iteration = i
  
  imp.path <- paste("results", mthd, 
                    paste("varimp", mthd,
                          nms_out,
                          sep = "_"),
                    sep = "/")
  
  dir.create(paste("results", mthd, sep = "/"), showWarnings = FALSE)
  
  write.table(vimp.df, imp.path, 
              row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  
  fit <- predict(model, eval)
  
  df.out <- data.frame(year = ta.eval@data$year,
                       month = ta.eval@data$Month,
                       PlotID = as.character(ta.eval@data$PlotId),
                       StationID = as.character(ta.eval@data$StationId),
                       Ta_obs = ta.eval@data$Ta_200,
                       Ta_pred = fit,
                       elevation = ta.eval@data$dem,
                       method = mthd,
                       iteration = i)
  
  out.path <- paste("results", mthd, 
                    paste("ki", mthd,
                          nms_out,
                          sep = "_"),
                    sep = "/")
  
  write.table(df.out, out.path, 
              row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  
  print(model)
}