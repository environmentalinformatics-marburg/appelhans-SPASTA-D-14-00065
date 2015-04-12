library(dplyr)
library(latticeExtra)
library(gridExtra)

path <- "~/tappelhans/uni/temp"
setwd(path)

fls_varimp_varsel <- list.files("results",
                                pattern = glob2rx("varimp_varsel*.csv"),
                         full.names = TRUE, recursive = TRUE)

findMatch <- function(ptrn) {
  function(strng) grep(ptrn, strng)
}

### kriging results amalgamation
indx_ln <- do.call("c", lapply(c("earth", "glm", "pcr", "pls", "svmLinear"),
                               function(i) {
                                 f <- findMatch(i)
                                 f(fls_varimp_varsel)
                               }))
fls_ln <- fls_varimp_varsel[indx_ln]
fls_ln <- fls_ln[grep(glob2rx("*_ndvi_tr*"), fls_ln)]

fls_varimp_varsel <- c(fls_varimp_varsel[-indx_ln],
                       fls_ln)

varimp_varsel <- do.call("rbind", lapply(fls_varimp_varsel, function(i) {
  tmp <- read.csv(i, stringsAsFactors = TRUE)
  names(tmp) <- c("importance", "variable", "method", "iteration")
  return(tmp)
}))

### refactorize method so that it is ordered according to
### performance statistics, works only with dotplots results still in
### global environment

if (exists("stats_rfe")) {
  varimp_varsel$method <- factor(varimp_varsel$method,
                                 levels = levels(stats_rfe$method))
}

var_count <- varimp_varsel %>%
  count(variable, method)

var_stats <- varimp_varsel %>%
  group_by(method, variable) %>%
  summarise(varimp_varsel = mean(importance))

var_stats <- merge(var_stats, var_count)
var_stats$varimp_weighted <- var_stats$varimp_varsel * var_stats$n / 10

var_stats_ovrall <- var_stats %>%
  group_by(variable) %>%
  summarise(rank = mean(varimp_weighted))

var_stats_ovrall <- var_stats_ovrall[order(var_stats_ovrall$rank,
                                           decreasing = TRUE), ]
var_stats_ovrall$variable <- as.character(var_stats_ovrall$variable)

var_stats$variable <- factor(var_stats$variable,
                             levels = var_stats_ovrall$variable)

write.csv(var_stats, "results/mean_weighted_variable_importance_varsel.csv",
          row.names = FALSE)

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

hmap <- levelplot(varimp_weighted ~ variable * method, data = var_stats,
                  col.regions = clr(101), at = seq(0, 100, 1),
                  asp = 1, as.table = TRUE,
                  ylab = "Method", xlab = "Variable",
                  scales = list(x = list(rot = 45)),
                  main = "Variable importance",
                  cex.title = 1,
                  colorkey = list(space = "top",
                                  width = 1, height = 0.75),
                  panel=function(...) {
                    grid.rect(gp=gpar(col=NA, fill="grey60"))
                    panel.levelplot(...)
                  })

# ### imp vars für final interpolation
# fls_finterp <- list.files("interp_maps",
#                           pattern = glob2rx("*interp_final_varsel_model.rda"),
#                           full.names = TRUE)
#
# load(fls_finterp[1])
# vimp_finterp_avNNet <- model_varsel$finalModel$names
# df_avNNet <- data.frame(method = "avNNet",
#                         variable = vimp_finterp_avNNet,
#                         varimp_varsel = 1,
#                         n = 1,
#                         varimp_weighted = 1)
#
# load(fls_finterp[2])
# vimp_finterp_cubist <- model_varsel$finalModel$vars$all
# df_cubist <- data.frame(method = "cubist",
#                         variable = vimp_finterp_cubist,
#                         varimp_varsel = 1,
#                         n = 1,
#                         varimp_weighted = 2)
#
# load(fls_finterp[3])
# vimp_finterp_gbm <- model_varsel$finalModel$var.names
# df_gbm <- data.frame(method = "gbm",
#                         variable = vimp_finterp_gbm,
#                         varimp_varsel = 1,
#                         n = 1,
#                         varimp_weighted = 3)
#
# load(fls_finterp[4])
# vimp_finterp_rf <- model_varsel$finalModel$xNames
# df_rf <- data.frame(method = "rf",
#                         variable = vimp_finterp_rf,
#                         varimp_varsel = 1,
#                         n = 1,
#                         varimp_weighted = 4)
#
# var_stats_new <- var_stats
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "avNNet"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "cubist"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "gbm"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "rf"), ]
#
# var_stats_new$varimp_weighted <- NA
# var_stats_new <- rbind(var_stats_new, df_avNNet, df_cubist, df_gbm, df_rf)
#
# hmap_new <- levelplot(varimp_weighted ~ variable * method,
#                       data = var_stats_new, contours = TRUE,
#                       col.regions = "transparent",
#                       border = "black",
#                       border.lwd = 2,
#                       at = seq(0, 100, 1),
#                       asp = 1, as.table = TRUE,
#                       ylab = "Method", xlab = "Variable",
#                       scales = list(x = list(rot = 45)),
#                       main = "Variable importance",
#                       cex.title = 1,
#                       colorkey = FALSE)
#
# hmap + as.layer(hmap_new)

pdf("graphs/varimp_varsel_heatmap.pdf", paper = "a4")
hmap #+ hmap_new
dev.off()



# ### rfe results
# fls_varimp_rfe <- list.files("results",
#                                 pattern = glob2rx("varimp_rfe*.csv"),
#                                 full.names = TRUE, recursive = TRUE)
#
# findMatch <- function(ptrn) {
#   function(strng) grep(ptrn, strng)
# }
#
# ### kriging results amalgamation
# indx_ln <- do.call("c", lapply(c("earth", "glm", "pcr", "pls", "svmLinear"),
#                                function(i) {
#                                  f <- findMatch(i)
#                                  f(fls_varimp_rfe)
#                                }))
# fls_ln <- fls_varimp_rfe[indx_ln]
# fls_ln <- fls_ln[grep(glob2rx("*_ndvi_tr*"), fls_ln)]
#
# fls_varimp_rfe <- c(fls_varimp_rfe[-indx_ln],
#                        fls_ln)
#
# varimp_rfe <- do.call("rbind", lapply(fls_varimp_rfe, function(i) {
#   tmp <- read.csv(i, stringsAsFactors = TRUE)
#   names(tmp) <- c("importance", "variable", "method", "iteration")
#   return(tmp)
# }))
#
# ### refactorize method so that it is ordered according to
# ### performance statistics, works only with dotplots results still in
# ### global environment
#
# if (exists("stats_rfe")) {
#   varimp_rfe$method <- factor(varimp_rfe$method,
#                                  levels = levels(stats_rfe$method))
# }
#
# var_count <- varimp_rfe %>%
#   count(variable, method)
#
# var_stats <- varimp_rfe %>%
#   group_by(method, variable) %>%
#   summarise(varimp_rfe = mean(importance))
#
# var_stats <- merge(var_stats, var_count)
# var_stats$varimp_weighted <- var_stats$varimp_rfe * var_stats$n / 10
#
# var_stats_ovrall <- var_stats %>%
#   group_by(variable) %>%
#   summarise(rank = mean(varimp_weighted))
#
# var_stats_ovrall <- var_stats_ovrall[order(var_stats_ovrall$rank,
#                                            decreasing = TRUE), ]
# var_stats_ovrall$variable <- as.character(var_stats_ovrall$variable)
#
# var_stats$variable <- factor(var_stats$variable,
#                              levels = var_stats_ovrall$variable)
#
# write.csv(var_stats, "results/mean_weighted_variable_importance_rfe.csv",
#           row.names = FALSE)
#
# clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
#
# hmap <- levelplot(varimp_weighted ~ variable * method, data = var_stats,
#                   col.regions = clr(101), at = seq(0, 100, 1),
#                   asp = 1, as.table = TRUE,
#                   ylab = "Method", xlab = "Variable",
#                   scales = list(x = list(rot = 45)),
#                   main = "Variable importance",
#                   cex.title = 1,
#                   colorkey = list(space = "top",
#                                   width = 1, height = 0.75),
#                   panel=function(...) {
#                     grid.rect(gp=gpar(col=NA, fill="grey60"))
#                     panel.levelplot(...)
#                   })
#
# ### imp vars für final interpolation
# fls_finterp <- list.files("interp_maps",
#                           pattern = glob2rx("*interp_final_rfe_model.rda"),
#                           full.names = TRUE)
#
# load(fls_finterp[1])
# vimp_finterp_avNNet <- rfe_model$optVariables
# df_avNNet <- data.frame(method = "avNNet",
#                         variable = vimp_finterp_avNNet,
#                         varimp_rfe = 1,
#                         n = 1,
#                         varimp_weighted = 1)
#
# load(fls_finterp[2])
# vimp_finterp_cubist <- rfe_model$optVariables
# df_cubist <- data.frame(method = "cubist",
#                         variable = vimp_finterp_cubist,
#                         varimp_rfe = 1,
#                         n = 1,
#                         varimp_weighted = 2)
#
# load(fls_finterp[3])
# vimp_finterp_gbm <- rfe_model$optVariables
# df_gbm <- data.frame(method = "gbm",
#                      variable = vimp_finterp_gbm,
#                      varimp_rfe = 1,
#                      n = 1,
#                      varimp_weighted = 3)
#
# load(fls_finterp[4])
# vimp_finterp_rf <- rfe_model$optVariables
# df_rf <- data.frame(method = "rf",
#                     variable = vimp_finterp_rf,
#                     varimp_rfe = 1,
#                     n = 1,
#                     varimp_weighted = 4)
#
# var_stats_new <- var_stats
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "avNNet"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "cubist"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "gbm"), ]
# var_stats_new <- var_stats_new[!c(var_stats_new$method == "rf"), ]
#
# var_stats_new$varimp_weighted <- NA
# var_stats_new <- rbind(var_stats_new, df_avNNet, df_cubist, df_gbm, df_rf)
#
# hmap_new <- levelplot(varimp_weighted ~ variable * method,
#                       data = var_stats_new, contours = TRUE,
#                       col.regions = "transparent",
#                       border = "black",
#                       border.lwd = 2,
#                       at = seq(0, 100, 1),
#                       asp = 1, as.table = TRUE,
#                       ylab = "Method", xlab = "Variable",
#                       scales = list(x = list(rot = 45)),
#                       main = "Variable importance",
#                       cex.title = 1,
#                       colorkey = FALSE)
#
# hmap + as.layer(hmap_new)
#
# pdf("graphs/varimp_rfe_heatmap.pdf", paper = "a4")
# hmap + hmap_new
# dev.off()
