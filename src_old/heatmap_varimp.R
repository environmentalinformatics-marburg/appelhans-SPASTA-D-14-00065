library(dplyr)
library(latticeExtra)
library(gridExtra)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

fls_varimp <- list.files("results_server", pattern = glob2rx("var*.csv"),
                         full.names = TRUE, recursive = TRUE)

varimp <- do.call("rbind", lapply(fls_varimp, function(i) {
  tmp <- read.csv(i, stringsAsFactors = TRUE)
  names(tmp) <- c("method", "variable", "importance", "rank", "iteration")
  return(tmp)
}))

### refactorize method so that it is ordered according to 
### performance statistics, works only with dotplots results still in 
### global environment

if (exists("stats")) {
  varimp$method <- factor(varimp$method, levels = stats$method)
}

var_stats <- varimp %>%
  group_by(method, variable) %>%
  summarise(varimp = median(rank))

var_stats_ovrall <- var_stats %>%
  group_by(variable) %>%
  summarise(rank = median(varimp))

var_stats_ovrall <- var_stats_ovrall[order(var_stats_ovrall$rank), ]
var_stats_ovrall$variable <- as.character(var_stats_ovrall$variable)

var_stats$variable <- factor(var_stats$variable,
                             levels = var_stats_ovrall$variable)

clr <- colorRampPalette(rev(brewer.pal(9, "PuBuGn")))

hmap <- levelplot(varimp ~ variable * method, data = var_stats,
                  col.regions = clr(13), at = seq(0.5, 11.5, 1),
                  asp = 1, as.table = TRUE,
                  ylab = "Method", xlab = "Variable",
                  scales = list(x = list(rot = 45)),
                  main = "Median variable importance ranking",
                  colorkey = list(space = "top", 
                                  width = 1, height = 0.75))

pdf("varimp_heatmap.pdf", paper = "a4")
hmap
dev.off()
