library(raster)
library(latticeExtra)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

fls <- list.files("results_server", pattern = glob2rx("*2013-07*"),
                  full.names = TRUE)

rsts <- lapply(fls, function(i) {
  raster(i)
})

rsts <- rsts[-3]

clrs <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

panel_txt <- c("a) avNNet", "b) Kriging", "c) Random forest")

plots <- lapply(seq(rsts), function(i) {
  spplot(rsts[[i]], col.regions = clrs(1000), maxpixels = ncell(rsts[[1]]),
         colorkey = list(space = "top"), at = seq(-10, 35, 0.5),
         panel = function(x, y, ...) {
           panel.levelplot(x, y, ...)
           panel.text(x = 295000, y = 9627500, 
                      labels = panel_txt[i],
                      adj = c(0, 0.5))
           })
})

latticeCombineGrid <- function(x, y, ...) {
  update(c(x, y, 
           layout = c(1, 2)), 
         between = list(y = 0.3, x = 0.3))
}

out <- Reduce(latticeCombineGrid, plots)

png("test.png", width = 20, height = 30, units = "cm", res = 300)
print(out)
dev.off()