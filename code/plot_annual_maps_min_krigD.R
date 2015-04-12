library(raster)
library(Rsenal)
library(latticeExtra)

source("set_wd.R")


### load interpolated maps
ptrn <- "ki*varselOK_2*.tif"

fls_ml <- list.files("interp_maps", pattern = glob2rx(ptrn),
                     full.names = TRUE, recursive = TRUE)

fls_krigD <- list.files("interp_maps/krigD",
                        full.names = TRUE)

# fls_krigDSN <- list.files("interp_maps/krigDSN",
#                           full.names = TRUE)

fls_mthd <- split(fls_ml, substr(fls_ml, 1, 14))
# fls_mthd[[5]] <- fls_krigD
# fls_mthd[[6]] <- fls_krigDSN

stck_lst <- lapply(fls_mthd, function(i) {
  stack(i)
})

ann_mean <- lapply(seq(stck_lst), function(i) {
  calc(stck_lst[[i]], fun = mean)
})

stck_krigD <- stack(fls_krigD)

ann_mean_krigD <- calc(stck_krigD, fun = mean)

mean_diff <- lapply(seq(ann_mean), function(i) {
  ann_mean[[i]] - ann_mean_krigD
})


### load dem and produce contourplot
ki_dem_utm <- raster("data/in/DEM_UTM37S_WGS84_30m_Hemp.tif")
ki_dem_utm <- crop(ki_dem_utm, extent(ann_mean[[1]]))

ki_dem_flipped <- flip(ki_dem_utm, "y")
x <- coordinates(ki_dem_flipped)[, 1]
y <- coordinates(ki_dem_flipped)[, 2]
z <- ki_dem_flipped[]

cp <- levelplot(z ~ x * y, alpha = 0.5,
                at = seq(500, 6000, 500), colorkey = FALSE,
                panel = function(...) {
                  Rsenal:::panel.filledcontour(contours = TRUE,
                                               fill = FALSE,
                                               col.contours = "grey30",
                                               lwd = 0.2,
                                               ...)
                })


ann_mean_plts <- lapply(seq(ann_mean), function(i) {

  labs <- c("gbm - krigD", "cubist - krigD", "rf - krigD", "avNNet - krigD")

  clrs <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
  p <- spplot(mean_diff[[i]], col.regions = clrs(1000),
              at = seq(-12.5, 12, 0.1),
              maxpixels = ncell(ann_mean[[i]]),
              main = "Temperature [K]",
              colorkey = list(space = "top", height = 0.75, width = 1),
              panel = function(...) {
                panel.levelplot(...)
                panel.text(x = 302000, y = 9626500,
                           labels = labs[i], adj = c(0, 0))
              })

  envinmrRasterPlot(p) + latticeExtra::as.layer(cp)

})

out <- latticeCombineGrid(ann_mean_plts, layout = c(2, 2))

png("graphs/maps_min_krigD.png", width = 25, height = 30, units = "cm", res = 300)
plot.new()
print(out)
dev.off()
