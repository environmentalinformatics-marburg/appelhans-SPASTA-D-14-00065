library(rgdal)
library(raster)
library(latticeExtra)
library(Rsenal)
library(gridExtra)
library(maptools)
library(OpenStreetMap)

source("set_wd.R")

ptrn <- "ki*varselOK_2*.tif"
fls_cubistOK <- list.files("interp_maps/cubist", pattern = glob2rx(ptrn),
                           full.names = TRUE, recursive = TRUE)
cubistOK <- calc(stack(fls_cubistOK), mean)

ptrn <- "ki*varsel_*.tif"
fls_cubist <- list.files("interp_maps/cubist", pattern = glob2rx(ptrn),
                         full.names = TRUE, recursive = TRUE)
cubist <- calc(stack(fls_cubist), mean)

fls_krigD <- list.files("interp_maps/krigD",
                        full.names = TRUE)
krigD <- calc(stack(fls_krigD), mean)

ndvi <- raster("data/in/ki_year_mean_ndvi_gimms_dwnsc_8206_utm.tif")

### bing imaging ==========================================================
# colorimage <- stack("/media/ede/tims_ex/kilimanjaro_sp1_station_overview/kili_from_bing.tif")
# colim.recl <- reclassify(colorimage, cbind(NA, 1))
# colim.recl[colim.recl < 0] <- 1
#
ext1 <- extent(c(322842, 329832, 9634358, 9640018))
#ext2 <- extent(c(311257, 318314, 9649975, 9655670))

rst1 <- raster(ext1)
rst1[] <- rnorm(100, 2, 1)
projection(rst1) <- projection(cubist)
rst1_ll <- projectRaster(rst1, crs = "+init=epsg:4326")
ext1_ll <- extent(rst1_ll)

xmin <- ext1_ll@xmin
ymin <- ext1_ll@ymin
xmax <- ext1_ll@xmax
ymax <- ext1_ll@ymax

ul <- c(ymax, xmin)
lr <- c(ymin, xmax)

omap.merc <- openmap(ul, lr, type = "bing", zoom = 16)
#plot(omap.merc)

omap.utm <- openproj(omap.merc, projection(cubist))
omap.utm.rst <- raster(omap.utm)
writeRaster(omap.utm.rst, "data/ki_bing_zoom_mruzunga.tif", overwrite = TRUE)

lyr <- ogrListLayers("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
                 layer = lyr)
proj4string(plots) <- "+init=epsg:21037"
plots <- spTransform(plots, CRS(projection(cubist)))
plots_amp <- subset(plots, plots$PoleType == "AMP")
### =======================================================================

c_min_k <- cubistOK - krigD

cubist_mrzunga <- crop(cubist, ext1)
cubistOK_mrzunga <- crop(cubistOK, ext1)
krig_mrzunga <- crop(krigD, ext1)
c_min_k_mrzunga <- crop(c_min_k, ext1)
ndvi_mrzunga <- resample(ndvi, cubist_mrzunga)

plots_amp_mruzunga <- crop(plots_amp, extent(cubist_mrzunga))


omap.utm.rst <- raster("data/ki_bing_zoom_mruzunga.tif")
#omap.utm.rst <- crop(omap.utm.rst, ext1)
omap.utm.rst <- resample(omap.utm.rst, cubist_mrzunga)
colim.recl <- omap.utm.rst
col.im <- colim.recl * 2
col.im.re <- reclassify(col.im, cbind(255, 9999, 255))

colim.recl <- col.im.re

### use downloaded map for sp raster layout definition
cols <- rgb(colim.recl[[1]][] / 255,
            colim.recl[[2]][] / 255,
            colim.recl[[3]][] / 255)

map.cols <- matrix(cols,
                   nrow = colim.recl@nrows,
                   ncol = colim.recl@ncols)

attr(map.cols, "class") <- c("ggmap", "raster")
attr(map.cols, "bb") <- data.frame(ll.y = colim.recl@extent@ymin,
                                   ll.x = colim.recl@extent@xmin,
                                   ur.y = colim.recl@extent@ymax,
                                   ur.x = colim.recl@extent@xmax)

bbMap <- attr(map.cols, 'bb')
latCenter <- with(bbMap, ll.y + ur.y)/2
lonCenter <- with(bbMap, ll.x + ur.x)/2
height <- with(bbMap, ur.y - ll.y)
width <- with(bbMap, ur.x - ll.x)

## Use sp.layout of spplot: a list with the function and its
## arguments
sp.raster <- list('grid.raster', map.cols,
                  x=lonCenter, y=latCenter,
                  width=width, height=height,
                  default.units='native')


### final plottng
clrs <- colorRampPalette(c("snow",
                           rev(brewer.pal(11, "Spectral")),
                           "black"))

rnbw <- envinmrPalette

clrs_dev <- colorRampPalette(brewer.pal(9, "RdBu"))
clrs_ndvi <- colorRampPalette(brewer.pal(9, "YlGn"))

cubist_p <- spplot(cubist_mrzunga, col.regions = rnbw(1000),
                   alternating = 3, at = seq(13, 25, 0.1),
                   maxpixels = ncell(cubist_mrzunga),
                   scales = list(draw = TRUE, y = list(rot = 90),
                                 alternating = 3),
                   colorkey = list(space = "top", width = 1, height = 0.75),
                   main = "Temperature [°C]",
                   panel = function(x, y, z, ...) {
                     panel.levelplot(x, y, z, ...)
                     panel.text(x = 329500, y = 9639700, adj = c(1, 0.5),
                                labels = c("a) cubist"))
                   })

krig_p <- spplot(krig_mrzunga, col.regions = rnbw(1000),
                   alternating = 3, at = seq(13, 25, 0.1),
                   maxpixels = ncell(krig_mrzunga),
                 panel = function(x, y, z, ...) {
                   panel.levelplot(x, y, z, ...)
                   panel.text(x = 329500, y = 9639700, adj = c(1, 0.5),
                              labels = c("b) kriging"))
                 })

cubistOK_p <- spplot(cubistOK_mrzunga, col.regions = rnbw(1000),
                     alternating = 3, at = seq(13, 25, 0.1),
                     maxpixels = ncell(cubistOK_mrzunga),
                     panel = function(x, y, z, ...) {
                       panel.levelplot(x, y, z, ...)
                       panel.text(x = 329500, y = 9639700, adj = c(1, 0.5),
                                  labels = c("c) cubistOK"))
                     })

c_min_k_p <- spplot(c_min_k_mrzunga, col.regions = rev(clrs_dev(1000)),
                   alternating = 3, at = seq(-5, 5, 0.1),
                   maxpixels = ncell(c_min_k_mrzunga),
                   panel = function(x, y, z, ...) {
                     panel.levelplot(x, y, z, ...)
                     panel.text(x = 329500, y = 9639700, adj = c(1, 0.5),
                                labels = c("d) cubistOK - kriging"))
                   })

ndvi_p <- spplot(ndvi_mrzunga, col.regions = clrs_ndvi(1000),
                 alternating = 3, at = seq(0.6, 1, 0.001),
                 maxpixels = ncell(ndvi_mrzunga),
                 panel = function(x, y, z, ...) {
                   panel.levelplot(x, y, z, ...)
                   panel.text(x = 329500, y = 9639700, adj = c(1, 0.5),
                              labels = c("e) NDVI"))
                 })

plots_amp_mruzunga@bbox <- bbox(extent(cubist_mrzunga))
plots_amp_mruzunga$V <- as.factor("A")

pts_sp <- spplot(plots_amp_mruzunga, zcol = "V",
                 xlim = c(extent(cubist_mrzunga)@xmin, extent(cubist_mrzunga)@xmax),
                 ylim = c(extent(cubist_mrzunga)@ymin, extent(cubist_mrzunga)@ymax),
                 sp.layout = list(sp.raster), col.regions = "transparent")

p_list <- list(cubist_p, krig_p, cubistOK_p, c_min_k_p, ndvi_p, pts_sp)

out <- latticeCombineGrid(p_list, layout = c(2, 3), x.same = TRUE)

png("graphs/mrzunga_zoom.png", width = 35, height = 40, units = "cm", res = 300)
grid.newpage()
vp0 <- viewport(x = 0.5, y = 0.5,
                height = 0.9, width = 0.9,
                just = c("centre", "centre"),
                name = "global_vp", clip = FALSE)
pushViewport(vp0)
print(out, newpage = FALSE)
downViewport(trellis.vpname(name = "figure"))
vp1 <- viewport(x = 0.5, y = 0,
                height = 0.1, width = 0.75,
                just = c("centre", "top"),
                name = "map_vp", clip = FALSE)
pushViewport(vp1)
draw.colorkey(key = list(col = clrs_ndvi(1000), width = 1,
                         at = seq(0.6, 1, 0.001),
                         space = "bottom"), draw = TRUE)
upViewport()
vp2 <- viewport(x = 0.5, y = 0,
                height = 0.08, width = 0.075,
                just = c("centre", "top"),
                name = "key.vp")
pushViewport(vp2)
grid.text("NDVI", x = 0.5, y = 0, just = c("centre", "top"),
          gp = gpar(cex = 1.2, fontface = "bold"))
upViewport(0)

downViewport(trellis.vpname(name = "panel", 2, 2, clip.off = TRUE))
vp3 <- viewport(x = 1, y = 0,
                height = 1, width = 0.3,
                just = c("left", "bottom"),
                name = "key_dev_vp", clip = FALSE)
pushViewport(vp3)
draw.colorkey(key = list(col = rev(clrs_dev(1000)), width = 1,
                         at = seq(-5, 5, 0.1),
                         space = "right"), draw = TRUE)

vp4 <- viewport(x = 1, y = 0.5,
                height = 0.2, width = 0.2,
                just = c("centre", "centre"),
                angle = 270,
                name = "key_dev_title_vp", clip = FALSE)
pushViewport(vp4)
#grid.rect()
grid.text("Temperature [K]", x = 0.5, y = 0, just = c("centre", "bottom"),
          gp = gpar(cex = 1.2, fontface = "bold"))
dev.off()
