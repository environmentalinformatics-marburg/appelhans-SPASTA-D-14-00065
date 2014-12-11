library(caret)
library(automap)
library(raster)
library(rgdal)
library(latticeExtra)
library(gmt)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

#load("cubist_interp_train_model_annual_ndvi.rda")

cubist <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_cubist_interp_annual_UTM37s_WGS84.tif")
krig <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_kriging_interp_annual_UTM37s_WGS84.tif")

plots_lyr <- ogrListLayers("../kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("../kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp", layer = plots_lyr)
proj4string(plots) <- "+init=epsg:21037"
plots <- spTransform(plots, CRS(projection(cubist)))
plots_amp <- subset(plots, plots$PoleType == "AMP")

sp1_ta200 <- read.csv("../kilimanjaro_ta200_rh200_interp/results/points/ta_200/envin-umr_ki_ta_all-plots_aggregated-annually_v010.csv")
sp1_ta200_valid <- subset(sp1_ta200, sp1_ta200$Source == "O")

plots_amp_valid <- plots_amp[plots_amp$PlotID %in% sp1_ta200_valid$PlotID, ]

plots_amp_valid@data = data.frame(plots_amp_valid@data, 
                                  sp1_ta200_valid[match(
                                    plots_amp_valid@data[,"PlotID"], 
                                    sp1_ta200_valid[,"PlotID"]),])

cubist_plots_amp_valid <- extract(cubist, plots_amp_valid)
obs_valid <- plots_amp_valid$MAT

resids_cubist <- obs_valid - cubist_plots_amp_valid

plots_amp_valid$cubist <- cubist_plots_amp_valid
plots_amp_valid$resids_cubist <- resids_cubist

plots_amp_valid_ll <- spTransform(plots_amp_valid, 
                                  CRS("+init=epsg:4326"))
x_coords <- coordinates(plots_amp_valid_ll)[, 1]
x_coords_exp <- expand.grid(x_coords, x_coords)
y_coords <- coordinates(plots_amp_valid_ll)[, 2]
y_coords_exp <- expand.grid(y_coords, y_coords)

dist <- mean(geodist(x_coords_exp[, 1], y_coords_exp[, 1], 
                     x_coords_exp[, 2], y_coords_exp[, 2])) * 0.5 * 1000

cubist_sp <- as(cubist, "SpatialPixelsDataFrame")

fmla <- as.formula("resids_cubist ~ 1")

krig_resids <- autoKrige(fmla, input_data = plots_amp_valid, 
                         new_data = cubist_sp, 
                         fix.values = c(NA, dist, NA))
#plot(krig_resids)

resids_rst <- raster(krig_resids[[1]])
plot(resids_rst)
writeRaster(resids_rst, "interp_maps_server/ki_Ta_200_cubist_rkiged_residuals_interp_annual_UTM37s_WGS84.tif",
            overwrite = TRUE)

resids_rst <- raster("interp_maps_server/ki_Ta_200_cubist_rkiged_residuals_interp_annual_UTM37s_WGS84.tif")

ki_dem_utm <- raster("../kilimanjaro_plots_dem/Plots_DEM_final/DEM_UTM37S_WGS84_30m_Hemp.tif")

panel.smoothconts <- function(x, y, z, at, col = "grey30", 
                              contours = T, 
                              zlevs.conts = seq(500, 6000, 500),
                              ...)
{
  stopifnot(require("gridBase"))
  z <- matrix(z,
              nrow = length(unique(x)),
              ncol = length(unique(y)))
  rownames(z) <- unique(coordinates(ki_dem_utm)[, 1])
  colnames(z) <- unique(coordinates(ki_dem_utm)[, 2])
  
  if (!is.double(z)) storage.mode(z) <- "double"
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (panel.number() > 1) par(new = TRUE)
  par(fig = gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  cpl <- current.panel.limits(unit = "native")
  plot.window(xlim = cpl$xlim, ylim = cpl$ylim,
              log = "", xaxs = "i", yaxs = "i")
  # paint the color contour regions
  
  if (isTRUE(contours)) 
    contour(as.double(do.breaks(range(as.numeric(rownames(z))), nrow(z) - 1)),
            as.double(do.breaks(range(as.numeric(colnames(z))), ncol(z) - 1)),
            z, levels = as.double(zlevs.conts), 
            add = T, cex = 1.8,
            axes = FALSE, lwd = 0.5,
            col = col, # color of the lines
            drawlabels = TRUE,
            labcex = 1 # add labels or not
    )
}

ki_dem_flipped <- flip(ki_dem_utm, "y")
x <- coordinates(ki_dem_flipped)[, 1]
y <- coordinates(ki_dem_flipped)[, 2]
z <- ki_dem_flipped[]

cp <- levelplot(z ~ x * y, 
                at = seq(500, 6000, 500), colorkey = FALSE,
                panel = function(...) {
                  panel.smoothconts(contours = TRUE, ...)
                })

clrs_dev <- colorRampPalette(brewer.pal(9, "RdBu"))
resids_p <- spplot(resids_rst, col.regions = rev(clrs_dev(1000)),
                   at = seq(-4, 4, 0.5), 
                   maxpixels = ncell(resids_rst),
                   scales = list(draw = TRUE, y = list(rot = 90),
                                 alternating = 3),
                   colorkey = list(space = "top", width = 1, height = 0.75),
                   main = "Temperature [K]")

resids_p_out <- resids_p +
  as.layer(cp, x.same = TRUE, y.same = TRUE)

png("interp_maps_server/cubist_resids_kriged.png", 
    width = 25, height = 20, units = "cm", res = 300)
plot.new()
print(resids_p_out)
dev.off()

cubistOK <- cubist + resids_rst
writeRaster(cubistOK, "interp_maps_server/ki_Ta_200_cubistOK_interp_annual_UTM37s_WGS84.tif",
            overwrite = TRUE)

clrs <- colorRampPalette(c("snow", 
                           rev(brewer.pal(11, "Spectral")),
                           "black"))
clrs_dev <- colorRampPalette(brewer.pal(9, "RdBu"))

cubist_p <- spplot(cubistOK, col.regions = clrs(1000),
                   alternating = 3, at = seq(-10, 30, 0.1),
                   maxpixels = ncell(cubistOK), 
                   scales = list(draw = TRUE, y = list(rot = 90)),
                   colorkey = list(space = "top", width = 1, height = 0.75),
                   main = "Temperature [°C]", 
                   panel = function(x, y, z, ...) {
                     panel.levelplot(x, y, z, ...)
#                      panel.text(x = 323000, y = 9634700, pos = 4,
#                                 labels = c("a) Cubist"))
                   })

cubist_p

cubistOK_minus_krig <- cubistOK - krig

c_min_k_p <- spplot(cubistOK_minus_krig, col.regions = rev(clrs_dev(1000)),
                    alternating = 3, at = seq(-10, 10, 0.1),
                    maxpixels = ncell(cubistOK_minus_krig), 
                    panel = function(x, y, z, ...) {
                      panel.levelplot(x, y, z, ...)
#                       panel.text(x = 323000, y = 9634700, pos = 4,
#                                  labels = c("c) Cubist - Kriging"))
                    })

print(c_min_k_p)

