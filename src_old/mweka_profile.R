library(rgdal)
library(raster)
library(latticeExtra)
library(Rsenal)
library(gridExtra)
library(maptools)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)

prof_fls <- list.files("profiles", pattern = ".shp", full.names = TRUE)

profs <- lapply(prof_fls, function(i) {
  lyr <- ogrListLayers(i)
  prof <- readOGR(i, layer = lyr)
})

dem <- raster("../kilimanjaro_plots_dem/Plots_DEM_final/DEM_UTM37S_WGS84_30m_Hemp.tif")
cubist <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_cubist_interp_annual_UTM37s_WGS84.tif")
krig <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_kriging_interp_annual_UTM37s_WGS84.tif")
cubistOK <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_cubistOK_interp_annual_UTM37s_WGS84.tif")
gbm <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_gbm_interp_annual_UTM37s_WGS84.tif")
rf <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_rf_interp_annual_UTM37s_WGS84.tif")
avnnet <- raster("../kilimanjaro_ta200_interp/interp_maps_server/ki_Ta_200_avNNet_interp_annual_UTM37s_WGS84.tif")
ndvi <- raster("in/ki_month_mean_ndvi_gimms_dwnsc_8206_30m_utm37s_wgs84_annual.tif")
# slp <- raster("in/DEM_UTM37S_WGS84_30m_Hemp_slope.tif")
# asp <- raster("in/DEM_UTM37S_WGS84_30m_Hemp_aspect.tif")
# svf <- raster("in/DEM_UTM37S_WGS84_30m_Hemp_sky_view_factor.tif")

sp1_ta200 <- read.csv("../kilimanjaro_ta200_rh200_interp/results/points/ta_200/envin-umr_ki_ta_all-plots_aggregated-annually_v010.csv")
plots_lyr <- ogrListLayers("../kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("../kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp", layer = plots_lyr)
proj4string(plots) <- "+init=epsg:21037"
plots <- spTransform(plots, CRS(projection(dem)))
plots_amp <- subset(plots, plots$PoleType == "AMP")

theplots <- list(marangu = c("hel4", "fpo5", "fod4", 
                             "hom5", "gra5", "sav3"),
                 maua = c("fed3", "fed5", "fed4", "fpd4", "fpo4", 
                          "fod3", "cof5", "hom4", "sav2"),
                 mweka = c("cof4", "cof6", "flm2", "fod2", "foc5", 
                           "fpo2", "fpo3", "mwh0", "fed2", "hel3"))

panel_txt <- c("c)", "b)", "a)")

profiles_p <- lapply(seq(profs), function(i) {
  x <- i
  print(x)
  sp1_ta200_profile <- sp1_ta200[sp1_ta200$PlotID %in% theplots[[x]], ]
  sp1_ta200_profile <- subset(sp1_ta200_profile, sp1_ta200_profile$Source == "O")
  sp1_ta200_profile_scaled <- sort(sp1_ta200_profile$MAT / 
    max(sp1_ta200_profile$MAT, na.rm = TRUE))

  plots_amp_elevation <- plots_amp@data$Z_DEM_HMP[plots_amp@data$PlotID %in% 
                                                    sp1_ta200_profile$PlotID]
  plots_amp_elevation <- rev(sort(plots_amp_elevation))

  sp1_ta200_profile <- sort(sp1_ta200_profile$MAT)
  
  elevation_profile <- extract(dem, profs[[x]])[[1]]
  elevation_index_scaled <- seq_along(elevation_profile) / 
    max(seq_along(elevation_profile), na.rm = TRUE)
  
  cubist_profile <- extract(cubist, profs[[x]])[[1]]
  cubist_profile_scaled <- cubist_profile / max(cubist_profile, na.rm = TRUE)
  
  krig_profile <- extract(krig, profs[[x]])[[1]]
  krig_profile_scaled <- krig_profile / max(krig_profile, na.rm = TRUE)
  
  cubistOK_profile <- extract(cubistOK, profs[[x]])[[1]]
  cubistOK_profile_scaled <- cubistOK_profile / max(cubistOK_profile, na.rm = TRUE)
  
  gbm_profile <- extract(gbm, profs[[x]])[[1]]
  gbm_profile_scaled <- gbm_profile / max(gbm_profile, na.rm = TRUE)
  
  rf_profile <- extract(rf, profs[[x]])[[1]]
  rf_profile_scaled <- rf_profile / max(rf_profile, na.rm = TRUE)
  
  avnnet_profile <- extract(avnnet, profs[[x]])[[1]]
  avnnet_profile_scaled <- avnnet_profile / max(avnnet_profile, na.rm = TRUE)
  
  ndvi_profile <- extract(ndvi, profs[[x]])[[1]]
  ndvi_profile_scaled <- ndvi_profile / max(ndvi_profile, na.rm = TRUE)
#   
#   slp_profile <- extract(slp, profs[[x]])[[1]]
#   slp_profile_scaled <- slp_profile / max(slp_profile, na.rm = TRUE)
#   
#   asp_profile <- extract(asp, profs[[x]])[[1]]
#   asp_profile_scaled <- asp_profile / max(asp_profile, na.rm = TRUE)
#   
#   svf_profile <- extract(svf, profs[[x]])[[1]]
#   svf_profile_scaled <- svf_profile / max(svf_profile, na.rm = TRUE)
  
  clrs <- brewer.pal(6, "Set1")
  
#   plot(elevation_profile ~ cubist_profile_scaled, type = "l", xlim = c(0, 1))
#   lines(elevation_profile ~ krig_profile_scaled, col = "red", xlim = c(0, 1))
#   lines(elevation_profile ~ gbm_profile_scaled, col = "blue", xlim = c(0, 1))
#   lines(elevation_profile ~ rf_profile_scaled, col = "purple", xlim = c(0, 1))
#   lines(elevation_profile ~ avnnet_profile_scaled, col = "brown", xlim = c(0, 1))
#   lines(elevation_profile ~ ndvi_profile_scaled, col = "darkgreen", xlim = c(0, 1))
#   points(plots_amp_elevation ~ sp1_ta200_profile_scaled, col = "black", 
#          xlim = c(0, 1), pch = 20)
#   
#   plot(elevation_profile ~ cubist_profile, type = "l", xlim = c(0, 25))
#   lines(elevation_profile ~ krig_profile, col = "red", xlim = c(0, 25))
#   lines(elevation_profile ~ gbm_profile, col = "blue", xlim = c(0, 25))
#   lines(elevation_profile ~ rf_profile, col = "purple", xlim = c(0, 25))
#   lines(elevation_profile ~ avnnet_profile, col = "brown", xlim = c(0, 25))
#   points(plots_amp_elevation ~ sp1_ta200_profile$MAT, col = "black", 
#          xlim = c(0, 25), pch = 20)
  
  ele_p <- xyplot(elevation_profile ~ elevation_index_scaled,
                  type = "l", col = "grey30", lwd = 2)
  
  cubist_p <- xyplot(elevation_profile ~ cubist_profile_scaled, 
                     scales = list(alternating = 1),
                     xlim = c(-0.1, 1.1), ylim = c(650, 4650),
                     type = "l", col = clrs[1], lwd = 2, asp = 1.5,
                     xlab = "Scaled Profiles", ylab = "Elevation [m]",
                     xscale.components = xscale.components.subticks,
                     yscale.components = yscale.components.subticks,
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.text(x = 1, y = 4400,
                                  labels = panel_txt[i])
                       })
  krig_p <- xyplot(elevation_profile ~ krig_profile_scaled, 
                   type = "l", col = clrs[2], lwd = 2)
  cubistOK_p <- xyplot(elevation_profile ~ cubistOK_profile_scaled, 
                       type = "l", col = clrs[3], lwd = 2)
  gbm_p <- xyplot(elevation_profile ~ gbm_profile_scaled, 
                  type = "l", col = clrs[6], lwd = 2)
  rf_p <- xyplot(elevation_profile ~ rf_profile_scaled,
                 type = "l", col = clrs[5], lwd = 2)
  avnnet_p <- xyplot(elevation_profile ~ avnnet_profile_scaled,
                     type = "l", col = clrs[4], lwd = 2)
  points_p <- xyplot(plots_amp_elevation ~ sp1_ta200_profile_scaled, 
                     col = "black", pch = 20, cex = 2)
  ndvi_p <- xyplot(elevation_profile ~ ndvi_profile_scaled,
                   type = "l", col = "black", lwd = 2, lty = 3)
#   slp_p <- xyplot(elevation_profile ~ slp_profile_scaled,
#                   type = "l", col = "darkgreen", lwd = 2, lty = 3)
#   asp_p <- xyplot(elevation_profile ~ asp_profile_scaled,
#                   type = "l", col = "darkblue", lwd = 2, lty = 3)
#   svf_p <- xyplot(elevation_profile ~ svf_profile_scaled,
#                   type = "l", col = "darkred", lwd = 2, lty = 3)  

  all_p <- latticeCombineLayer(list(cubist_p, krig_p,
                                    #gbm_p, rf_p, 
                                    cubistOK_p, #avnnet_p, 
                                    points_p, ndvi_p, ele_p))

  #out <- latticeCombineLayer(list(all_p, ele_p))

  return(all_p)
})

all_p <- latticeCombineGrid(rev(profiles_p))

ki_dem_utm <- crop(dem, extent(cubist))

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

sp1_ta200_prof <- sp1_ta200[sp1_ta200$PlotID %in% do.call("c", theplots), ]
sp1_ta200_prof_obs <- subset(sp1_ta200_prof, sp1_ta200_prof$Source == "O")

plots_amp_prof <- plots_amp[plots_amp$PlotID %in% sp1_ta200_prof_obs$PlotID, ]

plots_p <- spplot(plots_amp_prof, zcol = "PoleType", pch = 21, 
                  col.regions = "black")

prof1 <- as(profs[[1]], "SpatialLines")
prof2 <- as(profs[[2]], "SpatialLines")
prof3 <- as(profs[[3]], "SpatialLines")
slot(prof1@lines[[1]], "ID") <- "1"
slot(prof2@lines[[1]], "ID") <- "2"
slot(prof3@lines[[1]], "ID") <- "3"
prof12 <- spRbind(prof1, prof2)
prof123 <- spRbind(prof12, prof3)

lout <- list("sp.lines", prof123, lwd = 2,
             col = "grey30", grid = TRUE)

clrs_dem <- terrain.colors #colorRampPalette(c("snow", 
                         #      rev(brewer.pal(11, "Spectral")),
                          #     "black"))

txt_x <- c(plots_amp$X[plots_amp$PlotID == "cof4"],
           plots_amp$X[plots_amp$PlotID == "sav2"],
           plots_amp$X[plots_amp$PlotID == "sav3"])

txt_y <- c(plots_amp$Y[plots_amp$PlotID == "cof4"],
           plots_amp$Y[plots_amp$PlotID == "sav2"],
           plots_amp$Y[plots_amp$PlotID == "sav3"])

dem_p <- spplot(ki_dem_utm, col.regions = clrs_dem(10000), 
                at = seq(500, 6000, 1), alternating = 3,
                maxpixels = ncell(ki_dem_utm), 
                scales = list(draw = TRUE, y = list(rot = 90)),
                colorkey = list(space = "top", width = 1, height = 0.75),
                mai = "Elevation [m]", panel = function(x, y, z, ...) {
                  panel.levelplot(x, y, z, ...)
                  for (i in seq(txt_x)) {
                    panel.text(x = txt_x[i] + 1000, y = txt_y[i],
                             labels = rev(panel_txt)[i])
                    }
                  })

out_dem <- dem_p + #as.layer(np) +
  as.layer(cp, x.same = TRUE, y.same = TRUE) +
  as.layer(plots_p) +
  layer(sp.lines(prof123))

png("profiles.png", width = 40, height = 45, units = "cm", res = 300)
grid.newpage()
vp1 <- viewport(x = 0, y = 1, 
                height = 0.5, width = 1,
                just = c("left", "top"),
                name = "map_vp")
pushViewport(vp1)
plot.new()
print(out_dem, newpage = FALSE)
upViewport(1)

vp2 <- viewport(x = 0, y = 0, 
                height = 0.5, width = 1,
                just = c("left", "bottom"),
                name = "profile_vp")
pushViewport(vp2)

print(all_p, newpage = FALSE)
downViewport(trellis.vpname(name = "panel", 1, 1))
#grid.rect()
vp1 <- viewport(x = 0.1, y = 0, 
                height = 0.3, width = 0.3,
                just = c("left", "bottom"),
                name = "legend.vp")

pushViewport(vp1)
clrs <- brewer.pal(6, "Set1")
draw.key(key = list(col = c(clrs[c(1, 2, 3)], "black", "grey30"),
                    lines = list(lty = c(rep(1, 3), 3, 1)),
                    lwd = rep(2, 5), 
                    text = list(c("cubist", "kriging", "cubistOK",
                                  "NDVI", "Elevation"), 
                                col = "black")), draw = TRUE)
upViewport(0)
dev.off()
