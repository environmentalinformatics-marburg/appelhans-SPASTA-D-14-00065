library(raster)
library(latticeExtra)
library(rgdal)
library(Rsenal)
library(grid)
library(Rtography)
library(gridExtra)
library(gnumeric)

path <- "/media/tims_ex/kilimanjaro_ta200_interp/interp_maps_server"
setwd(path)

### create annual data ====================================================
fldrs <- list.dirs()
fldrs <- fldrs[-1]
fls <- lapply(fldrs, function(i) list.files(i, full.names = TRUE))
# fls <- list.files("cubist", pattern = glob2rx("*annual*"), 
#                   full.names = TRUE)
# stcks <- stack(fls[5:40])
stcks <- lapply(seq(fls), function(i) stack(fls[[i]][5:40]))

means <- lapply(seq(stcks), function(i) {
  calc(stcks[[i]], fun = mean, na.rm = TRUE)
})
# means <- calc(stcks, fun = mean, na.rm = TRUE)

# means_web_merc <- lapply(seq(means), function(i) {
#   projectRaster(means[[i]], crs = "+init=epsg:3857")
# })

mthd <- lapply(seq(fls), function(i) {
  strsplit(fls[[i]], split = "/")[[1]][[2]]
})

nms <- paste("ki_Ta_200_", mthd, "_interp_annual_UTM37s_WGS84.tif", sep = "")
nms <- paste("ki_Ta_200_", "cubist_annual_ndvi", "_interp_annual_UTM37s_WGS84.tif", sep = "")

for (i in seq(nms)) {
  writeRaster(means[[i]], nms[i], overwrite = TRUE)
}
### =======================================================================

### visualise annual maps =================================================
fls <- list.files(".", pattern = glob2rx("*interp_annual*.tif"))
fls <- fls[c(5, 9, 12, 1, 10)]

stck <- stack(fls)

shp <- "/media/tims_ex/kilimanjaro_plots_dem/NP_boder_shp/kinapa_boundary_polygon.shp"
lyr <- ogrListLayers(shp)
np_border <- readOGR(shp, layer = lyr)
np_border_wgs <- spTransform(np_border, CRS(projection(stck)))

ki_dem_utm <- raster("../../kilimanjaro_plots_dem/Plots_DEM_final/DEM_UTM37S_WGS84_30m_Hemp.tif")

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

clrs_ta <- colorRampPalette(c("snow", 
                              rev(brewer.pal(11, "Spectral")),
                              "black"))

rnbw <- colorRampPalette(c("snow", "#413B93", "#4065B1", "#488BC2", 
                           "#55A1B1", "#63AD99", "#7FB972", "#B5BD4C", 
                           "#D9AD3C", "#E68E34", "#E6642C", "#D92120",
                           "black"))

np <- spplot(np_border_wgs, zcol = "NEW_SEG", 
             col.regions = "transparent", lty = 1, lwd = 2)

mthd <- lapply(seq(fls), function(i) {
  strsplit(fls[[i]], split = "_")[[1]][[4]]
})

nms <- paste(letters[1:5], ") ", mthd, sep = "")

p_list <- lapply(seq(nlayers(stck)), function(i) {
  ta <- spplot(stck[[i]], col.regions = rnbw(1000), 
               at = seq(-8.5, 27, 0.1),
               maxpixels = ncell(stck[[i]]), 
               scales = list(draw = TRUE, alternating = 3, 
                             y = list(rot = 90)),
               colorkey = list(space = "top", width = 1, height = 0.75),
               mai = "Temperature [°C]",
               panel = function(x, y, ...) {
                 panel.levelplot(x, y, ...)
                 panel.text(x = 352500, y = 9662500, 
                            labels = nms[i],
                            adj = c(1, 0.5))
               })
  
  out_ta <- ta + #as.layer(np) +
    as.layer(cp, x.same = TRUE, y.same = TRUE)
  
  return(out_ta)
})

out_p <- latticeCombineGrid(p_list, layout = c(2, 3))

png("ta_area-wide_annual_ml_comparison.png",
    width = 30, height = 40, res = 300, units = "cm")
plot.new()
print(out_p)
dev.off()
### =======================================================================
## same but with overview map ---------------------------------------------
colorimage <- stack("../../kilimanjaro_sp1_station_overview/kili_from_bing.tif")

colim.recl <- reclassify(colorimage, cbind(NA, 1))
colim.recl[colim.recl < 0] <- 1

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

### hardy location data
x_hardy <- 37.35458
y_hardy <- -3.05793
z_hardy <- 5776

hardy_loc <- data.frame(PlotID = "kib0", X = x_hardy, Y = y_hardy, 
                        Z_DEM_HMP = z_hardy)
coordinates(hardy_loc) <- ~ X + Y
projection(hardy_loc) <- "+init=epsg:4326"
hardy_loc <- spTransform(hardy_loc, CRS("+init=epsg:21037"))
hardy_loc@data <- data.frame(EASTING = coordinates(hardy_loc)[, 1], 
                             NORTHING = coordinates(hardy_loc)[, 2],
                             TYPE = "kiboplateau",
                             PLOTID = "kib0", 
                             LOGGER = "000hdy",
                             SERIAL = 10000000000,
                             DATE_START = "2007-10-01",
                             DATE_END = "2013-08-01",
                             SENSOR = "XXXXXX",
                             PARAMETER = "T/rH",
                             PROVIDER = "KiLi-SP1")

sheets <- c("ta")

stns <- lapply(seq(sheets), function(i) {
  dat <- read.gnumeric.sheet("../../kilimanjaro_sp1_station_overview/ki_config_station_inventory_detail.ods",
                             sheet = sheets[i], stringsAsFactors = TRUE, 
                             head = TRUE)
  dat <- dat[!is.na(dat$EASTING), ]
  dat <- subset(dat, dat$DATE_END == "2099-12-31")
  dat <- as.data.frame(rbind(dat, hardy_loc@data))
  coordinates(dat) <- c("EASTING", "NORTHING")
  dat@proj4string <- CRS("+init=epsg:21037")
  dat <- spTransform(dat, CRS(projection(colim.recl)))
  nms <- rownames(dat@bbox)
  dat@bbox <- as.matrix(extent(colim.recl))
  rownames(dat@bbox) <- nms
  return(dat)
})

### get elevation from dem
# dem <- raster("../kiliDEM/in/dem_hemp_utm37s1.tif/dem_hemp_utm37s1.tif")
# ele <- lapply(seq(stns), function(i) {
#   data.frame(plotid = stns[[i]]$PLOTID,
#              elevation = extract(dem, stns[[i]]))
# })

labels <- c("Station locations")

clrs <- list("white")

stns[[1]]@bbox <- bbox(extent(stck))

trh <- spplot(stns[[1]]["PROVIDER"], col.regions = clrs[[1]],
              sp.layout = list(sp.raster), xlim = stns[[1]]@bbox[1, ],
              ylim = stns[[1]]@bbox[2, ], as.table = TRUE, cex = 1)

trh <- Rtographize(trh, point.type = "rectangles", alpha = 0.8,
                   point.size = 1.2)

cp_black <- levelplot(z ~ x * y, 
                      at = seq(500, 6000, 500), colorkey = FALSE,
                      panel = function(...) {
                        panel.smoothconts(contours = TRUE, 
                                          col = "black", ...)
                      })

trh <- trh +
  as.layer(cp_black, x.same = TRUE, y.same = TRUE)

p_list_map <- append(p_list, list(trh))

out_p <- latticeCombineGrid(p_list_map, layout = c(2, 3))

png("ta_area-wide_annual_ml_comparison_incl_overview_map.png",
    width = 30, height = 40, res = 300, units = "cm")
plot.new()
print(out_p)
dev.off()
## ------------------------------------------------------------------------
### =======================================================================

### visualise differences to kriging ======================================
diff_stck <- stack(lapply(1:4, function(i) {
  stck[[i]] - stck[[5]]
}))

clrs_diff <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

nms_diff <- paste(letters[1:5], ") ", mthd, " - kriging", sep = "")

p_list <- lapply(seq(nlayers(diff_stck)), function(i) {
  ta <- spplot(diff_stck[[i]], col.regions = clrs_diff(1000), 
               at = seq(-10, 10, 0.1),
               maxpixels = ncell(diff_stck[[i]]), 
               scales = list(draw = TRUE, y = list(rot = 90)),
               colorkey = list(space = "top", width = 1, height = 0.75),
               mai = "Temperature [K]",
               panel = function(x, y, ...) {
                 grid.rect(gp = gpar(fill = "black"))
                 panel.levelplot(x, y, ...)
                 panel.text(x = 352500, y = 9662500, 
                            labels = nms_diff[i],
                            adj = c(1, 0.5))
               })
  
  out_ta <- ta + #as.layer(np) +
    as.layer(cp, x.same = TRUE, y.same = TRUE)
  
  return(out_ta)
})

out_p <- latticeCombineGrid(p_list, layout = c(2, 2))

png("ta_area-wide_annual_ml_minus_kriging.png",
    width = 30, height = 30, res = 300, units = "cm")
plot.new()
print(out_p)
dev.off()