library(raster)
library(plotKML)
library(rgdal)
library(Rsenal)

source("set_wd.R")

ptrn <- "ki*varselOK_2*.tif"
fls <- list.files("interp_maps/cubist", full.names = TRUE,
                  pattern = glob2rx(ptrn))

### raster data
stck <- stack(fls)

st <- as.POSIXct("2014-01-01 00:00:00")
nd <- as.POSIXct("2014-12-31 00:00:00")

ts_st <- seq(st, nd, "months")
ts_nd <- seq(st, nd, "months")


### point data
load("data/ta.pred.complete.rda")

ind <- duplicated(ta.pred$plotID)
ta.pred <- ta.pred[!ind, ]
coordinates(ta.pred) <- ~ x + y
plots <- ta.pred
# lyr <- ogrListLayers("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
# plots <- readOGR("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
#                  layer = lyr)
proj4string(plots) <- "+init=epsg:21037"
#plots <- subset(plots, PoleType == "AMP")
#plots_ll <- spTransform(plots, CRS = CRS("+proj=longlat +datum=WGS84"))
#plots_ll <- subset(plots_ll, PoleType == "AMP")


### reproject to lat lon
brck <- brick(stck)
brck@title <- "Monthly temperatures interpolated with cubistOK for 2014"
#brck_ll <- projectRaster(brck, crs = projection(plots_ll))


### RasterBrickTimeSeries
cubist_ts <- new("RasterBrickTimeSeries",
                 variable = "resp",
                 sampled = plots, #plots_ll,
                 rasters = brck, #brck_ll,
                 TimeSpan.begin = ts_st,
                 TimeSpan.end = ts_nd)

setwd("months_2014_kml")

plotKML(cubist_ts,
        colour_scale = envinmrPalette(20),
        folder.name = "months_2014_kml")




