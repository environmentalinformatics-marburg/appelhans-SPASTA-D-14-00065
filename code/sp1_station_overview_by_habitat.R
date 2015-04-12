library(raster)
library(latticeExtra)
library(Rtography)
library(gridExtra)
library(gnumeric)
library(stargazer)
library(reshape2)
library(rgdal)
library(dplyr)

source("code/gridMapLegend_n.R")

path <- "~/tappelhans/uni/temp"
setwd(path)

colorimage <- stack("data/kili_from_bing.tif")

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


### stations
## point location data
lyr <- ogrListLayers("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
                 layer = lyr)
proj4string(plots) <- "+init=epsg:21037"
plots <- spTransform(plots, CRS(projection(colorimage)))
plots <- subset(plots, PoleType == "AMP")
plots@data$X <- coordinates(plots)[, 1]
plots@data$Y <- coordinates(plots)[, 2]

### hardy location data
x <- 37.35458
y <- -3.05793
z <- 5776

hardy_loc <- data.frame(PlotID = "kib0", X = x, Y = y, Z_DEM_HMP = z)
coordinates(hardy_loc) <- ~ X + Y
projection(hardy_loc) <- "+init=epsg:4326"
hardy_loc <- spTransform(hardy_loc, CRS(projection(plots)))
hardy_loc@data <- data.frame(PlotID = "kib0",
                             PoleName = "A middle Pole",
                             PoleType = "AMP",
                             X = coordinates(hardy_loc)[, 1],
                             Y = coordinates(hardy_loc)[, 2],
                             Z_GPS = z,
                             Z_DEM_HMP = z)

plots <- rbind(plots, hardy_loc)
plots$lg <- factor(substr(plots@data$PlotID, 1, 3),
                   levels = unique(substr(plots@data$PlotID, 1, 3)))

### load and merge temperature relevant locations
ta.monthly <- read.csv("data/plots.csv", stringsAsFactors = FALSE)
ta.monthly <- ta.monthly[!ta.monthly$plotID %in% c("mch0", "nkw1", "sun1",
                                                   "sun2", "sun3", "sun4"), ,
                         drop = TRUE]

ta.monthly$tg <- substr(ta.monthly$datetime, 1, 7)
ta.monthly$lg <- substr(ta.monthly$plotID, 1, 3)

ta.monthly.c <- ta.monthly[complete.cases(ta.monthly),]
ta.monthly.c.s <- split(ta.monthly.c, ta.monthly.c$tg)

n <- data.frame(dplyr::count(ta.monthly.c, group = tg, sort=TRUE))

ta.monthly.c.s.m <- ta.monthly.c.s[names(ta.monthly.c.s) %in% n$group[n$n >= 25]]
ta.monthly.c.s.m <- do.call("rbind", ta.monthly.c.s.m)
ta.monthly.c.s.m <- ta.monthly.c.s.m[, c(1, 4:6)]
rownames(ta.monthly.c.s.m) <- NULL


### read doug hardys data and merge with ours
hardy <- read.csv("data/from_doug/kib0_doug_hardy_0714.csv",
                  stringsAsFactors = FALSE)
names(hardy) <- c("Yearmon", "Ta_200", "nvaliddays", "plotID", "StationId")
hardy <- hardy[, -c(3, 5)]
hardy$tg <- as.Date(hardy$Yearmon)
hardy$tg <- substr(hardy$tg, 1, 7)
hardy$lg <- "kib"
hardy <- hardy[, c(3, 2, 4, 5), ]

hardy.c <- hardy[complete.cases(hardy),]
hardy.c.s <- split(hardy.c, hardy.c$tg)
hardy.c.s.m <- hardy.c.s[names(hardy.c.s) %in% n$group[n$n >= 25]]
hardy.c.s.m <- do.call("rbind", hardy.c.s.m)

ta.monthly <- rbind(ta.monthly.c.s.m, hardy.c.s.m)
ta.monthly$Month <- as.integer(substr(ta.monthly$tg, 6, 7))
ta.monthly$Datetime <- ta.monthly$tg

ta.monthly <- ta.monthly[complete.cases(ta.monthly), ]
ta.monthly.lst <- split(ta.monthly, ta.monthly$tg)

ind <- do.call("c", lapply(ta.monthly.lst, function(x) {
  any(unique(x$lg) %in% "kib")
}))

ta.monthly <- do.call("rbind", ta.monthly.lst[ind])

## merge with plot shp
plots.ta.monthly <- merge(ta.monthly, plots, by.x = "plotID",
                          by.y = "PlotID", all.x = TRUE)
plots.ta.monthly <- plots.ta.monthly[!is.na(plots.ta.monthly$X), ]
coordinates(plots.ta.monthly) <- ~ X + Y
proj4string(plots.ta.monthly) <- projection(plots)

data("kili12")

plots.ta.monthly@data$lg.x <- factor(plots.ta.monthly@data$lg.x,
                                     levels = c("kib",
                                                rev(levels(kili12$Habitat))))
levels(plots.ta.monthly@data$lg.x) <- rev(c("Maize field", "Savanna", "Coffee plantation",
                                            "Chagga homegarden", "Grassland",
                                            "Lower montane forest",
                                            "Ocotea forest disturbed", "Ocotea forest",
                                            "Podocarpus forest", "Podocarpus forest dist.",
                                            "Ericacea forest", "Helychrysum",
                                            "Kibo summit"))

## crop grids to research area extent
ext <- extent(bbox(plots))
plots.ta.monthly@bbox[1, 1] <- ext@xmin - 3000
plots.ta.monthly@bbox[1, 2] <- ext@xmax + 1000
plots.ta.monthly@bbox[2, 1] <- ext@ymin - 1000
plots.ta.monthly@bbox[2, 2] <- ext@ymax + 3000
ext <- extent(bbox(plots.ta.monthly))
### plot
### load dem and produce contourplot
ki_dem_utm <- raster("data/in/DEM_UTM37S_WGS84_30m_Hemp.tif")
ki_dem_utm <- crop(ki_dem_utm, ext)

ki_dem_flipped <- flip(ki_dem_utm, "y")
x <- coordinates(ki_dem_flipped)[, 1]
y <- coordinates(ki_dem_flipped)[, 2]
z <- ki_dem_flipped[]

cp <- levelplot(z ~ x * y, alpha = 0.5,
                at = seq(500, 6000, 500), colorkey = FALSE,
                panel = function(...) {
                  Rsenal:::panel.filledcontour(contours = TRUE,
                                               fill = FALSE,
                                               col.contours = "grey70",
                                               lwd.contours = 0.5,
                                               ...)
                })

clrs <- c("snow", brewer.pal(12, "Paired"))

tmp <- spplot(plots.ta.monthly, zcol = "lg.x", col.regions = clrs,
              sp.layout = list(sp.raster), xlim = plots.ta.monthly@bbox[1, ],
              ylim = plots.ta.monthly@bbox[2, ], as.table = TRUE, cex = 0.1)

pts <- Rtographize(tmp, point.type = "rectangles", alpha = 0.8,
                   point.size = 1)


labels <- levels(plots.ta.monthly@data$lg.x)


png("graphs/sp1_stations_by_habitat.png", width = 17, height = 15,
    units = "cm", res = 600)
plot.new()
#grid.newpage()
print(pts) + as.layer(cp)

downViewport(trellis.vpname(name = "figure"))

#seekViewport("plot_01.panel.1.1.vp")
# vp0 <- viewport(x = unit(1, "npc"), y = unit(0, "npc"),
#                 width = 0.17, height = 0.04, just = c("right", "top"),
#                 name = "inset_proj_text", clip = "off", gp = gpar(cex = 1))
#
# pushViewport(vp0)
# grid.text(label = "Projection: EPSG 32737", just = "left", x = 0)
# upViewport(1)

#downViewport(trellis.vpname(name = "figure"))
vp1 <- viewport(x = unit(0.01, "npc"), y = unit(0.01, "npc"),
                width = 0.3, height = 0.1, just = c("left", "bottom"),
                name = "inset_scalebar", gp = gpar(cex = 0.5))

pushViewport(vp1)
#grid.rect(gp = gpar(fill = "transparent", col = "transparent"))
gridScaleBar(pts, addParams = list(noBins = 4,
                                    vpwidth = as.numeric(vp1$width)),
             unit = "kilometers", scale.fact = 2.2)
upViewport(1)

#seekViewport("plot_01.panel.1.1.vp")
vp2 <- viewport(x = unit(0.01, "npc"), y = unit(0.97, "npc"),
                width = 0.05, height = 0.15, just = c("left", "top"),
                name = "inset_northarrow", gp = gpar(cex = 1))

pushViewport(vp2)
gridNorthArrow()
upViewport(1)

seekViewport("plot_01.panel.1.1.vp")
vp3 <- viewport(x = unit(0.99, "npc"), y = unit(0.98, "npc"),
                width = 0.3, height = 0.035 * length(clrs),
                just = c("right", "top"),
                name = "inset_legend")

pushViewport(vp3)
gridMapLegend_n(labs = labels, clrs = unlist(clrs),
                type = "rectangles")
upViewport(0)
dev.off()
### =======================================================================

