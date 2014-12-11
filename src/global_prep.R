#library(automap)
library(raster)
library(caret)
library(rgdal)

# path <- "/media/tims_ex/kilimanjaro_ta200_interp"
# setwd(path)
path.server <- "/media/memory01/data/casestudies/kilimanjaro_ta200_interp_ephraim"
setwd(path.server)

# preparation -------------------------------------------------------------
### define response variable and cross-validation parameters
resp.var <- "Ta_200"
iters <- 100
train.size <- 0.8

### load necessary data
## grids
nms.grids <- c("dem", "slp", "svf", "asp", "ndvi")
files <- c("in/DEM_UTM37S_WGS84_30m_Hemp.tif", 
           "in/DEM_UTM37S_WGS84_30m_Hemp_slope.tif", 
           "in/DEM_UTM37S_WGS84_30m_Hemp_sky_view_factor.tif",
           "in/DEM_UTM37S_WGS84_30m_Hemp_aspect.tif")

exp.var.stck <- stack(files)

ndvi.files <- list.files("in/monthly_means", 
                         pattern = glob2rx("*utm37s_wgs84*"),
                         full.names = TRUE)
ndvi.stck <- stack(ndvi.files)


## point location data
lyr <- ogrListLayers("plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
                 layer = lyr)
proj4string(plots) <- "+init=epsg:21037"
plots <- spTransform(plots, CRS(projection(exp.var.stck)))
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

### temperature data
ta.monthly <- read.csv("sp1_monthly_Ta_200_2011_2014.csv",
                       stringsAsFactors = FALSE)

## read doug hardys data and merge with ours
hardy <- read.csv("from_doug/kib0_doug_hardy.csv", 
                  stringsAsFactors = FALSE)
names(hardy) <- c("Yearmon", "Ta_200", "nvaliddays", "PlotId", "StationId")
hardy <- hardy[, -3]
hardy$Yearmon <- as.Date(strptime(hardy$Yearmon, format = "%m/%d/%Y"))
hardy$Yearmon <- substr(hardy$Yearmon, 1, 7)
hardy$Ta_200_min <- 0
hardy$Ta_200_max <- 0

ta.monthly <- merge(ta.monthly, hardy, all = TRUE)
ta.monthly$Month <- as.integer(substr(ta.monthly$Yearmon, 6, 7))
ta.monthly$Datetime <- ta.monthly$Yearmon

## merge with plot shp
plots.ta.monthly <- merge(ta.monthly, plots, by.x = "PlotId",
                          by.y = "PlotID", all.x = TRUE)
plots.ta.monthly <- plots.ta.monthly[!is.na(plots.ta.monthly$X), ]
coordinates(plots.ta.monthly) <- ~ X + Y

## crop grids to research area extent
ext <- extent(bbox(plots.ta.monthly))
ext@xmin <- ext@xmin - 3000
ext@xmax <- ext@xmax + 1000
ext@ymin <- ext@ymin - 1000
ext@ymax <- ext@ymax + 3000

exp.var.stck.cr <- crop(exp.var.stck, ext)
dem <- scale(exp.var.stck.cr[][, 1], center = TRUE, scale = TRUE)
slp <- scale(exp.var.stck.cr[][, 2], center = TRUE, scale = TRUE)
svf <- scale(exp.var.stck.cr[][, 3], center = TRUE, scale = TRUE)
asp <- scale(exp.var.stck.cr[][, 4], center = TRUE, scale = TRUE)
exp.var.stck.cr[[1]][] <- dem[, 1]
exp.var.stck.cr[[2]][] <- slp[, 1]
exp.var.stck.cr[[3]][] <- svf[, 1]
exp.var.stck.cr[[4]][] <- asp[, 1]

ndvi.stck.cr <- crop(ndvi.stck, ext)
ndvi <- lapply(seq(nlayers(ndvi.stck.cr)), function(i) {
  scale(ndvi.stck.cr[[i]][], center = TRUE, scale = TRUE)
})
ndvi.stck.cr <- stack(lapply(seq(nlayers(ndvi.stck.cr)), function(i) {
  tmp <- ndvi.stck.cr[[i]] 
  tmp[] <- ndvi[[i]][, 1]
  return(tmp)
}))

### extract predictor variables and add to table
extracted.values <- lapply(seq(nlayers(exp.var.stck.cr)), function(j) {
  exp <- extract(exp.var.stck.cr[[j]], plots.ta.monthly)
})

for (k in seq(extracted.values)) {
  plots.ta.monthly@data[nms.grids[k]] <- extracted.values[[k]]
}

plots.ta.monthly.mss <- split(plots.ta.monthly, plots.ta.monthly$Month)

extracted.ndvi <- lapply(seq(nlayers(ndvi.stck.cr)), function(i) {
  extract(ndvi.stck[[i]], plots.ta.monthly.mss[[i]])
})

for (i in seq(plots.ta.monthly.mss)) {
  plots.ta.monthly.mss[[i]]$ndvi <- extracted.ndvi[[i]]
}

plots.ta.monthly.all <- do.call("rbind", plots.ta.monthly.mss)
plots.ta.monthly <- plots.ta.monthly.all[complete.cases(plots.ta.monthly.all@data), ]

plots.ta.monthly$year <- substr(plots.ta.monthly$Yearmon, 1, 4)
plots.ta.monthly <- subset(plots.ta.monthly, year != "2010")
plots.ta.monthly.rug <- subset(plots.ta.monthly, 
                               StationId == "000rug" | StationId == "000rad")

#proj4string(plots.ta.monthly.rug) <- crs(exp.var.stck)
plots.ta.monthly.rug <- plots.ta.monthly.rug[order(plots.ta.monthly.rug$Yearmon), ]

cells.plots <- cellFromXY(exp.var.stck.cr, coordinates(plots.ta.monthly.rug))

x <- scale(coordinates(exp.var.stck.cr)[, 1], center = TRUE, scale = TRUE)
y <- scale(coordinates(exp.var.stck.cr)[, 2], center = TRUE, scale = TRUE)

x.plots <- x[cells.plots]
y.plots <- y[cells.plots]

### final definition of training table
ta.pred <- plots.ta.monthly.rug
ta.pred.splt <- split(ta.pred, ta.pred$Datetime)

### calcualte mean, min, max regional temperature
ta.pred <- lapply(seq(ta.pred.splt), function(i) {
  treg <- mean(c(ta.pred.splt[[i]]@data[[resp.var]]), na.rm = TRUE)
  tregmin <- min(c(ta.pred.splt[[i]]@data[[resp.var]]), na.rm = TRUE)
  tregmax <- max(c(ta.pred.splt[[i]]@data[[resp.var]]), na.rm = TRUE)
  ta.pred.splt[[i]]@data$Ta_reg <- treg
  ta.pred.splt[[i]]@data$Ta_reg_min <- tregmin
  ta.pred.splt[[i]]@data$Ta_reg_max <- tregmax
  return(ta.pred.splt[[i]])
})

month.indx <- sapply(seq(ta.pred.splt), function(i) {
  unique(ta.pred.splt[[i]]@data$Month)
})

ta.pred <- do.call("rbind", ta.pred)

tscale <- scale(c(ta.pred@data$Ta_reg,
                  ta.pred@data$Ta_reg_min,
                  ta.pred@data$Ta_reg_max),
                center = TRUE, scale = TRUE)[, 1]

tareg <- tscale[1:nrow(ta.pred@data)]
taregmin <- tscale[(nrow(ta.pred@data) + 1):(2 * nrow(ta.pred@data))]
taregmax <- tscale[((2 * nrow(ta.pred@data)) + 1):length(tscale)]

# tareg <- scale(ta.pred@data$Ta_reg,
#                center = TRUE, scale = TRUE)[, 1]
# taregmin <- scale(ta.pred@data$Ta_reg_min,
#                   center = TRUE, scale = TRUE)[, 1]
# taregmax <- scale(ta.pred@data$Ta_reg_max,
#                   center = TRUE, scale = TRUE)[, 1]

resp <- ta.pred@data[[resp.var]]

month <- as.factor(plots.ta.monthly.rug$Month)
year <- as.factor(plots.ta.monthly.rug$year)

ta.pred <- data.frame(ta.pred@data$dem, ta.pred@data$slp, ta.pred@data$svf, 
                      ta.pred@data$asp, ta.pred@data$ndvi, 
                      tareg, taregmin, taregmax,
                      x = x.plots,
                      y = y.plots,
                      ta.pred@data$Datetime)

ta.pred$month <- month
ta.pred$year <- year
names(ta.pred) <- c(nms.grids, "Ta_reg", "Ta_reg_min", "Ta_reg_max", 
                    "x", "y", "datetime", "month", "year")

ta.pred.splt <- split(ta.pred, ta.pred$datetime)

pred.vars.unique <- do.call("rbind", lapply(seq(ta.pred.splt), function(i) {
  data.frame(Ta_reg = unique(ta.pred.splt[[i]]$Ta_reg),
             Ta_reg_min = unique(ta.pred.splt[[i]]$Ta_reg_min),
             Ta_reg_max = unique(ta.pred.splt[[i]]$Ta_reg_max),
             month = unique(ta.pred.splt[[i]]$month),
             year = unique(ta.pred.splt[[i]]$year),
             datetime = unique(ta.pred.splt[[i]]$datetime))
}))

pred_prep <- ta.pred[, -which(names(ta.pred) %in% c("datetime", "x", "y"))]
pred_prep$resp <- resp

nms_out <- paste(paste(names(pred_prep)[1:(length(names(pred_prep)) - 1)],
                 collapse = "_"), ".csv", sep = "")
