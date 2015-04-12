#library(automap)
library(raster)
library(caret)
library(rgdal)

path <- "/media/tims_ex/kilimanjaro_ta200_interp"
setwd(path)
# path.server <- "/media/memory01/data/casestudies/kilimanjaro_ta200_interp"
# setwd(path.server)

# preparation -------------------------------------------------------------
### define response variable and cross-validation parameters
resp.var <- "Ta_200"
iters <- 250
train.size <- 0.8

### load necessary data
## grids
nms.grids <- c("dem", "slp", "svf", "ndvi")
files <- c("in/DEM_UTM37S_WGS84_30m_Hemp.tif", 
           "in/DEM_UTM37S_WGS84_30m_Hemp_slope.tif", 
           "in/DEM_UTM37S_WGS84_30m_Hemp_sky_view_factor.tif")

exp.var.stck <- stack(files)

ndvi.files <- list.files("in/monthly_means", pattern = glob2rx("*utm37s*"),
                         full.names = TRUE)
ndvi.stck <- stack(ndvi.files)

## points
lyr <- ogrListLayers("plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
                 layer = lyr)
plots <- subset(plots, PoleType == "AMP")

ta.monthly <- read.csv("sp1_monthly_Ta_200_2011_2014.csv",
                       stringsAsFactors = FALSE)
ta.monthly$Month <- as.integer(substr(ta.monthly$Yearmon, 6, 7))
ta.monthly$Datetime <- ta.monthly$Yearmon
plots.ta.monthly <- merge(ta.monthly, plots, by.x = "PlotId",
                          by.y = "PlotID", all.x = TRUE)
plots.ta.monthly <- plots.ta.monthly[!is.na(plots.ta.monthly$X), ]
coordinates(plots.ta.monthly) <- ~ X + Y

## crop grids to research area extent
ext <- extent(bbox(plots.ta.monthly))
ext@xmin <- ext@xmin - 10000
ext@xmax <- ext@xmax + 3000
ext@ymin <- ext@ymin - 1000
ext@ymax <- ext@ymax + 10000

exp.var.stck.cr <- crop(exp.var.stck, ext)

dem <- scale(exp.var.stck.cr[][, 1], center = TRUE, scale = TRUE)
slp <- scale(exp.var.stck.cr[][, 2], center = TRUE, scale = TRUE)
svf <- scale(exp.var.stck.cr[][, 3], center = TRUE, scale = TRUE)

exp.var.stck.cr[[1]][] <- dem[, 1]
exp.var.stck.cr[[2]][] <- slp[, 1]
exp.var.stck.cr[[3]][] <- svf[, 1]

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

proj4string(plots.ta.monthly.rug) <- crs(exp.var.stck)
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

tareg <- scale(ta.pred@data$Ta_reg,
               center = TRUE, scale = TRUE)[, 1]
taregmin <- scale(ta.pred@data$Ta_reg_min,
                  center = TRUE, scale = TRUE)[, 1]
taregmax <- scale(ta.pred@data$Ta_reg_max,
                  center = TRUE, scale = TRUE)[, 1]

resp <- ta.pred@data[[resp.var]]

month <- scale(as.integer(plots.ta.monthly.rug$Month), 
               center = TRUE, scale = TRUE)
year <- scale(as.integer(plots.ta.monthly.rug$year), 
              center = TRUE, scale = TRUE)

ta.pred <- data.frame(ta.pred@data$dem, ta.pred@data$slp, ta.pred@data$svf, 
                      ta.pred@data$ndvi, tareg, taregmin, taregmax,
                      x = x.plots,
                      y = y.plots,
                      ta.pred@data$Datetime)

ta.pred$month <- scale(month[, 1], center = TRUE, scale = TRUE)[, 1]
ta.pred$year <- scale(year[, 1], center = TRUE, scale = TRUE)[, 1]
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

pred <- ta.pred[, -which(names(ta.pred) %in% c("datetime"))]
pred$resp <- resp
