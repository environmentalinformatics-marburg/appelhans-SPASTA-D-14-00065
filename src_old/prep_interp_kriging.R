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

### crop grids to research area extent
ext <- extent(bbox(plots.ta.monthly))
ext@xmin <- ext@xmin - 10000
ext@xmax <- ext@xmax + 3000
ext@ymin <- ext@ymin - 1000
ext@ymax <- ext@ymax + 10000

exp.var.stck.cr <- crop(exp.var.stck, ext)
ndvi.stck.cr <- crop(ndvi.stck, ext)

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


