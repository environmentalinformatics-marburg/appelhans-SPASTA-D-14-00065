#library(automap)
library(raster)
library(caret)
library(rgdal)
library(Rsenal)
library(dplyr)

path <- "~/tappelhans/uni/temp"
setwd(path)
# path.server <- "/media/memory01/data/casestudies/kilimanjaro_ta200_interp_ephraim"
# setwd(path.server)

# preparation -------------------------------------------------------------
### define response variable and cross-validation parameters
resp.var <- "Ta_200"


### load necessary data
## grids
nms.grids <- c("dem", "slp", "svf", "asp", "ndvi")
files <- c("data/in/DEM_UTM37S_WGS84_30m_Hemp.tif",
           "data/in/DEM_UTM37S_WGS84_30m_Hemp_slope.tif",
           "data/in/DEM_UTM37S_WGS84_30m_Hemp_sky_view_factor.tif",
           "data/in/DEM_UTM37S_WGS84_30m_Hemp_aspect.tif")

exp.var.stck <- stack(files)

ndvi.files <- list.files("data/in/monthly_means",
                         pattern = glob2rx("*utm37s_wgs84*"),
                         full.names = TRUE)
ndvi.stck <- stack(ndvi.files)


## point location data
lyr <- ogrListLayers("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp")
plots <- readOGR("data/plots_shp/PlotPoles_ARC1960_mod_20140807_final.shp",
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
# ta.monthly <- read.csv("data/Ta_200_0310_monthly_mean.dat",
#                        stringsAsFactors = FALSE)

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
proj4string(plots.ta.monthly) <- projection(exp.var.stck)


### delete months with less than 5 observations above 2321 masl
plots.ta.monthly.lst <- split(plots.ta.monthly, plots.ta.monthly$tg)

ind5 <- do.call("c", lapply(plots.ta.monthly.lst, function(x) {
  length(x$Z_DEM_HMP[x$Z_DEM_HMP > 2321]) >= 5
}))

plots.ta.monthly <- do.call("rbind", plots.ta.monthly.lst[ind5])

## crop grids to research area extent
ext <- extent(bbox(plots.ta.monthly))
ext@xmin <- ext@xmin - 3000
ext@xmax <- ext@xmax + 1000
ext@ymin <- ext@ymin - 1000
ext@ymax <- ext@ymax + 3000


### scale predictor variables
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


extracted.ndvi <- lapply(seq(names(plots.ta.monthly.mss)), function(i) {
  mnth <- as.numeric(names(plots.ta.monthly.mss)[i])
  extract(ndvi.stck[[mnth]], plots.ta.monthly.mss[[i]])
})

for (i in seq(plots.ta.monthly.mss)) {
  plots.ta.monthly.mss[[i]]$ndvi <- extracted.ndvi[[i]]
}

plots.ta.monthly.all <- do.call("rbind", plots.ta.monthly.mss)
plots.ta.monthly.rug <- plots.ta.monthly.all[complete.cases(plots.ta.monthly.all@data), ]

plots.ta.monthly.rug$year <- substr(plots.ta.monthly.rug$tg, 1, 4)
# plots.ta.monthly <- subset(plots.ta.monthly, year != "2010")
# plots.ta.monthly.rug <- subset(plots.ta.monthly,
#                                StationId == "000rug" | StationId == "000rad")

#proj4string(plots.ta.monthly.rug) <- crs(exp.var.stck)
plots.ta.monthly.rug <- plots.ta.monthly.rug[order(plots.ta.monthly.rug$tg), ]

cells.plots <- cellFromXY(exp.var.stck.cr, coordinates(plots.ta.monthly.rug))

x <- scale(coordinates(exp.var.stck.cr)[, 1], center = TRUE, scale = TRUE)
y <- scale(coordinates(exp.var.stck.cr)[, 2], center = TRUE, scale = TRUE)

x.plots <- x[cells.plots]
y.plots <- y[cells.plots]

### final definition of training table
ta.pred <- plots.ta.monthly.rug
ta.pred.splt <- split(ta.pred, ta.pred$tg)


### calcualte mean, lwr, upr regional temperature intercept
ta.pred <- lapply(seq(ta.pred.splt), function(i) {

  ### intercept of linear model over all obs
  lmod_all <- lm(ta.pred.splt[[i]]@data$Ta_200 ~ ta.pred.splt[[i]]@data$Z_DEM_HMP)
  treg <- lmod_all$coefficients[1]

  ### intercept of linear model over lower obs
  indx_lwr <- ta.pred.splt[[i]]@data$Z_DEM_HMP <= 2321
  lmod_lwr <- lm(ta.pred.splt[[i]]@data$Ta_200[indx_lwr] ~
                   ta.pred.splt[[i]]@data$Z_DEM_HMP[indx_lwr])
  treglwr <- lmod_lwr$coefficients[1]

  ### intercept of linear model over lower obs
  lmod_upr <- lm(ta.pred.splt[[i]]@data$Ta_200[!indx_lwr] ~
                   ta.pred.splt[[i]]@data$Z_DEM_HMP[!indx_lwr])
  tregupr <- lmod_upr$coefficients[1]

  ta.pred.splt[[i]]@data$Ta_reg <- treg
  ta.pred.splt[[i]]@data$Ta_reg_lwr <- treglwr
  ta.pred.splt[[i]]@data$Ta_reg_upr <- tregupr
  return(ta.pred.splt[[i]])
})

month.indx <- sapply(seq(ta.pred.splt), function(i) {
  unique(ta.pred.splt[[i]]@data$Month)
})

ta.pred <- do.call("rbind", ta.pred)

tscale <- scale(c(ta.pred@data$Ta_reg,
                  ta.pred@data$Ta_reg_lwr,
                  ta.pred@data$Ta_reg_upr),
                center = TRUE, scale = TRUE)[, 1]

tareg <- tscale[1:nrow(ta.pred@data)]
tareglwr <- tscale[(nrow(ta.pred@data) + 1):(2 * nrow(ta.pred@data))]
taregupr <- tscale[((2 * nrow(ta.pred@data)) + 1):length(tscale)]

# tareg <- scale(ta.pred@data$Ta_reg,
#                center = TRUE, scale = TRUE)[, 1]
# taregmin <- scale(ta.pred@data$Ta_reg_min,
#                   center = TRUE, scale = TRUE)[, 1]
# taregmax <- scale(ta.pred@data$Ta_reg_max,
#                   center = TRUE, scale = TRUE)[, 1]

resp <- ta.pred@data[[resp.var]]

month <- as.factor(plots.ta.monthly.rug$Month)
year <- as.factor(plots.ta.monthly.rug$year)

ta.pred <- data.frame(as.character(ta.pred@data$plotID),
                      as.character(ta.pred@data$lg),
                      ta.pred@data$dem, ta.pred@data$slp, ta.pred@data$svf,
                      ta.pred@data$asp, ta.pred@data$ndvi,
                      tareg, tareglwr, taregupr,
                      as.character(ta.pred@data$Datetime),
                      x = coordinates(ta.pred)[, 1],
                      y = coordinates(ta.pred)[, 2])

ta.pred$month <- as.character(month)
ta.pred$year <- as.character(year)
ta.pred$resp <- resp
names(ta.pred) <- c("plotID", "lg", nms.grids, "Ta_reg", "Ta_reg_lwr",
                    "Ta_reg_upr", "datetime", "x", "y",
                    "month", "year", "resp")

ta.pred.splt <- split(ta.pred, ta.pred$datetime, drop = TRUE)

pred.vars.unique <- do.call("rbind", lapply(seq(ta.pred.splt), function(i) {
  data.frame(Ta_reg = unique(ta.pred.splt[[i]]$Ta_reg),
             Ta_reg_lwr = unique(ta.pred.splt[[i]]$Ta_reg_lwr),
             Ta_reg_upr = unique(ta.pred.splt[[i]]$Ta_reg_upr),
             month = unique(ta.pred.splt[[i]]$month),
             year = unique(ta.pred.splt[[i]]$year),
             datetime = unique(ta.pred.splt[[i]]$datetime))
}))


### final creation of 10 folds per valid month
ta.pred.splt.folds <- lapply(seq(ta.pred.splt), function(i) {

  s <- split(ta.pred.splt[[i]], ta.pred.splt[[i]]$lg, drop = TRUE)

  ind <- unlist(lapply(seq(s), function(l) {
    if (nrow(s[[l]]) > 1) {
      v <- rep(TRUE, nrow(s[[l]]))
      set.seed(l)
      v[sample(seq(v), 1)] <- FALSE
      return(v)
    } else {
      if (nrow(s[[l]]) == 1) return(FALSE)
    }
  }))

  min_train_set <- ta.pred.splt[[i]][!ind, ]
  #print(min_train_set$plotID)
  rest <- ta.pred.splt[[i]][ind, ]

  set.seed(1234)
  flds <- createFolds(rest$resp, 10, returnTrain = TRUE)

  rest_train_sets <- lapply(seq(flds), function(f) {
    rbind(rest[flds[[f]], ], min_train_set)
  })

  test_sets <- lapply(seq(flds), function(f) {
    rest[-flds[[f]], ]
  })

  return(list(test = test_sets,
              train = rest_train_sets))#,
              #n = length(min_train_set$plotID)))
})

### calculate mean number of plots/habtats in min_train_set
### MOTE need to uncomment the return statement above for this to work
mean(do.call("c", lapply(ta.pred.splt.folds, function(x) x$n)))

save(ta.pred.splt.folds, file = "data/ta.pred.splt.folds.rda")

rm(i)


### create spatial grid dfs for kriging
exp_var_stck_sp <- lapply(seq(month.indx), function(i) {
  stck <- stack(exp.var.stck.cr, ndvi.stck.cr[[month.indx[i]]])
  stck_sp <- as(stck, "SpatialPixelsDataFrame")
  names(stck_sp) <- nms.grids
  return(stck_sp)
})

names(exp_var_stck_sp) <- names(ta.pred.splt)
save(exp_var_stck_sp, file = "data/exp_var_stck_sp.rda")


### create full data set for final interpolation
save(ta.pred, file = "data/ta.pred.complete.rda") # train data

save(exp.var.stck.cr, file = "data/exp.var.stck.cr.rda")
save(ndvi.stck.cr, file = "data/ndvi.stck.cr.rda")
