library(raster)

path <- "/media/tims_ex/kilimanjaro_ta200_interp/in"
setwd(path)

ndvi <- list.files("monthly_means", pattern = glob2rx("utm_*"),
                   full.names = TRUE)

ndvi.stck <- stack(ndvi)

dem <- raster("DEM_ARC1960_30m_Hemp.sdat")

ndvi.stck.arc1960 <- projectRaster(ndvi.stck, dem)

nms.new <- gsub("utm_", "utm_arc1960_", ndvi)
writeRaster(ndvi.stck.arc1960, nms.new, bylayer = TRUE)
