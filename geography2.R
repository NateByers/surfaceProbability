library(data.table)
# # read in 2008 ozone data
# ozone.2008 <- fread("O3Surface_12km_2008.csv", colClasses = c("integer", "integer", "character", 
#                                                               rep("numeric", 4)))
# 
# # get unique x/y's for each year
# ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))
ozone.2008.xy <- fread("ozone_grid.csv")


library("maps")
# create a map of Wasthington state and plot the points
map('state', plot = TRUE, fill = FALSE)
points(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude, col='red', pch = 20)

library(sp)
# make a SpatialPoints object and plot them 
ozone.longlat.matrix <- matrix(as.numeric(c(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude)), ncol = 2,
                                 dimnames = list(paste(ozone.2008.xy$Column, ozone.2008.xy$Row, sep = "_"),
                                                 c("Longitude", "Latitude")))


proj <- "+proj=longlat +datum=WGS84"
ozone.sp <- SpatialPoints(ozone.longlat.matrix, CRS(proj))

# read in all of the probability tables and create one data.table
ozone.prob.dt <- rbindlist(lapply(1:10, function(x){
  fread(paste0("O3Probabilities/region", x, "probabilities.csv"), drop = 1)
}))
ozone.prob.df <- as.data.frame(ozone.prob.dt)
rownames(ozone.prob.df) <- paste(ozone.prob.df$Column, ozone.prob.df$Row, sep = "_")

# create SpatialPointsDataFrame
ozone.sp.df <- SpatialPointsDataFrame(ozone.longlat.matrix, ozone.prob.df, 
                                        proj4string = CRS(proj),
                                        match.ID = T)


# transform projection to lambert
library(rgdal)
lambert.proj <- paste0("+proj=lcc +lat_1=33.0 +lat_2=45.0 +lon_0=-97.0 +x_0=-97.0 +y_0=40.0 +units=m")
ozone.sp.df <- spTransform(ozone.sp.df, CRS(lambert.proj))


library(raster)
XYZ <- cbind(ozone.sp.df@coords, ozone.sp.df@data$Prob)
ozone.raster <- rasterFromXYZ(XYZ, crs = lambert.proj, 
                              res = c(12000, 12000), digits = 0)


# file from https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
library(maptools)
us_shape <- readShapePoly("us_boundary\\cb_2013_us_nation_20m", 
                          proj4string = CRS("+proj=longlat +datum=WGS84"))
us_shape <- spTransform(us_shape, CRS(lambert.proj))
plot(ozone.raster, col = c("green3", "yellow", "darkorange"))
plot(us_shape, add = T)

# change raster projection
ozone.raster.longlat <- projectRaster(ozone.raster, crs = proj)
plot(ozone.raster.longlat, col = c("green3", "yellow", "darkorange"))
us_shape <- spTransform(us_shape, CRS(proj))
plot(us_shape, add = T)


# crop SpatialPoinstDataFrame object
ozone.sp.df <- spTransform(ozone.sp.df, CRS(proj))
us_shape <- spTransform(us_shape, CRS(proj))
index.vector <- over(as(ozone.sp.df, "SpatialPoints"), as(us_shape, "SpatialPolygons") )
ozone.cropped.sp.df <- ozone.sp.df[!is.na(index.vector), ]
bbox(ozone.cropped.sp.df)

ozone.cropped.sp.df <- spTransform(ozone.cropped.sp.df, CRS(lambert.proj))
XYZ.cropped <- cbind(ozone.cropped.sp.df@coords, ozone.cropped.sp.df@data$Prob)
ozone.cropped.raster <- rasterFromXYZ(XYZ.cropped, crs = lambert.proj, 
                              res = c(12000, 12000), digits = 0)
ozone.cropped.raster.longlat <- projectRaster(ozone.cropped.raster, crs = proj)
plot(ozone.cropped.raster.longlat, col = c("green3", "yellow", "darkorange"))



poly <- rasterToPolygons(ozone.cropped.raster.longlat, dissolve=TRUE)
plot(poly)
writeOGR(poly, ".", "ozoneProbability", driver="ESRI Shapefile")


##########################################
# Bin probabilities
table(cut(ozone.cropped.sp.df$Prob, c(-1, .4, .6, .8, .95, 100), labels = c("0", "1",
                                                                   "2", "3",
                                                                   "4")
          ))
Bin <- cut(ozone.cropped.sp.df$Prob, c(-1, .4, .6, .8, .95, 100), labels = c("0", "1",
                                                                             "2", "3",
                                                                             "4")
           )
Bin <- as.integer(Bin)


XYZ.cropped.binned <- cbind(ozone.cropped.sp.df@coords, Bin)
ozone.cropped.binned.raster <- rasterFromXYZ(XYZ.cropped.binned, crs = lambert.proj, 
                                      res = c(12000, 12000), digits = 0)
ozone.cropped.binned.raster.longlat <- projectRaster(ozone.cropped.binned.raster, crs = proj)
plot(ozone.cropped.binned.raster.longlat, col = c("green3", "yellow", "darkorange", "red", "purple"))


ozone.cropped.binned.raster.longlat <- as.factor(ozone.cropped.binned.raster.longlat)
poly.binned <- rasterToPolygons(ozone.cropped.binned.raster.longlat, n = 8, dissolve=T)
poly.binned$Bin <- cut(poly.binned$Bin, 
                       quantile(poly.binned$Bin, c(0, .4, .6, .8, .95, 1)),
                       labels = as.character(1:5))
plot(poly.binned, col = c("green3", "yellow", "darkorange", "red", "purple"))
writeOGR(poly.binned, ".", "ozoneProbabilityBinned", driver="ESRI Shapefile")
