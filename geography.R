library(data.table)
# read in 2008 ozone data
ozone.2008 <- fread("O3Surface_12km_2008.csv", colClasses = c("integer", "integer", "character", 
                                                              rep("numeric", 4)))

# get unique x/y's for each year
ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))


library("maps")
# create a map of Wasthington state and plot the points
map('state', plot = TRUE, fill = FALSE)
points(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude, col='red', pch = 20)

library(sp)
# make a SpatialPoints object and plot them in Wasthington
ozone.longlat.matrix <- matrix(as.numeric(c(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude)), ncol = 2,
                                 dimnames = list(paste(ozone.2008.xy$Column, ozone.2008.xy$Row, sep = "_"),
                                                 c("Longitude", "Latitude")))


proj <- "+proj=longlat +datum=WGS84"
ozone.sp <- SpatialPoints(ozone.longlat.matrix, CRS(proj))
map('state', plot = TRUE, fill = FALSE)
points(ozone.sp, col='red', pch = 20)


# transform projection to UTM
library(rgdal)
lambert.proj <- paste0("+proj=lcc +lat_1=33.0 +lat_2=45.0 +lon_0=-97.0 +x_0=-97.0 +y_0=40.0 +datum=WGS84 +units=m")
ozone.sp <- spTransform(ozone.sp, CRS(lambert.proj))



# read in all of the probability tables and create one data.table
ozone.prob.dt <- rbindlist(lapply(1:10, function(x){
  fread(paste0("O3Probabilities/region", x, "probabilities.csv"), drop = 1)
}))

rownames(ozone.prob.dt) <- paste(ozone.prob.dt$Column, ozone.prob.dt$Row, sep = "_")

# create SpatialPointsDataFrame
ozone.sp.df <- SpatialPointsDataFrame(ozone.longlat.matrix, ozone.prob.dt, 
                                        proj4string = CRS(proj),
                                        match.ID = T)


library(raster)
ozone.raster <- raster(ozone.sp.df)
plot(ozone.raster)
