#Create polygon of census tracts. See 'convertCensTract.R' script for how this was obtained
library(maptools)
proj <- "+proj=longlat +datum=WGS84"
census.tracts.spolydf <- readShapePoly("Tract_2010Census\\Tract_2010Census_DP1", 
                                       proj4string = CRS(proj))
# attach coordinats to data.frameget coordintes
census.tracts.df <- data.frame(Longitude = coordinates(census.tracts.spolydf)[, 1],
                               Latitude = coordinates(census.tracts.spolydf)[, 2],
                               as.data.frame(census.tracts.spolydf))

# make SpatialPointsDataFrame object
census.tracts.spdf <- SpatialPointsDataFrame(census.tracts.df[, c("Longitude", "Latitude")],
                                             census.tracts.df, proj4string = CRS(proj))
# remove large SpatialPolygonsDataFrame from memory
rm(census.tracts.spolydf)
gc()