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


#######################


# read in 2008 ozone data
ozone.2008 <- fread("O3Surface_12km_2008.csv", colClasses = c("integer", "integer", "character", 
                                                              rep("numeric", 4)))
# get unique x/y's for each year
ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))
# remove the large ozone file
rm(ozone.2008)
gc()

# make a SpatialPointsDataFrame ozone grid object 
ozone.spdf <-SpatialPointsDataFrame(matrix(as.numeric(c(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude)), ncol = 2,),
                                    data.frame(cell = paste0(ozone.2008.xy$Column, ozone.2008.xy$Row)), 
                                    proj4string = CRS(proj))
# remove ozone files
rm(ozone.2008.xy)
gc()

######################
# create ozone pixels
# transform projection to lambert
library(rgdal)
lambert.proj <- paste0("+proj=lcc +lat_1=33.0 +lat_2=45.0 +lon_0=-97.0 +x_0=-97.0 +y_0=40.0 +units=m")
ozone.spdf <- spTransform(ozone.spdf, CRS(lambert.proj))

bb <- bbox(ozone.spdf)
cs <- c(12000, 12000)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
ozone.grid <- GridTopology(cellcentre.offset = cc,
                           cellsize = cs, cells.dim = cd)
ozone.sgrid <- SpatialGrid(ozone.grid, CRS(lambert.proj))
ozone.sgriddf <- SpatialGridDataFrame(ozone.sgrid, 
                                      data.frame(cell = row.names(ozone.sgrid)))


###################
# assign cell to each census tract
# transform census.tract.spdf to lambert
census.tracts.spdf <- spTransform(census.tracts.spdf, CRS(lambert.proj))

plot(as(census.tracts.spdf, "SpatialPoints"))
plot(ozone.sgrid, add = T)

# add cell number to census.tracts.spdf
census.tracts.spdf$cell <- over(census.tracts.spdf, ozone.sgriddf)$cell
# check the census tracts that weren't in a grid cell
plot(as(census.tracts.spdf[!is.na(census.tracts.spdf$cell), ], "SpatialPoints"))
# remove those census tract centroids
census.tracts.spdf <- census.tracts.spdf[!is.na(census.tracts.spdf$cell), ]


##################
# analyze differences between values in each pixel
census.tracts.df <- as.data.frame(census.tracts.spdf)
names(census.tracts.df)[names(census.tracts.df) == "GEOID10"] <- "CTFIPS"
# get the CDC data and merge with df
cdc.ozone <- read.csv("ozone_max_209_11.csv")
cdc.ozone <- cdc.ozone[cdc.ozone$Max <= 4, ]
df.merged <- merge(census.tracts.df, cdc.ozone)

library(dplyr)
df.grouped <- group_by(df.merged, cell, year, Max)
df.diff <- summarise(df.grouped, max.diff = max(Ozone) - min(Ozone))
df.diff <- arrange(df.diff, desc(max.diff))
hist(df.diff$max.diff)
