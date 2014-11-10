library(data.table)
# download census tract shapefile

# create temporary file
temp <- tempfile()
# download to temp file
download.file("http://www2.census.gov/geo/tiger/TIGER2010DP1/Tract_2010Census_DP1.zip",
              temp)
#unzip into Tract_2010Census folder
unzip(temp, exdir = "Tract_2010Census")
file.remove(temp)
# edit .dbf
library(foreign)
dbfdata <- read.dbf("Tract_2010Census\\Tract_2010Census_DP1.dbf")
dbfdata <- dbfdata[, !grepl("DP", names(dbfdata), fixed = T)]
write.dbf(dbfdata, "Tract_2010Census\\Tract_2010Census_DP1.dbf")
rm(dbfdata)
#create poly
library(maptools)
proj <- "+proj=longlat +datum=WGS84"
census.tracts.spolydf <- readShapePoly("Tract_2010Census\\Tract_2010Census_DP1", 
                               proj4string = CRS(proj))
# get coordintes
coords <- coordinates(census.tracts.spolydf)
# get CTFIPS column
CTFIPS <- as.data.frame(census.tracts.spolydf)[, "GEOID10"]
# make matrix
census.longlat.matrix <- matrix(c(coords[, 1], coords[, 2]), ncol = 2)
# make SpatialPoints object
census.tracts.sp <- SpatialPoints(census.longlat.matrix, proj4string = CRS(proj))
# remove large SpatialPolygonsDataFrame from memory
rm(census.tracts.spolydf)
gc()


# get the ozone 12km by 12km grid from the 2008 file
library(data.table)
# create temporary file
temp2 <- tempfile()
# download to temp file
download.file("http://www.epa.gov/heasd/documents/cdc/HBM/ozone/O3Surface_12km_2008.zip",
              temp2)
#unzip into working directory
unzip(temp2)
file.remove(temp2)
# read in 2008 ozone data
ozone.2008 <- fread("O3Surface_12km_2008.csv", colClasses = c("integer", "integer", "character", 
                                                              rep("numeric", 4)))
# get unique x/y's for each year
ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))
# remove the large ozone file
rm(ozone.2008)
gc()

# make a SpatialPoints ozone grid object 
ozone.longlat.matrix <- matrix(as.numeric(c(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude)), ncol = 2,
                               dimnames = list(paste(ozone.2008.xy$Column, ozone.2008.xy$Row, sep = "_"),
                                               c("Longitude", "Latitude")))
ozone.sp <- SpatialPoints(ozone.longlat.matrix, CRS(proj))
# remove ozone files
rm(ozone.2008.xy, ozone.longlat.matrix)
gc()

# make 'ppp' objects
library(spatstat)
census.ppp <- as.ppp(census.tracts.sp)
grid.ppp <- as.ppp(ozone.sp)

# find the nearest neighbor in census.ppp for each point of grid.ppp
nearest.census.centroid <- nncross(grid.ppp, census.ppp)
range(sort(nearest.census.centroid$dist, decreasing = T))  # top 4 distances could be in Texas
N <- nearest.census.centroid$which
plot(superimpose(X=census.ppp,Y=grid.ppp), main="nncross", pch = c(20, 17), 
     cols=c("red","blue"))
arrows(grid.ppp$x, grid.ppp$y, census.ppp[N]$x, census.ppp[N]$y, length=0.15)

# create data frame of grid locations with their nearest census locations
grid.census.df <- data.frame(grid_x = grid.ppp$x, 
                             grid_y = grid.ppp$y, 
                             census_x = census.ppp[N]$x, 
                             census_y = census.ppp[N]$y)

# make census data frame and merge with grid data frame
census.df <- data.frame(census_x = census.longlat.matrix[, 1],
                        census_y = census.longlat.matrix[, 2],
                        CTFIPS)
grid.census.ID.df <- merge(grid.census.df, census.df, all.x = TRUE)

# read in CDC data
ozone.2009.2011 <- read.csv("ozone_max_209_11.csv", 
                            colClasses = c("factor", "integer", "numeric", "integer"))
# change from long to wide
library(reshape2)
ozone.2009.2011 <- dcast(ozone.2009.2011, CTFIPS + year ~ Max, value.var = "Ozone")
# merge CDC data with grid locations
grid.census.ID.data.df <- merge(grid.census.ID.df, ozone.2009.2011, all.x = T)
