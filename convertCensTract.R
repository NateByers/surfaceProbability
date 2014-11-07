# download census tract shapefile

# create temporary file
temp <- tempfile()
# download to temp file
download.file("http://www2.census.gov/geo/tiger/TIGER2010DP1/Tract_2010Census_DP1.zip",
              temp)
#unzip into Tract_2010Census folder
unzip(temp, exdir = "Tract_2010Census")
# edit .dbf
library(foreign)
dbfdata <- read.dbf("Tract_2010Census\\Tract_2010Census_DP1.dbf")
dbfdata <- dbfdata[, !grepl("DP", names(dbfdata), fixed = T)]
write.dbf(dbfdata, "Tract_2010Census\\Tract_2010Census_DP1.dbf")
#create poly
library(maptools)
proj <- "+proj=longlat +datum=WGS84"
census_tracts <- readShapePoly("Tract_2010Census\\Tract_2010Census_DP1", 
                               proj4string = CRS(proj))


# crop the census tract to the continental US
# file from https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
library(rgeos)
us_shape <- readShapePoly("us_boundary\\cb_2013_us_nation_20m", 
                          proj4string = CRS(proj))
census_tracts <- gIntersection(as(census_tracts, "SpatialPolygons"), 
                               as(us_shape, "SpatialPolygons"))
plot(census_tracts)


# get the ozone 12km by 12km grid from the 2008 file
library(data.table)
# # read in 2008 ozone data
# ozone.2008 <- fread("O3Surface_12km_2008.csv", colClasses = c("integer", "integer", "character", 
#                                                               rep("numeric", 4)))
# 
# # get unique x/y's for each year
# ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))
ozone.2008.xy <- fread("ozone_grid.csv")


# make a SpatialPoints ozone grid object 
library(sp)
ozone.longlat.matrix <- matrix(as.numeric(c(ozone.2008.xy$Longitude, ozone.2008.xy$Latitude)), ncol = 2,
                               dimnames = list(paste(ozone.2008.xy$Column, ozone.2008.xy$Row, sep = "_"),
                                               c("Longitude", "Latitude")))
ozone.sp <- SpatialPoints(ozone.longlat.matrix, CRS(proj))

# crop the grid to just the US boundary
index.vector <- over(ozone.sp, as(us_shape, "SpatialPolygons") )
ozone.cropped.sp <- ozone.sp[!is.na(index.vector), ]
