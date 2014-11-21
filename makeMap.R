# Get probabilities by sourcing downloadDownscaler.R, mergeDownscaler.R, and fitEVD.R scripts

# Read in ozone probabilities
ozone.prob.df <- read.csv("ozoneProbabilities75ppbThreshold.csv", 
                          stringsAsFactors = FALSE)
names(ozone.prob.df)[names(ozone.prob.df) == "CTFIPS"] <- "GEOID10"

# Download census tract shapefile
dir.create("Tract_2010Census")
temp <- tempfile()
download.file("http://www2.census.gov/geo/tiger/TIGER2010DP1/Tract_2010Census_DP1.zip",
              temp)
#unzip into Tract_2010Census folder
unzip(temp, exdir = "Tract_2010Census")
file.remove(temp)

#create poly
library(maptools)
proj <- "+proj=longlat +datum=WGS84"
lambert.proj <- "+proj=lcc +lat_1=33.0 +lat_2=45.0 +lon_0=-97.0 +x_0=-97.0 +y_0=40.0 +units=m"
leaflet.proj <-"+proj=EPSG:3857"
census.tracts.spolydf <- readShapePoly("Tract_2010Census\\Tract_2010Census_DP1", 
                                       proj4string = CRS(proj))
census.tracts.spolydf$GEOID10 <- as.numeric(as.character(census.tracts.spolydf$GEOID10))
# merge ozone with spatialpolygonsdataframe
census.tracts.spolydf <- merge(census.tracts.spolydf, ozone.prob.df, all.x = FALSE)
census.tracts.spolydf <- census.tracts.spolydf[, c("GEOID10", "prob")]


# # read in US outline
# dir.create("us_boundary")
# temp <- tempfile()
# download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_nation_20m.zip",
#               temp)
# #unzip into us_boundary folder
# unzip(temp, exdir = "us_boundary")
# file.remove(temp)
# us_shape <- readShapePoly("us_boundary\\cb_2013_us_nation_20m", 
#                           proj4string = CRS(proj))

# create a grid over the us and clip on us_shape
# transform projection to lambert
library(rgdal)
census.tracts.spolydf <- spTransform(census.tracts.spolydf, CRS(lambert.proj))
box <- bbox(census.tracts.spolydf)
cs <- c(12000, 12000)
cc <- box[, 1] + (cs/2)
cd <- ceiling(diff(t(box))/cs)
us.topo <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
us.sgriddf <- SpatialGridDataFrame(us.topo, data = data.frame(id = 1:prod(cd[1],cd[2])),
                                                              CRS(lambert.proj))
# index.vector <- over(as(us.sgriddf, "SpatialGrid"), as(census.tracts.spolydf, "SpatialPolygons") )
# us.clipped.sgriddf <- us.sgriddf[!is.na(index.vector), ]
library(raster)
grid.raster <- raster(as(us.sgriddf, "SpatialGrid"))
grid.raster <- crop(grid.raster, census.tracts.spolydf)
census.tracts.raster <- rasterize(census.tracts.spolydf, grid.raster,
                                  "prob", fun = 'max')
census.tracts.raster <- spTransform(census.tracts.raster, CRS(leaflet.proj))
library(RColorBrewer)
spplot(census.tracts.raster)
