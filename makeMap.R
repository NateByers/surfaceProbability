# Get probabilities by sourcing downloadDownscaler.R, mergeDownscaler.R, and fitEVD.R scripts

# Read in ozone probabilities
ozone.prob.75.df <- read.csv("ozoneProbabilities75ppbThreshold.csv", 
                          stringsAsFactors = FALSE)
names(ozone.prob.75.df)[names(ozone.prob.75.df) == "CTFIPS"] <- "GEOID10"
names(ozone.prob.75.df)[names(ozone.prob.75.df) == "prob"] <- "ozone_prob_75"

# Download census tract shapefile
dir.create("Tract_2010Census")
temp <- tempfile()
download.file("http://www2.census.gov/geo/tiger/TIGER2010DP1/Tract_2010Census_DP1.zip",
              temp)
#unzip into Tract_2010Census folder
unzip(temp, exdir = "Tract_2010Census")
file.remove(temp)

#create SpatialPolygonsDataFrame
library(maptools)
proj <- "+proj=longlat +datum=NAD83"
leaflet.proj <-"+init=epsg:3857"
census.tracts.spolydf <- readShapePoly("Tract_2010Census\\Tract_2010Census_DP1", 
                                       proj4string = CRS(proj))
census.tracts.spolydf$GEOID10 <- as.numeric(as.character(census.tracts.spolydf$GEOID10))
# merge ozone with spatialpolygonsdataframe
census.tracts.spolydf <- merge(census.tracts.spolydf, ozone.prob.75.df, all.x = FALSE)
census.tracts.spolydf <- census.tracts.spolydf[, c("GEOID10", "ozone_prob_75")]
# convert to Leaflet projection
library(rgdal)
census.tracts.spolydf <- spTransform(census.tracts.spolydf, CRS(leaflet.proj))

# Create ozone map with 75 ppb threshold
ozone.75.plot <- spplot(census.tracts.spolydf, "ozone_prob_75", col.regions=terrain.colors(100),
                        col = "transparent")
then <- Sys.time()
png(filename = "OzoneProb75ppbCT.png", width = 6000, height = 6000)
plot(ozone.75.plot)
then - Sys.time()
dev.off()

################################################################################################
# Add PM2.5 and other ozone probabilities
# Read in pm probabilities
pm.prob.35.df <- read.csv("pmProbabilities35ug_m3Threshold.csv", 
                          stringsAsFactors = FALSE)
names(pm.prob.35.df)[names(pm.prob.35.df) == "CTFIPS"] <- "GEOID10"
names(pm.prob.35.df)[names(pm.prob.35.df) == "prob"] <- "pm_prob_35"

# merge pm with spatialpolygonsdataframe
census.tracts.spolydf <- merge(census.tracts.spolydf, 
                               pm.prob.35.df[, c("GEOID10", "pm_prob_35")])

# Read in ozone probabilities, 70 pbb threshold
ozone.prob.70.df <- read.csv("ozoneProbabilities70ppbThreshold.csv", 
                          stringsAsFactors = FALSE)
names(ozone.prob.70.df)[names(ozone.prob.70.df) == "CTFIPS"] <- "GEOID10"
names(ozone.prob.70.df)[names(ozone.prob.70.df) == "prob"] <- "ozone_prob_70"

# merge ozone 70 threshold with spatialpolygonsdataframe
census.tracts.spolydf <- merge(census.tracts.spolydf, 
                               ozone.prob.70.df[, c("GEOID10", "ozone_prob_70")])

# Read in ozone proabilities, 65 ppb threshold
ozone.prob.65.df <- read.csv("ozoneProbabilities65ppbThreshold.csv", 
                             stringsAsFactors = FALSE)
names(ozone.prob.65.df)[names(ozone.prob.65.df) == "CTFIPS"] <- "GEOID10"
names(ozone.prob.65.df)[names(ozone.prob.65.df) == "prob"] <- "ozone_prob_65"

# merge ozone 65 threshold with spatialpolygonsdataframe
census.tracts.spolydf <- merge(census.tracts.spolydf, 
                               ozone.prob.65.df[, c("GEOID10", "ozone_prob_65")])

# Save pm map
pm.35.plot <- spplot(census.tracts.spolydf, "pm_prob_35", col.regions=terrain.colors(100),
                        col = "transparent")
then <- Sys.time()
png(filename = "PM_Prob_35_ugperm3_CT.png", width = 6000, height = 6000)
plot(pm.35.plot)
then - Sys.time()
dev.off()


# Save ozone 70 ppb map
ozone.70.plot <- spplot(census.tracts.spolydf, "ozone_prob_70", col.regions=terrain.colors(100),
                     col = "transparent")
then <- Sys.time()
png(filename = "Ozone_Prob_70_ppb_CT.png", width = 6000, height = 6000)
plot(ozone.70.plot)
then - Sys.time()
dev.off()

# Save ozone 65 ppb map
ozone.65.plot <- spplot(census.tracts.spolydf, "ozone_prob_65", col.regions=terrain.colors(100),
                        col = "transparent")
then <- Sys.time()
png(filename = "Ozone_Prob_65_ppb_CT.png", width = 6000, height = 6000)
plot(ozone.65.plot)
then - Sys.time()
dev.off()

# save the probability data.frame
write.csv(as.data.frame(census.tracts.spolydf), file = "probabilities.csv")


# Code for turning census tracts into a grid
#####################################################################################################
# census.tracts.spdf <- SpatialPointsDataFrame(coordinates(census.tracts.spolydf), 
#                                              as.data.frame(census.tracts.spolydf[, "GEOID10"]),
#                                              proj4string = CRS(leaflet.proj))
# 
# library(rgdal)
# box <- bbox(census.tracts.spolydf)
# cs <- c(10000, 10000)
# cc <- box[, 1] + (cs/2)
# cd <- ceiling(diff(t(box))/cs)
# us.topo <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
# us.sgriddf <- SpatialGridDataFrame(us.topo, data = data.frame(grid.id = 1:prod(cd[1],cd[2])),
#                                    CRS(leaflet.proj))
# us.grid.spdf <- SpatialPointsDataFrame(coordinates(us.sgriddf), data.frame(grid.id = us.sgriddf$grid.id),
#                                      proj4string = CRS(leaflet.proj))
# 
# # add grid id to census tract centroid data frame, and census tract FIP to 
# # grid centriod data frame
# census.tracts.spdf$grid.id <- over(census.tracts.spdf, us.sgriddf)$grid.id
# census.tracts.df <- as.data.frame(census.tracts.spdf)
# 
# us.grid.spdf$GEOID10 <- over(us.grid.spdf, census.tracts.spolydf)$GEOID10
# us.grid.spdf <- us.grid.spdf[!is.na(us.grid.spdf$GEOID10), ]
# us.grid.df <- as.data.frame(us.grid.spdf)
# 
# grid.census.df <- merge(us.grid.df[, c("grid.id", "GEOID10")], 
#                         census.tracts.df[, c("GEOID10", "grid.id")],
#                         all = TRUE)
# grid.census.df <- merge(grid.census.df, 
#                         ozone.prob.75.df[, c("GEOID10", "ozone_prob_75")], 
#                         all.x = TRUE)
# 
# 
# library(dplyr)
# grid.census.df <- group_by(grid.census.df, grid.id)
# grid.census.df <- summarize(grid.census.df, ozone_prob_max_75 = max(ozone_prob_75))
# 
# us.sgriddf.merged <- merge(us.sgriddf, as.data.frame(grid.census.df))
# 
# 
# spplot(us.sgriddf.merged, "ozone_prob_max_75", col.regions=terrain.colors(100))
#
# grid.census.df <- merge(us.grid.df[, c("grid.id", "GEOID10")], 
#                         census.tracts.df[, c("GEOID10", "grid.id")],
#                         all = TRUE)
# grid.census.pm.35.df <- merge(grid.census.df, pm.prob.35.df[, c("GEOID10", "pm_prob_35")], all.x = TRUE)
# 
# grid.census.pm.35.df <- group_by(grid.census.pm.35.df, grid.id)
# grid.census.pm.35.df <- summarize(grid.census.pm.35.df, pm_prob_max_35 = max(pm_prob_35))
# 
# us.sgriddf.pm.ozone.merged <- merge(us.sgriddf.merged, as.data.frame(grid.census.pm.df))
# 
# spplot(us.sgriddf.pm.ozone.merged, "pm_prob_max", col.regions=terrain.colors(100))
# 
# prob.grid <- us.sgriddf.pm.ozone.merged
# save(prob.grid, file = "probability_grid.rda")
# 
# bb <- bbox(prob.grid)
# center <- (bb[, 2] - bb[, 1])/2 + bb[, 1]
# 
# require(sp)
# getProb <- function(longitude, latitude, grid){
#   probs <- over(SpatialPoints(matrix(c(longitude, latitude), nrow = 1), 
#                      proj4string = CRS("+init=epsg:3857")), grid)
#   list(long = longitude, lat = latitude, ozone_prob = probs[2], pm_prob = probs[3])
# }

##################################################################################################


