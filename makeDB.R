library(data.table)

# read in data
ozone.2008 <- fread("O3Surface_12km_2008.csv")


# get unique lat/longs for each year
ozone.2008.latlong <- unique(ozone.2008, by = c("Longitude", "Latitude", "Column", "Row"))
ozone.2008.latlong <- as.data.frame(ozone.2008.latlong)

ozone.2001 <- fread("O3Surface_12km_2001.csv")
ozone.2001.latlong <- unique(ozone.2001, by = c("Longitude", "Latitude", "Column", "Row"))
ozone.2001.latlong <- as.data.frame(ozone.2001.latlong) 


west <- ozone.2008.latlong[!(ozone.2008.latlong[, c("Longitude", "Latitude")] %in% ozone.2001.latlong[, c("Longitude", "Latitude")]), ]

library(maps)
map("state")
points(west$Longitude, west$Latitude)