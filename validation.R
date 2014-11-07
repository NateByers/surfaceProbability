# library(RSQLite)
# db <- dbConnect(SQLite(), "data/netassess.sqlite")
# 
# dbListTables(db)              # The tables in the database
# dbListFields(db, "Sites")    # The columns in a table
# head(dbReadTable(db, "Sites"))     
# 
# # get moinitor site lat/longs
# monitor.locations <- dbReadTable(db, "Sites")
# monitor.locations <- subset(monitor.locations, State_Code != 2 &
#                               State_Code != 15)
# # monitor.locations <- subset(monitor.locations, Longitude > -86.8551 &     # subset down to greater Indy
# #                               Longitude < -85.5065 & Latitude > 39.4986 &
# #                               Latitude < 40.1378)
# dbDisconnect(db)

# create a temporary file
temp <- tempfile()

# download the .zip file to a temporary file--this will take several minutes
download.file('http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_44201_2013.zip', temp)

# unzip temporary file to your working directory
unzip(temp)

# delete the temporary file
unlink(temp)

# read the data into R
monitor.data <- fread('daily_44201_2013.csv')

# remove Alaska and Hawaii
setattr(monitor.data, 'names', gsub(" ", "_", names(monitor.data))) # replace space with underscore in column names
setattr(monitor.data, 'names', gsub("1st_", "", names(monitor.data), fixed = T)) # remove "1st_" from 1st_Max_Value
monitor.data <- subset(monitor.data, State_Code != 2 & State_Code != 15)

# remove days with less than 75% completeness
monitor.data <- subset(monitor.data, Observation_Percent >= 75)

# identify columns to group by
library(dplyr)
monitor.data <- group_by(monitor.data, Latitude, Longitude, Datum)

# get the number of days each monitor was active
monitored.days <- summarize(monitor.data, days = n())

# subset down to monitors with values for at least 75% of the ozone season
monitored.days <- subset(monitored.days, days >= ((365/2)*.75))

# drop monitors that didn't meet completeness criteria
monitor.data <- merge(monitor.data, monitored.days[, c("Longitude", "Latitude"), with = F])

# create summary table that gives the 4th highest value for every column/row combination
monitor.data <- group_by(monitor.data, Latitude, Longitude, Datum)
monitor.4th.highest.2013 <- summarize(monitor.data, fourth_high = nth(Max_Value, 4))

# make SpatialPoints object
proj <- "+proj=longlat +datum=WGS84"
monitor.sp <- SpatialPoints(monitor.locations[, c("Longitude", "Latitude")], CRS(proj))
# transform projection to lambert
library(rgdal)
lambert.proj <- paste0("+proj=lcc +lat_1=33.0 +lat_2=45.0 +lon_0=-97.0 +x_0=-97.0 +y_0=40.0 +units=m")
monitor.sp <- spTransform(monitor.sp, CRS(lambert.proj))
# make 'ppp' object
monitor.ppp <- as.ppp(monitor.sp)


# get the ozone HB model grid
model.centroids <- read.csv("ozone_grid.csv")
# model.centroids <- subset(model.centroids, Longitude > -86.8551 &       # subset down to greater Indy
#                             Longitude < -85.5065 & Latitude > 39.4986 &
#                             Latitude < 40.1378)
# make SpatialPoints object
model.sp <- SpatialPoints(model.centroids[, c("Longitude", "Latitude")], CRS(proj))
# transform projection to lambert
model.sp <- spTransform(model.sp, CRS(lambert.proj))
# make 'ppp' object
model.ppp <- as.ppp(model.sp)

# find the nearest neighbor in model.ppp of each point of monitor.ppp
nearest.centroid <- nncross(monitor.ppp, model.ppp)
range(sort(nearest.centroid$dist, decreasing = T)[-(1:4)])  # top 4 distances could be in Texas
N <- nearest.centroid$which
plot(superimpose(X=model.ppp,Y=monitor.ppp), main="nncross", pch = c(20, 17), 
     cols=c("red","blue"))
arrows(monitor.ppp$x, monitor.ppp$y, model.ppp[N]$x, model.ppp[N]$y, length=0.15)

# create data frame of monitor locations with their nearest centroid locations
monitor.centroid.df <- data.frame(monitor_x = monitor.ppp$x, 
                                  monitor_y = monitor.ppp$y, 
                                  centroid_x = model.ppp[N]$x, 
                                  centroid_y = model.ppp[N]$y)

# read in all of the probability tables and create one data frame
ozone.prob.df <- as.data.frame(rbindlist(lapply(1:10, function(x){
  fread(paste0("O3Probabilities/region", x, "probabilities.csv"), drop = 1)
})))
rownames(ozone.prob.df) <- paste(ozone.prob.df$Column, ozone.prob.df$Row, sep = "_")

# create SpatialPointsDataFrame
centroid.matrix <- matrix(as.numeric(c(model.centroids$Longitude, model.centroids$Latitude)), ncol = 2,
                               dimnames = list(paste(model.centroids$Column, model.centroids$Row, sep = "_"),
                                               c("Longitude", "Latitude")))
ozone.prob.sp.df <- SpatialPointsDataFrame(centroid.matrix, ozone.prob.df, 
                                      proj4string = CRS(proj),
                                      match.ID = T)
# transform projection to lambert
ozone.prob.sp.df <- spTransform(ozone.prob.sp.df, CRS(lambert.proj))
# create data frame with coordinates
ozone.prob.df <- as.data.frame(ozone.prob.sp.df)
names(ozone.prob.df)[1:2] <- c("centroid_x", "centroid_y")

# merge with the monitor/centroid data frame
mon.centr.oz.prob.df <- merge(ozone.prob.df, monitor.centroid.df)


