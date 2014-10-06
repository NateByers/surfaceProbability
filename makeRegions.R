library(data.table)
library(dplyr)

# read in data
ozone.2008 <- fread("O3Surface_12km_2008.csv")


# get unique x/y's for each year
ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))
ozone.2008.xy <- arrange(ozone.2008.xy, Column, Row)

# get unique columns
columns <- unique(ozone.2008.xy$Column)

# make data.table of regions and merge with ozone.2008.xy
regions.dt <- data.table(Column = columns, region = rep(1:10, each = length(columns)/10)[1:length(columns)])
ozone.regions <- merge(subset(ozone.2008.xy, select = c("Column", "Row")), regions.dt, 
                       by = "Column", all = T)

# subset down to region 1 and plot
region.1 <- subset(ozone.regions, region == 1)

library(maps)
map("state")
points(region.1$Longitude, region.1$Latitude, pch = 20)


getGrid <- function (year, surface.type = "O3Surface_12km"){
  # year <- 2001
  dt <- fread(paste0(surface.type, "_", year, ".csv"))
  dt <- unique(dt, by = c("Column", "Row"))
  year <- subset(dt, select = c("Longitude", "Latitude", "Column", "Row"))
  year
}

ozone.grids <- lapply(as.character(2001:2008), getGrid)
names(ozone.grids) <- sapply(2001:2008, function(x)paste("O3Surface_12km", x, sep = "_"))
save(ozone.grids, file = "ozone_12km_grids.rda")


makeRegionalDFs <- function(csv.file, regional.dt = ozone.regions){
  all.dt <- fread(csv.file)
  all.dt <- merge(all.dt, regional.dt, by = c("Column", "Row"), all.x = T)
  file.name <- paste(strsplit(csv.file, "_", fixed = T)[[1]][1:2], collapse = "_", sep = "_")
  for(i in 1:10){ # i = 1
    write.csv(subset(all.dt, region == i), file = paste0(file.name, "_region_", i, ".csv"))
  }
}

makeRegionalDFs("O3Surface_12km_2001.csv")




ozone.2001 <- fread("O3Surface_12km_2001.csv")


# get unique x/y's for each year
ozone.2001.xy <- unique(ozone.2001, by = c("Column", "Row"))
ozone.2001.xy <- arrange(ozone.2001.xy, Column, Row)

# get unique columns
columns <- unique(ozone.2001.xy$Column)

# make data.table of regions and merge with ozone.2008.xy
regions.dt <- data.table(Column = columns, region = rep(1:10, each = length(columns)/10)[1:length(columns)])
ozone.2001.regions <- merge(subset(ozone.2001.xy, select = c("Longitude", "Latitude", "Column", "Row")), regions.dt, 
                       by = "Column", all = T)

# subset down to region 1 and plot
region.1 <- subset(ozone.2001.regions, Longitude < -99 & Latitude > 45)

library(maps)
map("state")
points(region.1$Longitude, region.1$Latitude, pch = 20)










