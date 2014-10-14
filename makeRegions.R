library(data.table)
library(dplyr)
library(reshape)

# read in data
ozone.2008 <- fread("O3Surface_12km_2008.csv")


# get unique x/y's for each year
ozone.2008.xy <- unique(ozone.2008, by = c("Column", "Row"))


# get unique columns
columns <- unique(ozone.2008.xy$Column)
rows <- unique(ozone.2008.xy$Row)

# make data.table of regions and merge with ozone.2008.xy
regions.dt <- data.table(Column = columns, region = rep(1:10, each = length(columns)/10)[1:length(columns)])
ozone.regions <- merge(subset(ozone.2008.xy, select = c("Column", "Row")), regions.dt, 
                       by = "Column", all = T)

# subset down to region 1 and plot
region.1 <- subset(ozone.regions, region == 1)

library(maps)
map("state")
points(region.1$Longitude, region.1$Latitude, pch = 20)

# read in yearly data and get a data.table of the centroids for that year
getCentroid <- function (year, surface.type = "O3Surface_12km"){
  # year <- 2001
  dt <- fread(paste0(surface.type, "_", year, ".csv"))
  dt <- unique(dt, by = c("Column", "Row"))
  year <- subset(dt, select = c("Longitude", "Latitude", "Column", "Row"))
  year
}

ozone.centroids <- lapply(as.character(2001:2008), getCentroid)
names(ozone.centroids) <- sapply(2001:2008, function(x)paste("O3Surface_12km", x, sep = "_"))
save(ozone.centroids, file = "ozone_12km_centroids.rda")

# function for identifying records in data frame A that are not contained in data frame B http://www.r-bloggers.com/identifying-records-in-data-frame-a-that-are-not-contained-in-data-frame-b-%E2%80%93-a-comparison/
compareDFs <- function(x.1,x.2,...){
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
}

diff <- compareDFs(as.data.frame(ozone.centroids[[8]][, 1:2]), as.data.frame(ozone.centroids[[6]][, 1:2]))

library(maps)
map("state")
points(diff$Longitude, diff$Latitude, pch = 20)

map("state")
points(ozone.centroids[[6]]$Longitude, ozone.centroids[[6]]$Latitude)


## get unique columns from 2008 data
columns <- unique(ozone.centroids[["O3Surface_12km_2008"]]$Column)

# make data.table of regions and merge with ozone.2008.xy
regions.dt <- data.table(Column = columns, region = rep(1:10, each = ceiling(length(columns)/10))[1:length(columns)])
ozone.regions <- merge(subset(ozone.centroids[["O3Surface_12km_2008"]], select = c("Column", "Row")), regions.dt, 
                       by = "Column", all = T)

# read in a file for one year, split it up into regional data tables, and save each table as a .csv file
splitYearlyFile <- function(csv.file, regional.dt, parallel = F, clusters){ 
  all.dt <- fread(csv.file)
  all.dt <- merge(all.dt, regional.dt, by = c("Column", "Row"), all.x = T)
  file.name <- strsplit(csv.file, ".", fixed = T)[[1]][1]
  if(parallel == T){
    library(parallel)
    cl <- makeCluster(clusters)
    parLapply(cl, 1:10, function(x){
      write.csv(subset(all.dt, region == x), file = paste0(file.name, "_region_", x, ".csv"))
    })
    stopCluster(cl)
    
  } else{
    for(i in 1:10){ 
      write.csv(subset(all.dt, region == i), file = paste0(file.name, "_region_", i, ".csv"))
    }
  }
  
}

splitYearlyFile("O3Surface_12km_2008.csv", ozone.regions)
splitYearlyFile("O3Surface_12km_2007.csv", ozone.regions, parallel = T, clusters = 10)


makeRegionalFiles <- function(pollutant.resolution, years, regions){
#   pollutant.resolution = "O3Surface_12km"
#   years = 2007:2008
#   regions = 1:5
  
  
  loadFile <- function(year, region){
    # year = 2007
    fread(paste0(pollutant.resolution, "_", year, "_region_", region, ".csv"))
  }
  
  writeRegionalFile <- function(region){
    # region = 1
    df <- rbindlist(lapply(years, loadFile, region = region))
    write.csv(df, file = paste0(pollutant.resolution, "_region_", region, ".csv"))
    rm(df)
    gc()
  }
  
  lapply(regions, writeRegionalFile)
  
}

makeRegionalFiles("O3Surface_12km", 2007:2008, 1:5)
makeRegionalFiles("O3Surface_12km", 2007:2008, 6:10)








