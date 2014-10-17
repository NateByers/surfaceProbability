# get some time series to work with 
library(data.table)
dt <- fread("O3Surface_12km_region_1.csv")
cells <- unique(subset(dt, select = c("Column", "Row", "Longitude", "Latitude")))
columns <- unique(subset(dt, select = "Column"))
rows <- unique(subset(dt, select = "Row"))


library(maps)
map("state")
points(cells$Longitude, cells$Latitude, pch = 20)

test.cell <- subset(cells, Column == 55 & Row == 150)
map("state")
points(test.cell$Longitude, test.cell$Latitude, pch = 20)

test.cell.daily <- subset(dt, Column == test.cell$Column & Row == test.cell$Row)
test.cell.daily <- test.cell.daily[, Date:=as.IDate(Date, format = "%m/ %d/ %Y")]
plot(O3_pred ~ Date, data = test.cell.daily)
abline(h = c(75, 65), col = c("red", "blue"))

library(extRemes)
fit <- fevd(O3_pred, data = test.cell.daily, threshold = 65, # there are no values over 75
            type="PP",
            time.units = "365/year", 
            units = "ppb", 
            verbose = TRUE)


library(ismev)
data(venice)
fit.rlarg <- rlarg.fit(venice[, -1])
rlarg.diag(fit.rlarg)
library(evd)
# probability that rth largest value is above q
pgev(q = 100, loc = fit.rlarg$mle[1], scale = fit.rlarg$mle[2], shape = fit.rlarg$mle[3],
     lower.tail = F)

library(dplyr)
test.cell.daily[, Year:=year(Date)]
test.cell.daily <- arrange(test.cell.daily, Year, desc(O3_pred))
test.cell.rlarg <- group_by(test.cell.daily, Year)
test.cell.rlarg <- summarize(test.cell.rlarg, r1 = first(O3_pred), r2 = nth(O3_pred, 2),
                             r3 = nth(O3_pred, 3), r4 = nth(O3_pred, 4))

fit.test.cell <- rlarg.fit(as.data.frame(test.cell.rlarg)[, -1])
# probability that 4th largest value is above 75
pgev(q = 75, loc = fit.test.cell$mle[1], scale = fit.test.cell$mle[2], shape = fit.test.cell$mle[3],
     lower.tail = F)


######################################################################
library(parallel)
library(ismev)
library(dplyr)
library(data.table)
library(parallel)
library(lubridate)

# this function takes a named data table and fits an extreme value distribution
fitEVD <- function(cell, type = c("rth largest", "threshold"), pollutant, region){
  if(type[1] == "rth largest"){
    # read in .csv file
    column <- strsplit(cell, "_")[[1]][1]
    row <- strsplit(cell, "_")[[1]][2]
    count <- strsplit(cell, "_")[[1]][3]
    csv.file <- file <- paste0("EVDs/", region, "/col", column, "row", row, ".csv")
    cell.dt <- fread(csv.file)
    
    # cell.dt <- fread("EVDs/region1/col24row19.csv")
    # pollutant <- "O3_pred"
    
    # add a year column
    cell.dt[, Year := year(as.Date(Date, format = "%m/ %d/ %Y"))]
    
    # order by year and descending predicted values
    call <- substitute(cell.dt <- arrange(cell.dt, Year, desc(pollutant)), 
                       list(pollutant = as.name(pollutant)))
    eval(call) # this is necessary to pass variable name to the dplyr function
    
    # group by year
    cell.dt <- group_by(cell.dt, Year)
    
    # create table of top 4 values per year
    call <- substitute(cell.summary <- summarize(cell.dt, r1 = first(pollutant), r2 = nth(pollutant, 2),
                                                 r3 = nth(pollutant, 3), r4 = nth(pollutant, 4)), 
                       list(pollutant = as.name(pollutant)))
    eval(call)  # this is necessary to pass variable name to the dplyr function
    
    # fit an extreme value distribution of the 4 largest values
    fit <- rlarg.fit(cell.summary[, Year := NULL])
    
    # save as .rda file
    rda.file <- file <- paste0("EVDs/", region, "/col", column, "row", row, ".rda")
    save(fit, file = rda.file)
    cat("Fit ", count, " EVDs\n")
    unlink(csv.file)
    rm(cell.data.table)
    gc()
  }
}

getEVDfits <- function(regional.data.table, pollutant = c("O3_pred"), region, method = c("rth largest", "threshold"),
                       parallel = T, clusters = (detectCores() - 1)){
  # get time
  then <- Sys.time()
  
  # create vector of Column_Row_Count to identify cells and keep track of progress
  cells <- unique(subset(regional.data.table, select = c("Column", "Row")))
  cells <- paste(cells$Column, cells$Row, 1:dim(cells)[1], sep = "_")  
  
  # set the key for quick subsetting
  regional.data.table <- setkey(regional.data.table, Column, Row)
  
  # split up the data into text files
  lapply(cells, function(col_row_count, data){
    column <- as.integer(strsplit(col_row_count, "_", fixed = T)[[1]][1])       # get the column number
    row <- as.integer(strsplit(col_row_count, "_", fixed = T)[[1]][2])          # get the row number
    count <- strsplit(col_row_count, "_", fixed = T)[[1]][3]        # get the count
    sub.data <- data[list(column, row)]   # subset down to the daily values for that cell 
    file <- paste0("EVDs/", region, "/col", column, "row", row, ".csv") # create file name
    write.csv(sub.data, file = file)                           # save as .csv file
    rm(sub.data)
    cat("Written ", count, " files\n")
  }, data = regional.data.table)
  
  print("all csv files written")
  print("fitting EVDs")
  
  if(parallel == T){
    
    # set the number of clusters
    cl <- makeCluster(clusters)
    
    clusterEvalQ(cl, {library(data.table); library(lubridate); library(dplyr); library(ismev)})
    
    # use multiple cores to fit an extreme value distribution to tables and save objects as .rda files
    parLapply(cl, cells, fitEVD, type = method[1], pollutant = pollutant, region = region)
    
    stopCluster(cl)
    
  } else {
    
    # fit an extreme value distribution to tables and save object as .rda file
    lapply(cells, fitEVD, type = method[1], pollutant = pollutant, region = region)
    
  }
  # print time difference
  then - Sys.time()
}

# read in region 1
region1.dt <- fread("O3Surface_12km_region_1.csv")

# get test subset
cells <- unique(subset(region1.dt, select = c("Column", "Row", "Longitude", "Latitude")))
sub.cells <- cells[1:60, ]
sub.region1.dt <- merge(region1.dt[, !"V1", with = F], sub.cells, by = c("Column", "Row", "Longitude", "Latitude"))

getEVDfits(sub.region1.dt, region = "region1", clusters = 10)


getEVDfits(region1.dt, region = "region1")



############################## New version #####################################################
library(parallel)
library(ismev)
library(dplyr)
library(data.table)
library(lubridate)

fitEVD <- function(){
  # fit a single EVD
}

lapplyEVDS <- function(){
  data <- fread()
  column_row <- select()
  
  
}

getEVDfits <- function(regional.files, parallel = T){
  
  if(parallel == T){
    # take list of files and parLapply
    
    # set the number of clusters
    cl <- makeCluster(clusters)
    
    # load libraries in each cluster
    clusterEvalQ(cl, {library(data.table); library(lubridate); library(dplyr); library(ismev)})
    
    parLapply(cl, regional.files, lapplyEVDs)
    
    stopCluster(cl)
  }
  
}








