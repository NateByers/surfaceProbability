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


# create regional subdirectories under the EVDs/ folder
lapply(1:10, function(x) dir.create(paste0("O3EVDs/region", x)))

# this function takes a named data table and fits an extreme value distribution
fitEVD <- function(cell, type = c("rth largest", "threshold"), pollutant, region){
  pollutant.column <- paste(pollutant, "pred", sep = "_")
  if(type[1] == "rth largest"){
    # read in .csv file
    column <- strsplit(cell, "_")[[1]][1]
    row <- strsplit(cell, "_")[[1]][2]
    count <- strsplit(cell, "_")[[1]][3]
    csv.file <- file <- paste0(pollutant, "EVDs/", region, "/col", column, "row", row, ".csv")
    cell.dt <- fread(csv.file)
    
    # cell.dt <- fread("O3EVDs/region1/col24row19.csv")
    # pollutant <- "O3_pred"
    
    # add a year column
    cell.dt[, Year := year(as.Date(Date, format = "%m/ %d/ %Y"))]
    
    # order by year and descending predicted values
    call <- substitute(cell.dt <- arrange(cell.dt, Year, desc(pollutant.column)), 
                       list(pollutant.column = as.name(pollutant.column)))
    eval(call) # this is necessary to pass variable name to the dplyr function
    
    # group by year
    cell.dt <- group_by(cell.dt, Year)
    
    # create table of top 4 values per year
    call <- substitute(cell.summary <- summarize(cell.dt, r1 = first(pollutant.column), r2 = nth(pollutant.column, 2),
                                                 r3 = nth(pollutant.column, 3), r4 = nth(pollutant.column, 4)), 
                       list(pollutant.column = as.name(pollutant.column)))
    eval(call)  # this is necessary to pass variable name to the dplyr function
    
    # fit an extreme value distribution of the 4 largest values
    fit <- rlarg.fit(cell.summary[, Year := NULL])
    
    # save as .rda file
    rda.file <- file <- paste0(pollutant, "EVDs/", region, "/col", column, "row", row, ".rda")
    save(fit, file = rda.file)
    cat("Fit ", count, " EVDs\n")
    unlink(csv.file)
    rm(cell.data.table)
    gc()
  }
}

getEVDfits <- function(regional.data.file, pollutant = "O3", region, 
                       method = c("rth largest", "threshold"),
                       parallel = T, clusters = (detectCores() - 1)){
  # get time
  then <- Sys.time()
  
  regional.data.table <- fread(regional.data.file)
  
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
    file <- paste0(pollutant, "EVDs/", region, "/col", column, "row", row, ".csv") # create file name
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

getEVDfits("O3Surface_12km_region_1.csv", region = "region1")
getEVDfits("O3Surface_12km_region_2.csv", region = "region2")
getEVDfits("O3Surface_12km_region_3.csv", region = "region3")
getEVDfits("O3Surface_12km_region_4.csv", region = "region4")
getEVDfits("O3Surface_12km_region_5.csv", region = "region5")
getEVDfits("O3Surface_12km_region_6.csv", region = "region6")
getEVDfits("O3Surface_12km_region_7.csv", region = "region7")
getEVDfits("O3Surface_12km_region_8.csv", region = "region8")
getEVDfits("O3Surface_12km_region_9.csv", region = "region9")
getEVDfits("O3Surface_12km_region_10.csv", region = "region10")




############################## New version #####################################################
library(parallel)
library(ismev)
library(dplyr)
library(data.table)
library(lubridate)

fitEVD <- function(cell, data, pollutant, method){
  # read in .csv file and get column, row, and count
  column <- strsplit(cell, "_")[[1]][1]
  row <- strsplit(cell, "_")[[1]][2]
  count <- strsplit(col_row_count, "_", fixed = T)[[1]][3] 
  # subset the data table
  data <- data[list(column, row)]
  # order by year and descending predicted values
  call <- substitute(data <- arrange(data, Year, desc(pollutant)), 
                     list(pollutant = as.name(pollutant)))
  eval(call) # this is necessary to pass variable name to the dplyr function
  
  if(method == "rth largest"){
    # fit an extreme value distribution of the 4 largest values
    fit <- rlarg.fit(cell.summary[, Year := NULL])
  }
  
  # save as .rda file
  rda.file <- file <- paste0("EVDs/col", column, "row", row, ".rda")
  save(fit, file = rda.file)
  rm(data)
  # write progress to text file
  writeLines(paste0(count, "\n"), "progress.txt")
  gc()
}

lapplyEVDs <- function(file, pollutant, method = method){
  # read in the regional file
  data <- fread(file)
  # create vector identifying cells and counting progress
  column_row_count <- paste(data$Column, data$Row, 1:dim(data)[1], sep = "_")
  
  # add a year column
  data[, Year := year(as.Date(Date, format = "%m/ %d/ %Y"))]
  
  # set the key for fast subsetting
  data <- setkey(data, Column, Row)
  
  lapply(column_row_count, fitEVD, data = data, pollutant = pollutant, method = method)
  
}

getEVDfits <- function(regional.files, pollutant = c("O3_pred"), 
                       method = c("rth largest", "threshold"), parallel = T,
                       clusters = length(regional.files)){
  then <- Sys.time
  
  if(parallel == T){
    # take list of files and parLapply
    
    # set the number of clusters
    cl <- makeCluster(clusters)
    
    # load libraries and functions in each cluster
    clusterEvalQ(cl, {library(data.table)
                      library(lubridate)
                      library(dplyr)
                      library(ismev)})
    
    parLapply(cl, regional.files, lapplyEVDs, pollutant = pollutant[1], method = method[1])
    
    stopCluster(cl)
  }
  
  print(then - Sys.time)
}


regional.files <- grep("O3Surface_12km_region", list.files(), fixed = T, value = T)

getEVDfits(regional.files)




