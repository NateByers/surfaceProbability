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

# this function takes a named data table and fits an extreme value distribution
fitEVD <- function(cell.data.table, type = c("rth largest", "threshold")){
  if(type[1] == "rth largest"){
    
    # add a year column
    cell.data.table[, Year:=year(Date)]
    # order by year and descending predicted values
    cell.data.table <- arrange(cell.data.table, Year, desc(contains("pred")))
    # group by year
    cell.data.table <- group_by(cell.data.table, Year)
    # create table of top 4 values per year
    cell.summary <- summarize(cell.data.table, r1 = first(O3_pred), r2 = nth(O3_pred, 2),
                              r3 = nth(O3_pred, 3), r4 = nth(O3_pred, 4))
    # fit an extreme value distribution of the 4 largest values
    rlarg.fit(as.data.frame(data)[, -1])
  }
}

getEVDfits <- function(regional.data.table, method = c("rth largest", "threshold"),
                       parallel = T, clusters = (detectCores() - 1)){
  
  # create vector of Column_Row to identify cells
  cells <- paste(sprintf("%03d", regional.data.table$Column), 
                         sprintf("%03d", regional.data.table$Column), sep = "_")
  
  if(parallel == T){
    
    # set the number of clusters
    cl <- makeCluster(clusters)
    
    # use multiple cores to split up the data into a list of data tables
    table.list <- parLapply(cl, cells, function(x){
      column <- strsplit(x, "_", fixed = T)[[1]][1] # get the column number
      row <- strsplit(x, "_", fixed = T)[[1]][2]    # get the row number
      subset(data, Column == column & Row == row)   # subset down to the daily values for that cell
    }, data = regional.data.table)
    
    # use multiple cores to fit an extreme value distribution to tables
    fit.list <- parLapply(cl, table.list, fitEVD, type = method[1])
    
    stopCluster(cl)
    
    # attach names to the list members
    names(fit.list) <- cells
    
    fit.list
    
  } else {
    # split up the data into a list of data tables
    tables.list <- lapply(cells, function(x){
      column <- strsplit(x, "_", fixed = T)[[1]][1] # get the column number
      row <- strsplit(x, "_", fixed = T)[[1]][2]    # get the row number
      subset(data, Column == column & Row == row)   # subset down to the daily values for that cell 
    }, data = regional.data.table)
    
    # fit an extreme value distribution to tables
    fit.list <- lapply(table.list, fitEVD, type = method[1])
    
    # attach names to the list members
    names(fit.list) <- cells
    
    fit.list
  }
  
}

region1.dt <- fread("O3Surface_12km_region_1.csv")
region1.fits <- getEVDfits(region1.dt)
Sys.time()