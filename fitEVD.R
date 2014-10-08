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

library(extRemes)
fit <- fevd(O3_pred, data = test.cell.daily, threshold = 75, type="PP",
            #time.units = "365/year", 
            #units = "ppb", 
            verbose = TRUE)


data(Fort)
names(Fort)
