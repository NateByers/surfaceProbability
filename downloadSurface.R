# download ozone surface data 2007-2008

downloadSurface <- function(file, year){
  temp <- tempfile()
  download.file(file, temp)
  unzip(temp)
  file.remove(temp)
}

ozone.link <- "http://www.epa.gov/heasd/documents/cdc/HBM/ozone/"

ozone.zip.files <- sapply(7:8, function(year, url){
  paste0(url, "O3Surface_12km_200", year, ".zip")
}, url = ozone.link)

mapply(downloadSurface, file = ozone.zip.files, year = 2007:2008)

# read in ozone files and cut them down to just the 4 highest values for the year
library(data.table)
library(dplyr)

readAndProcessOzone <- function(file){
  data <- fread(file)
  data <- group_by(data, Column, Row)
  data <- summarize(data, first_high = first(O3_pred), second_high = nth(O3_pred, 2),
                    third_high = nth(O3_pred, 3), fourth_high = nth(O3_pred, 4))
  write.csv(data, file = paste0("processed_", file), row.names = F)
  rm(data)
  gc()
}

ozone.csv.files <- ozone.zip.files <- sapply(7:8, function(year){
  paste0("O3Surface_12km_200", year, ".csv")
})

lapply(ozone.csv.files, readAndProcessOzone)
