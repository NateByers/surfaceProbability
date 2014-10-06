# download ozone surface data 2001-2008


link <- "http://www.epa.gov/heasd/documents/cdc/HBM/ozone/"

files <- sapply(1:8, function(year, url){
  paste0(url, "O3Surface_12km_200", year, ".zip")
}, url = link)

downloadSurface <- function(file, year, pollutant){
  temp <- tempfile()
  download.file(file, temp)
  unzip(temp)
  unlink(temp)
}

mapply(downloadSurface, file = files, year = 2001:2008, MoreArgs = list(pollutant = "ozone"))
