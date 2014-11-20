# download downscaler data 2007-2008

downloadSurface <- function(file){
  temp <- tempfile()
  download.file(file, temp)
  unzip(temp)
  file.remove(temp)
}

epa.link <- "http://www.epa.gov/esd/land-sci/lcb/zipfiles_may12/"

zip.files <- sapply(c("07", "08"), function(year, url){
  sapply(c("O3", "PM2.5"), function(pollutant){
    paste0(url, "DS", pollutant, "Surface_CenTr_20", year, "_0512.zip")
  })
}, url = epa.link)

lapply(c(zip.files["O3", ], zip.files["PM2.5", ]), downloadSurface)

# read in ozone files and cut them down to just the 4 highest values for the year
library(data.table)
library(dplyr)

readAndProcessDownscaler <- function(file){
  # file = "Predictions07ozone-Census2010edited.csv"
  pollutant <- strsplit(file, "-", fixed = TRUE)[[1]][1]
  pollutant <- strsplit(pollutant, "[7-8]")[[1]][2]
  data <- fread(file)
  names(data) <- c("Date", "CTFIPS", "Latitude", "Longitude", "pred", "se")
  data <- group_by(data, CTFIPS, Latitude, Longitude)
  data <- summarize(data, r1 = first(pred), r2 = nth(pred, 2),
                    r3 = nth(pred, 3), r4 = nth(pred, 4), r5 = nth(pred, 5),
                    r6 = nth(pred, 6), r7 = nth(pred, 7))
  data$pollutant <- rep(pollutant, dim(data)[1])
  write.csv(data, file = paste0("processed_", file), row.names = F)
  rm(data)
  gc()
}

csv.files <- sapply(c("07", "08"), function(year){
  sapply(c("ozone", "PM"), function(pollutant){
    paste0("Predictions", year, pollutant, "-Census2010edited.csv")
  })
})

lapply(c(csv.files["ozone", ], csv.files["PM", ]), readAndProcessDownscaler)
