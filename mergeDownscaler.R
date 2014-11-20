# To download the 2007-2008 downscaler data from the EPA website,
# source the downloadDownscaler.R script. This code reads in the downloaded
# and processed 2007-2008 data and merges it with the CDC downscaler
# data for 2009-2011.

# Read in processed downscaler data for 2007-2008 and rbind into
# one data table
combos <- expand.grid(c("07", "08"), c("ozone", "PM"))
year.pollutant <- paste(combos[, 1], combos[, 2], sep = ".")

ds.2007.2008.list <- lapply(year.pollutant, function(x){
  year <- strsplit(x, ".", fixed = TRUE)[[1]][1]
  pollutant <- strsplit(x, ".", fixed = TRUE)[[1]][2]
  data <- read.csv(paste0("processed_Predictions", year, pollutant, "-Census2010edited.csv"))
  data$year = rep(paste0("20", year), dim(data)[1])
  data$pollutant = rep(pollutant, dim(data)[1])
  data
})

ds.2007.2008.df <- do.call(rbind, ds.2007.2008.list)

# Read in the CDC downscaler data for 2009-2011 and combine with 2007-2011
ds.2009.2011.ozone.df <- read.csv("ozone_max_209_11.csv") 
ds.2009.2011.PM.df <- read.csv("PM_max_209_11.csv")
ds.2009.2011.df <- rbind(data.frame(ds.2009.2011.ozone.df[c("CTFIPS", "year", "Max")],
                                    pred = ds.2009.2011.ozone.df$Ozone,
                                    pollutant = rep("ozone", dim(ds.2009.2011.ozone.df)[1])),
                         data.frame(ds.2009.2011.PM.df[c("CTFIPS", "year", "Max")],
                                    pred = ds.2009.2011.PM.df$PM, 
                                    pollutant = rep("PM", dim(ds.2009.2011.PM.df)[1])))

# change from long to wide
library(reshape2)
ds.2009.2011.df <- dcast(ds.2009.2011.df, CTFIPS + year + pollutant ~ Max, value.var = "pred")
names(ds.2009.2011.df)[names(ds.2009.2011.df) %in% as.character(1:7)] <- paste0("r", 1:7)
# merge lat/longs with 2009-2011 data
ds.2009.2011.df <- merge(ds.2009.2011.df[, !(names(ds.2009.2011.df) %in% as.character(5:7))], 
                         unique(ds.2007.2008.df[, c("CTFIPS", "Latitude", "Longitude")]))


# rbind the two tables together
ds.2007.2011.df <- rbind(ds.2007.2008.df, ds.2009.2011.df[names(ds.2007.2008.df)])
write.csv(ds.2007.2011.df, file = "merged_Predictions_CT_2007_2011.csv")
