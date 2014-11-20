# Create CensusTractSubsets/ directory
dir.create("CensusTractSubsets")

library(ismev)
library(evd)
library(data.table)
library(bit64)
library(dplyr)
library(pbapply)
library(parallel)


# This function takes a data.table, subsets down to the data for a pollutant/census-tract, 
# and saves it as an .rdata file
makeCensusTractSubsets <- function(census.tract, pollutant, data){
  data <- subset(data, CTFIPS == census.tract)
  
  save(data, file = paste0("CensusTractSubsets/pol_", pollutant, "_CT_", census.tract, ".rdata"))
}


merged.predictions.dt <- fread("merged_Predictions_CT_2007_2011.csv", integer64 = "character")
CT <- unique(merged.predictions.dt$CTFIPS)
# make ozone census tract subsets
ozone.predictions.dt <- subset(merged.predictions.dt, pollutant == "ozone")
setkey(ozone.predictions.dt, CTFIPS, year)
then <- Sys.time()
pblapply(CT, makeCensusTractSubsets, pollutant = "ozone", 
       data = ozone.predictions.dt)
Sys.time() - then

# make PM census tract subsets
library(R.utils)
PM.predictions.dt <- subset(merged.predictions.dt, pollutant == "PM")
setkey(PM.predictions.dt, CTFIPS, year)
total <- length(CT)
pb <- txtProgressBar(min = 0, max = total, style = 3)
then <- Sys.time()
for(i in 1:total){
  makeCensusTractSubsets(census.tract = CT[i], pollutant = "PM",
                         data = PM.predictions.dt)
  setTxtProgressBar(pb, i)
}
Sys.time() - then
close(pb)





# This function reads in an .rdata file, fits an EVD, and returns a probability
# of the rth value exceeding the threshold
fitEVD <- function(rdata.file, rth, threshold){
  # rdata.file <- "pol_ozone_CT_1001021000.rdata"
  # rth <- 4
  # threshold <- 75
  load(paste0("CensusTractSubsets/", rdata.file))
  data <- data[order(year)]
  # fit an extreme value distribution of the 4 largest values
  fit <- rlarg.fit(data[, paste0("r", 1:rth), with = FALSE])
  
  # probability that rth largest value is above threshold
  prob <- pgev(q = threshold, loc = fit$mle[1], scale = fit$mle[2], shape = fit$mle[3],
               lower.tail = F)
  prob
}

# Get all files in CensusTractSubsets/ director
files <- list.files("CensusTractSubsets")

# Get ozone files
ozone.files <- files[grepl("ozone", files, fixed = TRUE)]

# Get ozone probabilities
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {library(data.table); library(ismev); library(evd)})

ozone.probabilities.75 <- parSapply(cl, ozone.files, fitEVD, rth = 4, threshold = 75)

stopCluster(cl)

save(ozone.probabilities.75, file = "ozoneProbabilities75ppbThreshold.rdata")
