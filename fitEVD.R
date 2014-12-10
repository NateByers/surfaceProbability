library(ismev)
library(evd)
library(data.table)
library(bit64)
library(dplyr)
library(pbapply)
library(parallel)
library(mail)

# Create CensusTractSubsets/ directory
dir.create("CensusTractSubsets")
# Create EVD directories
directories <- c(paste0("ozone", c(75, 70, 65), "EVDs"))
lapply(c("PM35EVDs", directories), dir.create)

# This function takes a data.table, subsets down to the data for a pollutant/census-tract, 
# and saves it as an .rdata file
makeCensusTractSubsets <- function(census.tract, pollutant, data){
  data <- subset(data, CTFIPS == census.tract)
  
  save(data, file = paste0("CensusTractSubsets/pol_", pollutant, "_CT_", census.tract, ".rdata"))
}

# to get merged data, source downloadDownscaler.R and mergeDownscaler.R scripts
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


# This function reads in an .rdata file, fits an EVD, saves the fitted object,
# and returns a probability of the rth value exceeding the threshold
fitEVD <- function(rdata.file, rth, threshold, object.folder){
  
  load(paste0("CensusTractSubsets/", rdata.file))
  data <- data[order(year)]
  
  # fit an extreme value distribution of the 4 largest values
  fit <- rlarg.fit(data[, paste0("r", 1:rth), with = FALSE])
  
  # save the object
  save(fit, file = paste0(object.folder, "/", strsplit(rdata.file, ".", fixed = T)[[1]][1],
                          "_", threshold, ".rdata"))
  
  # probability that rth largest value is above threshold
  prob <- pgev(q = threshold, loc = fit$mle[1], scale = fit$mle[2], shape = fit$mle[3],
               lower.tail = F)
  prob
}

# Get all files in CensusTractSubsets/ director
files <- list.files("CensusTractSubsets")

# Get ozone files
ozone.files <- files[grepl("ozone", files, fixed = TRUE)]

# Get ozone probabilities with 75 threshold
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {library(data.table); library(ismev); library(evd)})
ozone.probabilities.75 <- parSapply(cl, ozone.files, fitEVD, rth = 4, 
                                    threshold = 75, object.folder = "ozone75EVDs")
stopCluster(cl)
# make data frame and save as .csv file
ct.ozone <- regmatches(names(ozone.probabilities.75), regexpr("\\d+", names(ozone.probabilities.75)))
write.csv(data.frame(CTFIPS = ct.ozone, prob = ozone.probabilities.75),
          file = "ozoneProbabilities75ppbThreshold.csv")

# Get ozone probabilities with 70 threshold
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {library(data.table); library(ismev); library(evd)})
ozone.probabilities.70 <- parSapply(cl, ozone.files, fitEVD, rth = 4, 
                                    threshold = 70, object.folder = "ozone70EVDs")
stopCluster(cl)
# make data frame and save as .csv file
ct.ozone <- regmatches(names(ozone.probabilities.70), regexpr("\\d+", names(ozone.probabilities.70)))
write.csv(data.frame(CTFIPS = ct.ozone, prob = ozone.probabilities.70),
          file = "ozoneProbabilities70ppbThreshold.csv")

# Get ozone probabilities with 65 threshold
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {library(data.table); library(ismev); library(evd)})
then <- Sys.time()
ozone.probabilities.65 <- parSapply(cl, ozone.files, fitEVD, rth = 4, 
                                    threshold = 65, object.folder = "ozone65EVDs")
elapsed <- then - Sys.time()
stopCluster(cl)
# make data frame and save as .csv file
ct.ozone <- regmatches(names(ozone.probabilities.65), regexpr("\\d+", names(ozone.probabilities.65)))
write.csv(data.frame(CTFIPS = ct.ozone, prob = ozone.probabilities.65),
          file = "ozoneProbabilities65ppbThreshold.csv")


# Get PM files
pm.files <- files[grepl("PM", files, fixed = TRUE)]

# Get PM probabilities
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {library(data.table); library(ismev); library(evd)})
then <- Sys.time()
pm.probabilities.35 <- parSapply(cl, pm.files, fitEVD, rth = 7, threshold = 35)
Sys.time() - then
stopCluster(cl)
# make data frame and save as .csv file
ct.pm <- regmatches(names(pm.probabilities.35), regexpr("\\d+", names(pm.probabilities.35)))
write.csv(data.frame(CTFIPS = ct.pm, prob = pm.probabilities.35),
          file = "pmProbabilities35ug_m3Threshold.csv")

