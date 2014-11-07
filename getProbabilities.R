# Read in the fitted EVDs and get the probability of exceeding the NAAQS
library(parallel)


region1.files <- list.files("O3EVDs/region1")

# create an empty .csv file, with no header, for each region
lapply(1:10, function(x) file.create(paste0("O3Probabilities/region", x, "/probabilities.csv")))

# calculate the probability of exceeding the NAAQS for a given cell
calcCellProb <- function(column_row_region, pollutant, region){
  if(pollutant == "O3"){standard = 75}
  column <- strsplit(column_row_region, "_", fixed = T)[[1]][1]
  row <- strsplit(column_row_region, "_", fixed = T)[[1]][2]
  region <- strsplit(column_row_region, "_", fixed = T)[[1]][3]
  # load the object named 'fit'
  load(paste0(pollutant, "EVDs/region", region, "/col", column, "row", row, ".rda"))
  # probability that 4th largest value is above standard
  prob <- pgev(q = standard, loc = fit$mle[1], scale = fit$mle[2], shape = fit$mle[3],
               lower.tail = F)
  
  # pollutant = "O3"
  # region = 1
  # column = 24
  # row = 25
  # prob = .5
  
  file <- paste0(pollutant, "Probabilities/region", region, "probabilities.csv")
  cat(paste(column, row, prob, sep = ","), "\n", file = file, sep = "", append = T)
}

# loop through regions
lapplyRegions <- function(region, pollutant){
  # pollutant <- "O3"
  # region <- 1
  directory <- paste0(pollutant, "EVDs/region", region)
  # get all the .rda files in the directory for that region
  files <- list.files(directory)
  # create vector of column_row_region
  col_row <- sub("col", "", files, fixed = T)
  col_row <- sub(".rda", "", col_row, fixed = T)
  col_row <- sub("row", "_", col_row, fixed = T)
  col_row_region <- paste(col_row, region, sep = "_")
  rm(files, col_row)
  # loop through cells for the region and calculate the probability of exceeding NAAQS
  lapply(col_row_region, calcCellProb, pollutant = pollutant, region = region)
  
}


getProbabilities <- function(pollutant, number.of.regions = 10, parallel = T, clusters = number.of.regions){
  
  then <- Sys.time()
  
  if(parallel == T){
    
    # set the number of clusters
    cl <- makeCluster(clusters)
    
    clusterEvalQ(cl, library(evd))
    
    clusterExport(cl, "calcCellProb", envir=environment())
    
    # use multiple cores to fit an extreme value distribution to tables and save objects as .rda files
    parLapply(cl, 1:number.of.regions, lapplyRegions, pollutant = pollutant)
    
    stopCluster(cl)
    
  } else {
    
    # fit an extreme value distribution to tables and save object as .rda file
    lapply(1:number.of.regions, lapplyRegions, pollutant = pollutant)
    
  }
  
  # read in .csv files, delete empty rows, and assign column names
  lapply(1:number.of.regions, function(x){
    # x = 1
    # pollutant = "O3"
    file <- paste0(pollutant, "Probabilities/region", x, "probabilities.csv")
    data <- read.csv(file, header = F)
    colnames(data) <- c("Column", "Row", "Prob")
    write.csv(data, file = file)
  })
  print(then - Sys.time())
}

getProbabilities("O3", 10)




