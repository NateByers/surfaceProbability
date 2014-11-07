library(data.table)
library(dplyr)

# This code assumes that you have enough RAM to throw around a file that's 1.7 GB

# create a temporary file
temp <- tempfile()
# download the .zip file to the temorary location--this will take several minutes
download.file('http://www.epa.gov/heasd/documents/cdc/HBM/ozone/O3Surface_12km_2008.zip', temp)
# unzip temporary file to your working directory
unzip(temp)
# delete the temporary file
unlink(temp)

# Read the data into R
data <- fread('O3Surface_12km_2008.csv')
# add a year column
data[, Year := year(as.Date(Date, format = "%m/ %d/ %Y"))]
# identify columns to group by
data <- group_by(data, Column, Row, Year)
# create summary table that gives the top 4 values per year for every column/row combination
yearly.summary <- summarize(data, r1 = first(O3_pred), r2 = nth(O3_pred, 2),
                            r3 = nth(O3_pred, 3), r4 = nth(O3_pred, 4))
                            
