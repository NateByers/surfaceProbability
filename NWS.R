library(rNOMADS)

download.file("http://weather.noaa.gov/pub/SL.us008001/ST.opnl/DF.gr2/DC.ndgd/GT.aq/AR.conus/ds.mozone08.bin", "mozone08.bin")


ip <- file("mozone08.bin", "rb")

flag <- readChar(ip, 19)
sh <- readChar(ip, 21)
flag2 <- readChar(ip, 19)
sh2 <- readChar(ip, 21)

readChar(ip, 4)
readChar(ip, 2)
readChar(ip, 1)
readChar(ip, 1)
readChar(ip,  7)

readChar(ip, 4)

close(ip)


