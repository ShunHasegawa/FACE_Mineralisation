library(lubridate)
library(plyr)
library(reshape)

source("functions//function.R")

fils <- dir(path = "Data/AQ2/row.data/ReadyToProcess/", pattern = ".csv$", full.names = TRUE)

write.csv(cmbn.fls(fils), "Data/AQ2/processed.dat.csv", row.names = FALSE) 

