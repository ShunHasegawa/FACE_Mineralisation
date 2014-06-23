mine<-read.csv("Data/mineralisation.kgbais.csv",
               colClasses=c("ring"="factor",
                            "plot"="factor",
                            "time"="factor",
                            "coverage" = "NULL"))

# remove unnecessary rows
mine <- droplevels(mine[complete.cases(mine), ])

# organise data frame
mine <- within(mine, {
  insertion <- as.Date(dmy(insertion))
  sampling <- as.Date(dmy(sampling))
  
  # id for later analysis
  id <- ring:plot
  
  # block for later analysis
  block <- recode(ring, "1:2 = 'A'; 3:4 = 'B'; 5:6 = 'C'")
})

# middle date of insertion and sampling
MeanDate <- apply(cbind(mine$insertion, mine$sampling), 1, mean)
mine$date <- as.Date(ave(MeanDate, mine$time), origin = origin) # same date for same time


##################
# soil variables #
##################
load("Data/FACE_TDR_ProbeDF.RData")

# subset iem
TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

# compute mean of soil variables for given period
SoilVarDD <- function(data, rings, plots, Start, End){
  sDF <- subset(data, Date >= Start & Date >= End & ring == rings & plot == plots)
  ddply(sDF, .(ring, plot),function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE))
}

MineSoil <- ddply(mine, .(insertion, sampling, ring, plot), 
                  function(x) SoilVarDD(data = TdrSoil, Start = x$insertion, End = x$sampling, rings = x$ring, plot = x$plot))

# merge
mine <- merge(mine, MineSoil, by = c("insertion", "sampling", "ring", "plot"))
