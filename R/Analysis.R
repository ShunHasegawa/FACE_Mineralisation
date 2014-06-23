rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)

source("R/function.R")

######################
# Process data frame #
######################
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

# save
save(mine, file = "output//data/FACE_mineralisation.RData")

#######################
# Excel summary table #
#######################
source("R/SummaryExlTable.R")

########
# Figs #
########
source("R//Figs.R")

#########
# Stats #
#########
source("R//Stats.R")