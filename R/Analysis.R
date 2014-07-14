rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)
library(visreg)
library(pbkrtest)
library(lmerTest)

source("R/function.R")

######################
# Process data frame #
######################
# source("R/ProcessData.R")
load("output//data/FACE_mineralisation.RData")

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
# post-co2 data for ancova with soil variables
postDF <- subsetD(mine, time != 1)
source("R//Stats.R")