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
library(visreg)

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
source("R//Stats.R")