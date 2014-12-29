rm(list=ls(all=TRUE))

source("R/pckg.R")

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

# save all objects so far
save.image(file = "output//data/Allobj.RData")

#########
# Stats #
#########
# post-co2 data for ancova with soil variables
postDF <- subsetD(mine, time != 1)
save(postDF, file = "output//data/postDF.RData")

source("R//Stats.R")
