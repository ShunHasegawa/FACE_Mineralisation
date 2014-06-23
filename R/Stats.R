##################
# Moist and Temp #
##################
scatterplotMatrix(~ log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                  data = mine, diag = "boxplot")

# check correlation
mine$logMoist <- log(mine$Moist)
with(mine, cor(cbind(logMoist, Temp_Max, Temp_Min, Temp_Mean)))

# moisture and temperature appear to be negatively 
# correlated, so don't use them in the model at the same
# time

theme_set(theme_bw())
p <- ggplot(subsetD(mine, time != 1), aes(x = Temp_Min, y = log(Moist), col = ring))
p2 <- p + geom_point(alpha = .5) 

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output//figs/FACE_mine_SoilVa_Ringr", plot = pl, width = 6, height = 6)

ggsavePP(file = "output//figs/FACE_Mineralisation_SoilVa", plot = p2, width = 6, height = 6)


#################
# Nitrification #
#################
source("R//Stats_Nitrification.R")

####################
# N mineralisation #
####################
source("R/Stats_N_mineralisation.R")

####################
# P mineralisation #
####################
source("R/Stats_P_mineralisation.R")