theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(mineMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))

#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrification", "N_mineralisation", "P_minerlisation")

RngFg <- dlply(RngMean, .(variable), PltRnghMean)
fls <- paste("output//figs/FACE_mineralisation_Ring_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltCO2Mean)
fls <- paste("output//figs/FACE_mineralisation_CO2_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'nitrification' = expression(Net~nitrification~rate),
  'nh' = expression(Net~N_mineralisation~rate),
  'po' = expression(Net~P_mineralisation~rate)
  )

ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltCO2Mean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "output//figs/FACE_Mineralisation_CO2Trt", plot = pl, width = 6, height = 6)
