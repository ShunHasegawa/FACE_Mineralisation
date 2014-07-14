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

#######################
# Plot soil variables #
#######################

##########################
## Process raw TDR data ##
##########################
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil tdr
soilTDR <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

# add co2 and remove unnecessary columns
soilTDR <- within(soilTDR, {
  co2 <- factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
  plot <- factor(plot)
  Temp_Min <- NULL
  Temp_Max <- NULL
  Sample <- NULL
})

soilTDRdf <- melt(soilTDR, id = c("Date", "co2", "ring", "plot"))
soilTDRdf$type <- factor(ifelse(soilTDRdf$variable == "Moist", "Moist", "Temp")) 
# need "type" column for ggplot later

# compute mean
soilTDR_RngMean <- ddply(soilTDRdf, .(Date, co2, ring, variable, type), summarise, value = mean(value, na.rm = TRUE))
soilTDR_co2Mean <- ddply(soilTDR_RngMean, .(Date, co2, variable, type), summarise, value = mean(value, na.rm = TRUE))

##############################################
## Plot raw data and incubation-period mean ##
##############################################

## co2 ##
pl <- PltSoilVar(data = mine, var = "co2", tdrData = soilTDR_co2Mean, linealpha = .5) +
  scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt), 
                     labels = c("Ambient", expression(eCO[2]))) +
  ggtitle("Mean soil moisture and temperature\nduring incubation")
ggsavePP(filename = "output//figs/FACE_Mineralisation_SoilVarMonth_CO2", plot = pl, width = 6, height = 4)

## ring ##
pl <- PltSoilVar(data = mine, var = "ring", tdrData = soilTDR_RngMean, linealpha = .3) +
  scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6), sep = "_")) +
  ggtitle("Mean soil moisture and temperature\nduring incubation")

ggsavePP(filename = "output//figs/FACE_Mineralisation_SoilVarMonth_Ring", plot = pl, width = 6, height = 4)

############################
# Plot Moist against  Temp #
############################

p <- ggplot(subsetD(mine, time != 1), aes(x = Temp_Mean, y = log(Moist), col = ring))
p2 <- p + geom_point(alpha = .5) 

pl  <- p2 + facet_wrap( ~ ring)
ggsavePP(file = "output/figs/FACE_Mineralisation_SoilVar_Ring", plot = pl, width = 6, height = 6)

ggsavePP(file = "output/figs/FACE_Mineralisation_SoilVar", plot = p2, width = 6, height = 6)