theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(mineMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))
save(TrtMean, file = "output//data/FACE_Mineralisation_CO2Mean.RData")

#################################
# plot each nutrient separately #
#################################
vars <- c("N_mineralisation","Nitrification",  "Ammonification", "P_minerlisation")

RngFg <- dlply(RngMean, .(variable), PltRnghMean)
fls <- paste("output//figs/FACE_mineralisation_Ring_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltCO2Mean)
fls <- paste("output//figs/FACE_mineralisation_CO2_", vars, sep = "")
l_ply(1:4, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
plDF <- TrtMean
# change labels for variables
plDF$variable <- factor(plDF$variable, 
                        labels = paste("Net", c("N mineralisation", 
                                                "nitrification",
                                                "ammonification",
                                                "P mineralisation"),
                                       "rate"))

## Blank data frame ##
# Create dataframe to define y range for each plot. This data frame actually
# doesn't plot anything but enable one to define y range (or x range if one likes)

# N range
Nrng <- with(subsetD(plDF, variable != "P mineralisation"), 
             c(min(Mean - SE), max(Mean + SE)))

blankDF <- data.frame(date = plDF$date[1], 
                      variable = unique(plDF$variable),
                      Mean = c(rep(Nrng[1], 3), 0,
                               rep(Nrng[2], 3), 0),
                      co2 = "amb")

pl <- PltCO2Mean(plDF) + 
  geom_blank(aes(x = date, y = Mean), data = blankDF) +
  facet_wrap(~variable, ncol = 2, scal = "free_y")
  
ggsavePP(filename = "output//figs/FACE_Mineralisation_CO2Trt", plot = pl, 
         width = 6, height = 4)

########################
# Plot for publication #
########################
# theme
load("output/data/CO2Time_Stat.RData") 
  # Note if I need most updated summary, I need to run Stat.R first

p <- WBFig(data = plDF, 
           ylab = expression(Mineralisation~rate~(mg~kg^"-1"~d^"-1")),
           StatRes = Stat_CO2Time, 
           StatY = c(.01, .01, .01, -0.003))
ggsavePP(filename = "output//figs//FACE_Manuscript/FACE_Mineralisation", 
         plot = p, width = 6, height = 5)

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