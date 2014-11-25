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

##################
# Ammonification #
##################
source("R/Stats_Ammonification.R")

####################
# P mineralisation #
####################
source("R/Stats_P_mineralisation.R")

#######################
# Summary Stats table #
#######################

########################
## CO2 x Moist x Temp ##
########################

# create summary list for LMM with soil variable
StatSmmryLst <- list("Nitrification" = list(AnvF_Nit, Est_Nit),
                     "N_mineralisation" = list(AnvF_Nmin, Est_Nmin),
                     "P_mineralisation" = list(AnvF_P, Est_P))


# save in a single excel file
wb <- createWorkbook()
l_ply(c("Nitrification", "N_mineralisation", "P_mineralisation"), 
      function(x) CrSheetAnvTbl(workbook = wb, 
                                sheetName = x, 
                                smmaryLst = StatSmmryLst))
saveWorkbook(wb, "output//table/FACE_Minerlisation_Ancv.xlsx")

################
## CO2 x Time ##
################

# create stat summary table for LMM with CO2 and time
CO2TimeStatList <- list('Net nitrification rate' = AnvF_Nit_time, 
                        'Net ammonification rate' = AnvF_Amm_time, 
                        'Net N mineralisation rate' = AnvF_Nmin_time, 
                        'Net P mineralisation rate' = AnvF_Pmin_time)

Stat_CO2Time <- ldply(names(CO2TimeStatList), 
                      function(x) StatTable(CO2TimeStatList[[x]], variable = x))
save(Stat_CO2Time, file = "output//data/CO2Time_Stat.RData")

########################
## Result of contrast ##
########################
ContrastDF <- rbind.fill(list(
  FACE_Mine_Amm_CntrstDf,
  FACE_Mine_Nmin_CntrstDf,
  FACE_Mine_Nit_CntrstDf,
  FACE_Mine_P_CntrstDf
  ))
save(ContrastDF, file = "output//data/FACE_Mine_ContrastDF.RData")


