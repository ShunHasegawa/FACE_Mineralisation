## ----Stat_FACE_Mine_Nitrification

range(mine$nitrification)

bxplts(value= "nitrification", ofst= .03, data= mine)

# remove higher outlier
bxplts(value= "nitrification", ofst= .03, data= subset(mine, nitrification < max(nitrification)))
  # no need for tranformation

NitRmOl <- subset(mine, nitrification < max(nitrification))

# the inital model
Iml <- lmer(nitrification ~ co2 * time + (1|block) + (1|ring) + (1|id),
            data = NitRmOl)

Anova(Iml)
Anova(Iml, test.statistic = "F")
# interactive effect so keep it

# The final model
Fml <- Iml
Anova(Fml)
AnvF_Nit_time <- Anova(Fml, test.statistic = "F")
AnvF_Nit_time

summary(Fml)

# model diagnosis
plot(Fml)
# little wedge-shaped
qqnorm(residuals(Fml))
qqline(residuals(Fml))

############
# Contrast #
############
# contrast doesn't work with lmer so use lme
LmeMod <- lme(nitrification ~ co2 * time, random = ~1|block/ring/plot, data = NitRmOl)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(NitRmOl$time), co2 = "amb"),
                  b = list(time = levels(NitRmOl$time), co2 = "elev"))
FACE_Mine_Nit_CntrstDf <- cntrstTbl(cntrst, data = NitRmOl, variable = "nitrification")

FACE_Mine_Nit_CntrstDf

## ----Stat_FACE_Mine_Nitrification_withSoilvar

##########
# ANCOVA #
##########
## plot all variables
scatterplotMatrix( ~ log(nitrification + .1)  + Moist + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")

## Analysis
Iml_ancv <- lmer(log(nitrification + .1) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_Nit <- Anova(Fml_ancv, test.statistic = "F")
AnvF_Nit

plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

##########################
## plot predicted value ##
##########################
# back transformation
Rtr <- function(x) exp(x + .1)

PltPrdVal(model = Fml_ancv, variable = "Moist", 
          by = "co2",
          trans = Rtr,
          data = postDF)

#############
## 95 % CI ##
#############
ciDF <- CIdf(model = Fml_ancv)

Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  co2elev.Moist = ciDF[4, ] + ciDF[3, 3]
)

Est.val

# reshape Est.val and make a table
Est_Nit <- ANCV_Tbl(Est.val)


## ----Stat_FACE_Mine_NitrificationSmmry
# The starting model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call

# Chi-squre
Anova(Fml)

# F test
AnvF_Nit_time

# Contrast
FACE_Mine_Nit_CntrstDf

## ----Stat_FACE_Mine_Nitrification_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call

# Chi-squre
Anova(Fml_ancv)

# F test
AnvF_Nit

# squared R
rsquared.glmm(Fml_ancv)

# Plot predicted values
Rtr <- function(x) exp(x + .1)
PltPrdVal(model = Fml_ancv, variable = "Moist", 
          by = "co2",
          trans = Rtr,
          data = postDF)

# 95 % CI for estimates
Est.val
