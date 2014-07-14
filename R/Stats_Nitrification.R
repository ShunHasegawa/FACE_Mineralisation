## ----Stat_FACE_Mine_Nitrification

range(mine$nitrification)

bxplts(value= "nitrification", ofst= .03, data= mine)

# remove higher outlier
bxplts(value= "nitrification", ofst= .03, data= subset(mine, nitrification < max(nitrification)))
  # no need for tranformation

NitRmOl <- subset(mine, nitrification < max(nitrification))

# different random factor strucures
m1 <- lme(nitrification ~ co2 * time, random = ~1|block/ring/plot, data = NitRmOl)
RndmComp(m1)$anova
# m5 is better but just use m1 this time

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
# no need for autocorrelation

Iml <- atml[[1]]

# The starting model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
  # no factor was removed

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

# Contrast
cntrst<- contrast(Fml, 
                  a = list(time = levels(NitRmOl$time), co2 = "amb"),
                  b = list(time = levels(NitRmOl$time), co2 = "elev"))
FACE_Mine_Nit_CntrstDf <- cntrstTbl(cntrst, data = NitRmOl, digit = 2)

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
Anova(Fml_ancv, test.statistic = "F")
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

## ----Stat_FACE_Mine_NitrificationSmmry
# The starting model is:
Iml$call
xtable(Anova(Iml), floating = FALSE)

# The final model is:
Fml$call
xtable(Anova(Fml), floating = FALSE)

# Contrast
print(xtable(FACE_Mine_Nit_CntrstDf, floating = FALSE), 
      include.rownames = FALSE)

## ----Stat_FACE_Mine_Nitrification_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")
Est.val

# Plot predicted values
PltPrdVal(model = Fml_ancv, variable = "Moist", 
          by = "co2",
          trans = Rtr,
          data = postDF)

# 95 % CI for estimates
Est.val
