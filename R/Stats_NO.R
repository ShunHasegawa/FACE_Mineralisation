## ----Stat_FACE_Mine_Nitrification

range(mine$nitrification)

bxplts(value= "nitrification", ofst= .03, data= mine)

# remove higher outlier
bxplts(value= "nitrification", ofst= .03, data= subset(mine, nitrification < max(nitrification)))
  # no need for tranformation

NitRmOl <- subset(mine, nitrification < max(nitrification))

# different random factor strucures
m1 <- lme(nitrification ~ co2 * time, random = ~1|ring/plot, data = NitRmOl)
m2 <- lme(nitrification ~ co2 * time, random = ~1|ring, data = NitRmOl)
m3 <- lme(nitrification ~ co2 * time, random = ~1|id, data = NitRmOl)
anova(m1, m2, m3)
# m2 is better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
  # no need for correlation

Iml <- m2

# The starting model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
  # no factor was removed, but time:co2 is not 
  # significant so remove

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
