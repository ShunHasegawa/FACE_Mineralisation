## ----Stat_FACE_Mine_N_min

range(mine$n.min)

bxplts(value= "n.min", ofst= .5, data= mine)
  # inverse looks slightly better

# different random factor strucures
m1 <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|ring/plot, data = mine)
m2 <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|ring, data = mine)
m3 <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|id, data = mine)
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
# significant but marginal

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# model diagnosis
plot(Fml)
  # little wedge-shaped
qqnorm(Fml, ~ resid(.)|ring)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # not very great...

# Contrast
cntrst<- contrast(Fml, 
                  a = list(time = levels(mine$time), co2 = "amb"),
                  b = list(time = levels(mine$time), co2 = "elev"))
FACE_Mine_Nmin_CntrstDf <- cntrstTbl(cntrst, data = mine, digit = 2)

FACE_Mine_Nmin_CntrstDf

########################
# remove highest value #
########################
bxplts(value= "n.min", ofst= .128, data= subset(mine, n.min < max(n.min)))
bxplts(value= "n.min", ofst= .5, data= subset(mine, n.min < max(n.min)))
m1 <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|ring/plot, data =  subset(mine, n.min < max(n.min)))
plot(m1)
qqnorm(m1, ~ resid(.)|id)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))
# doesn't make difference


## ----Stat_FACE_Mine_N_minSmmry
# The starting model is:
Iml$call
xtable(Anova(Iml), floating = FALSE)

# The final model is:
Fml$call
xtable(Anova(Fml), floating = FALSE)

# Contrast
print(xtable(FACE_Mine_Nit_CntrstDf, floating = FALSE), 
      include.rownames = FALSE)
