## ----Stat_FACE_Mine_P_min

range(mine$p.min)

# turn time factor to ordered orthogonal plynomials
mine$timePly <- poly(as.numeric(mine$time), degree = 6)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxplts(value= "p.min", ofst= .1, data= mine)
  # use box-cox lambda

# different random factor strucures
m1 <- lme((p.min + .1)^(-2) ~ co2 * timePly, random = ~1|ring/plot, data = mine, method = "ML")
m2 <- lme((p.min + .1)^(-2) ~ co2 * timePly, random = ~1|ring, data = mine, method = "ML")
m3 <- lme((p.min + .1)^(-2) ~ co2 * timePly, random = ~1|id, data = mine, method = "ML")
anova(m1, m2, m3)
# m2 is better


# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
# model 3 looks best

Iml <- atcr.cmpr(m2, rndmFac="ring")[[3]]

# The starting model is:
Iml$call
Anova(Iml)
summary(Iml)


mine$timePly3 <- poly(as.numeric(mine$time), degree = 3)
m4 <- lme((p.min + .1)^(-2) ~ co2 * timePly3, random = ~1|ring/plot, data = mine)
m5 <- lme((p.min + .1)^(-2) ~ co2 * timePly3, random = ~1|ring, data = mine)
m6 <- lme((p.min + .1)^(-2) ~ co2 * timePly3, random = ~1|id, data = mine)
anova(m4, m5, m6)
Anova(m6)

atml <- atcr.cmpr(m6, rndmFac= "id")
atml$models
Iml <- atml[[3]]
Anova(Iml)
summary(Iml)
plot(Iml)

# model simplification
MdlSmpl(Iml)
  # time:co2 and co2 are removed

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)


library(effects)
mainEffects(Fml, at = co2 == "elev")
??mainEffects

# model diagnosis
plot(Fml)
# little wedge-shaped
qqnorm(Fml, ~ resid(.)|ring)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))


# different rondom factrs by polynomial time
timeL <- C(mine$time, poly, 1)
timeQ <- C(mine$time, poly, 2)
timeC <- C(mine$time, poly, 3)

mine$time <- ordered(mine$time)

m4 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring/plot, data = mine)
m5 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring, data = mine)
m6 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|id, data = mine)
summary(m4)
anova(m4, m5, m6)
Anova(m5)
levels(mine$time)

atml <- atcr.cmpr(m5, rndmFac= "ring")
atml$models
Anova(atml[[3]])

## ----Stat_FACE_Mine_P_minSmmry
# The starting model is:
Iml$call
xtable(Anova(Iml), floating = FALSE)

# The final model is:
Fml$call
xtable(Anova(Fml), floating = FALSE)
