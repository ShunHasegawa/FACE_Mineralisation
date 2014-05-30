## ----Stat_FACE_Mine_P_min

range(mine$p.min)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxplts(value= "p.min", ofst= .1, data= mine)
  # use box-cox lambda

#poly time
contrasts(mine$time) <- contr.poly(7, c(1:7))
mine$time <- ordered(mine$time)


# different random factor strucures
m1 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring/plot, data = mine)
m2 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring, data = mine)
m3 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|id, data = mine)
anova(m1, m2, m3)
# m2 is better

# autocorelation
atml <- atcr.cmpr(m2, rndmFac="ring")
atml$models
# model 3 looks best

Iml <- atml[[3]]

# The starting model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
  # time:co2 and co2 are removed

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

# withought auto-correlation
Anova(m2)
qqnorm(m2, ~ resid(.)|ring)
qqnorm(residuals.lm(m2))
qqline(residuals.lm(m2))

## ----Stat_FACE_Mine_P_minSmmry
# The starting model is:
Iml$call
Anova(Iml)

# The final model is:
Fml$call
Anova(Fml)
