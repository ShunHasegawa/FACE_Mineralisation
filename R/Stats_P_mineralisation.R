## ----Stat_FACE_Mine_P_min

range(mine$p.min)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxplts(value= "p.min", ofst= .1, data= mine)
  # use inverse

# different random factor strucures
m1 <- lme(1/(p.min + .1) ~ co2 * time, random = ~1|ring/plot, data = mine)
m2 <- lme(1/(p.min + .1) ~ co2 * time, random = ~1|ring, data = mine)
m3 <- lme(1/(p.min + .1) ~ co2 * time, random = ~1|id, data = mine)
anova(m1, m2, m3)
# m2 is better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
# model 3 looks best

Iml <- atcr.cmpr(m2, rndmFac="ring")[[3]]

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


## ----Stat_FACE_Mine_P_minSmmry
# The starting model is:
Iml$call
xtable(Anova(Iml), floating = FALSE)

# The final model is:
Fml$call
xtable(Anova(Fml), floating = FALSE)