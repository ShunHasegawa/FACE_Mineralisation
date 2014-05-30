## ----Stat_FACE_Mine_P_min

range(mine$p.min)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxplts(value= "p.min", ofst= .1, data= mine)
  # use box-cox lambda

# different random factor strucures
Iml <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring/plot, data = mine)

# The starting model is:
Iml$call
Anova(Iml)

# contrast
mine$time <- relevel(mine$time, 4)
Iml <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|ring/plot, data = mine)
cntrst <- contrast(Iml, a = list(time = levels(mine$time),
                                 co2 = "amb"), b = list(time = levels(mine$time), co2 = "elev"))



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
Anova(Iml)

# The final model is:
Fml$call
Anova(Fml)
