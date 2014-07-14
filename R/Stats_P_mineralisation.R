## ----Stat_FACE_Mine_P_min

range(mine$p.min)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1, lambda = seq(-3, 2, 1/10))
bxplts(value= "p.min", ofst= .1, data= mine, lambda = seq(-3, -1, 1/10))
  # use box-cox lambda

# different random factor strucures
m1 <- lme((p.min + .1)^(-2) ~ co2 * time, random = ~1|block/ring/plot, data = mine)
RndmComp(m1)$anova
# m5 is better, but use m1 for time being

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
# model 5 looks best

Iml <- atml[[5]]

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

## ----Stat_FACE_Mine_P_min_withSoilvar

##########
# ANCOVA #
##########
# plot all variables
scatterplotMatrix(~ I((p.min + .1)^(-2)) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")
scatterplotMatrix(~ log(p.min + .1) + Moist + Temp_Max + Temp_Min + Temp_Mean,
                  data = postDF, diag = "boxplot")

print(xyplot(log(p.min + .1) ~ Moist | ring + plot, postDF, type = c("r", "p")))
print(xyplot(log(p.min + .1) ~ log(Moist) | ring + plot, postDF, type = c("r", "p")))

print(xyplot(log(p.min + .1) ~ Temp_Max | ring + plot, postDF, type = c("r", "p")))


# Iml_ancv <- lmer((p.min + .1)^(-2) ~ co2 * log(Moist) +
#                    (1|block/ring/plot), 
#                  data = subsetD(mine, time != 1))

Iml_ancv <- lmer(log(p.min + .1) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = postDF)
# Fml_ancv <- stepLmer(Iml_ancv)
# eror message cause random terms don't explain any variabtion

# model simplification by hand
Anova(Iml_ancv)
# no need for interaction
m2 <- lmer(log(p.min + .1) ~ co2 + Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|id), data = postDF)
anova(Iml_ancv, m2)
Anova(m2)

# remove co2
m3 <- lmer(log(p.min + .1) ~ Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|id), data = postDF)

anova(m2, m3)
Anova(m3)

# final model
Fml_ancv <- m3
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
  # not very good... Other transformations (log(Moist), (p.min + .1)^(-2)) were
  # teste. They improved the model but didn't sinificantly change the final
  # interpretation so just use the simple one.

######################################
# Plot predicted values from a model #
######################################
# back transformation
Rtr <- function(x) exp(x + .1)

par(mfrow = c(1,2))
l_ply(c("Moist", "Temp_Mean"), function(x){
  PltPrdVal(model = Fml_ancv, variable = x, 
            trans = Rtr,
            data = postDF)
})

########################
# Confidence intervals #
########################
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)

Est.val <- ciDF

## ----Stat_FACE_Mine_P_minSmmry
# The starting model is:
Iml$call
Anova(Iml)

# The final model is:
Fml$call
Anova(Fml)

## ----Stat_FACE_Mine_P_min_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# 95% CI
Est.val

# Plot predicted values
Rtr <- function(x) exp(x + .1)
par(mfrow = c(1,2))
l_ply(c("Moist", "Temp_Mean"), function(x){
  PltPrdVal(model = Fml_ancv, variable = x, 
            trans = Rtr,
            data = postDF)
})
