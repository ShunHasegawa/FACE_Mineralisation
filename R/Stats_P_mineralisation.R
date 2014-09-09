## ----Stat_FACE_Mine_P_min

range(mine$p.min)

bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1)
bxcxplts(value= "p.min", data= mine, sval = .0192, fval = .1, lambda = seq(-3, 2, 1/10))
bxplts(value= "p.min", ofst= .1, data= mine, lambda = seq(-3, -1, 1/10))
  # use box-cox lambda

# the inital model
Iml <- lmer((p.min + .1)^(-2) ~ co2 * time + (1|block) + (1|ring) + (1|id),
            data = mine)
Iml <- lmer(log(p.min + .1) ~ co2 * time + (1|block) + (1|ring) + (1|id),
            data = mine)
Anova(Iml)
# no huge difference in results between the above transformations so just use
# log for simplicity

# marginally significantl time x co2 so keep

# The final model is
Fml <- Iml
Anova(Fml)
AnvF_Pmin_time <- Anova(Fml, test.statistic = "F")
AnvF_Pmin_time

summary(Fml)

############
# Contrast #
############
# contrast doesn't work with lmer so use lme

# LmeMod <- lme(log(p.min + .1) ~ co2 * time, random = ~1|block/ring/plot, data 
# = mine) 
# The above model will get the error message: Error in testStatistic(fit, X,
# modelCoef, covMat, conf.int = conf.int) : Non-positive definite approximate
# variance-covariance so make new data frame with reordered factor levels

newDF <- within(mine,{
  co2 <- relevel(co2, "elev")
  time <- relevel(time, "2")
})

LmeMod <- lme(log(p.min + .1) ~ co2 * time, random = ~1|block/ring/plot, 
              data = newDF) 

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(NitRmOl$time), co2 = "amb"),
                  b = list(time = levels(NitRmOl$time), co2 = "elev"))

FACE_Mine_P_CntrstDf <- cntrstTbl(cntrst, data = NitRmOl, digit = 2)

FACE_Mine_P_CntrstDf


# model diagnosis
plot(Fml)
qqnorm(residuals(Fml))
qqline(residuals(Fml))

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
AnvF_P <- Anova(Fml_ancv, test.statistic = "F")
AnvF_P

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

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val)

## ----Stat_FACE_Mine_P_minSmmry
# The starting model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call

# Chi-squre
Anova(Fml)

# F test
AnvF_Pmin_time

# contrast
FACE_Mine_P_CntrstDf

## ----Stat_FACE_Mine_P_min_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call

# Chi-squre
Anova(Fml_ancv)

# F test
AnvF_P

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
