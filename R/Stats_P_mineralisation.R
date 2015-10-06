## ----Stat_FACE_Mine_P_min_preCO2
range(subset(mine, time == 1)$p.min)
boxplot(p.min ~ co2 , data = subset(mine, time == 1))

Iml_pre_pmin <- lmer(p.min ~ co2 + (1|block) + (1|ring), data = mine, 
                     subset = time == 1)
Anova(Iml_pre_pmin, test.statistic = "F")
summary(Iml_pre_pmin)

plot(Iml_pre_pmin)
qqnorm(residuals(Iml_pre_pmin))
qqline(residuals(Iml_pre_pmin))

## ----Stat_FACE_Mine_P_min

range(postDF$p.min)

bxplts(value= "p.min", ofst= .02, data= mine)
# sqrt

# the inital model
Iml_post_pmin <- lmer(sqrt(p.min + .02) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                      data = postDF)
Anova(Iml_post_pmin)

# The final model is
Fml_post_pmin <- stepLmer(Iml_post_pmin, alpha.fixed = .1)
Anova(Fml_post_pmin)
AnvF_Pmin_time <- Anova(Fml_post_pmin, test.statistic = "F")
AnvF_Pmin_time

summary(Fml_post_pmin)

# model diagnosis
plot(Fml_post_pmin)
qqnorm(residuals(Fml_post_pmin))
qqline(residuals(Fml_post_pmin))

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

Iml_ancv_pmin <- lmer(log(p.min + .1) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = postDF)
# Fml_ancv <- stepLmer(Iml_ancv)
# eror message cause random terms don't explain any variabtion

# model simplification by hand
Anova(Iml_ancv_pmin)
# no need for interaction
m2 <- lmer(log(p.min + .1) ~ co2 + Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|id), data = postDF)
anova(Iml_ancv_pmin, m2)
Anova(m2)

# remove co2
m3 <- lmer(log(p.min + .1) ~ Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|id), data = postDF)

anova(m2, m3)
Anova(m3)

# final model
Fml_ancv_pmin <- m3
Anova(Fml_ancv_pmin)
AnvF_P <- Anova(Fml_ancv_pmin, test.statistic = "F")
AnvF_P

# main effects
plot(allEffects(Fml_ancv_pmin))

# model diagnosis
plot(Fml_ancv_pmin)
qqnorm(resid(Fml_ancv_pmin))
qqline(resid(Fml_ancv_pmin))
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
  PltPrdVal(model = Fml_ancv_pmin, variable = x, 
#             trans = Rtr,
            data = postDF)
})

########################
# Confidence intervals #
########################
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_pmin)

Est.val <- ciDF

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val)

## ----Stat_FACE_Mine_P_minSmmry_preCO2
Iml_pre_pmin@call
Anova(Iml_pre_pmin, test.statistic = "F")

## ----Stat_FACE_Mine_P_minSmmry_postCO2
# The starting model is:
Iml_post_pmin@call
Anova(Iml_post_pmin)

# The final model is:
Fml_post_pmin@call

# Chi-squre
Anova(Fml_post_pmin)

# F test
AnvF_Pmin_time

## ----Stat_FACE_Mine_P_min_withSoilvarSmmry
# The initial model:
Iml_ancv_pmin@call
Anova(Iml_ancv_pmin)

# The final model is:
Fml_ancv_pmin@call

# Chi-squre
Anova(Fml_ancv_pmin)

# F test
AnvF_P

# squared R
rsquared.glmm(Fml_ancv_pmin)

# 95% CI
Est_P

# Plot predicted values
Rtr <- function(x) exp(x + .1)
par(mfrow = c(1,2))
l_ply(c("Moist", "Temp_Mean"), function(x){
  PltPrdVal(model = Fml_ancv_pmin, variable = x, 
#             trans = Rtr,
            data = postDF)
})
