## ----Stat_FACE_Mine_N_min_preCO2
boxplot(n.min ~ co2, data = subset(mine, time == 1))
Iml_pre_nmin <- lmer(n.min ~ co2 + (1|block) + (1|ring), 
                     data = mine, 
                     subset = time == 1)
Anova(Iml_pre_nmin, test.statistic = "F")
summary(Iml_pre_nmin)

plot(Iml_pre_nmin)
qqnorm(residuals(Iml_pre_nmin))
qqline(residuals(Iml_pre_nmin))

## ----Stat_FACE_Mine_N_min_postCO2

range(postDF$n.min)

bxplts(value= "n.min", ofst= .5, data= postDF)
  # inverse looks slightly better

# the inital model
Iml_post_nmin <- lmer(log(n.min + .5) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                      data = postDF)

Anova(Iml_post_nmin)
Anova(Iml_post_nmin, test.statistic = "F")

# Model simplification
Fml_post_nmin <- stepLmer(Iml_post_nmin, alpha.fixed = .1)
Anova(Fml_post_nmin)
AnvF_Nmin_time <- Anova(Fml_post_nmin, test.statistic = "F")
AnvF_Nmin_time
summary(Fml_post_nmin)

# model diagnosis
plot(Fml_post_nmin)
qqnorm(residuals(Fml_post_nmin))
qqline(residuals(Fml_post_nmin))

############
# Contrast #
############
# Note that contrast doesn't work with lmer so use lme
LmeMod <- lme(log(n.min + .5) ~ co2 * time, random = ~1|block/ring/plot, 
              data = postDF)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(postDF$time), co2 = "amb"),
                  b = list(time = levels(postDF$time), co2 = "elev"))
FACE_Mine_Nmin_CntrstDf <- cntrstTbl(cntrst, data = postDF, variable = "n.min")

FACE_Mine_Nmin_CntrstDf

## ----Stat_FACE_Mine_N_mineralisation_withSoilvar

##########
# ANCOVA #
##########
## plot all variables
scatterplotMatrix( ~ log(n.min + .5)  + Moist + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")
scatterplotMatrix( ~ log(n.min + .5)  + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")

## Analysis
postDF$logMoist <- log(postDF$Moist)
Iml_ancv_nmin <- lmer(log(n.min + .5) ~ co2 * (logMoist + Temp_Mean) + 
                        (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_ancv_nmin)
Fml_ancv_nmin <- stepLmer(Iml_ancv_nmin, alpha.fixed = .1)
Anova(Fml_ancv_nmin)
AnvF_Nmin <- Anova(Fml_ancv_nmin, test.statistic = "F")
AnvF_Nmin
plot(Fml_ancv_nmin)
qqnorm(resid(Fml_ancv_nmin))
qqline(resid(Fml_ancv_nmin))

##########################
## plot predicted value ##
##########################
par(mfrow = c(1,2))
l_ply(c("logMoist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv_nmin, variable = x, data = postDF)
)

#############
## 95 % CI ##
#############
ciDF <- CIdf(model = Fml_ancv_nmin)

Est.val <- ciDF

# reshape Est.val and make a table
Est_Nmin <- ANCV_Tbl(Est.val)


## ----Stat_FACE_Mine_N_minSmmry_preCO2
Iml_pre_nmin@call
Anova(Iml_pre_nmin, test.statistic = "F")

## ----Stat_FACE_Mine_N_minSmmry_postCO2
# The starting model is:
Iml_post_nmin@call
Anova(Iml_post_nmin)

# The final model is:
Fml_post_nmin@call

# Chi-squre
Anova(Fml_post_nmin)

# F test
AnvF_Nmin_time

# Contrast
FACE_Mine_Nmin_CntrstDf

## ----Stat_FACE_Mine_N_min_withSoilvarSmmry
# The initial model:
Iml_ancv_nmin@call
Anova(Iml_ancv_nmin)

# The final model is:
Fml_ancv_nmin@call

# Chi-square
Anova(Fml_ancv_nmin)

# F test
AnvF_Nmin

# squared R
rsquared.glmm(Fml_ancv_nmin)

# Plot predicted values
par(mfrow = c(1,2))
l_ply(c("logMoist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv_nmin, variable = x, data = postDF)
)

# 95 % CI for estimates
Est_Nmin
