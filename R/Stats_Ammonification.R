## ----Stat_FACE_Mine_Ammonification

range(mine$ammonification)

bxplts(value= "ammonification", ofst= .16, data= mine)

# no transformation works bery well, so use raw data

# the inital model
Iml <- lmer(ammonification ~ co2 * time + (1|block) + (1|ring) + (1|id), 
            data = mine)

Anova(Iml)
# strong Time x CO2

# The final model
Fml <- Iml
Anova(Fml)
AnvF_Amm_time <- Anova(Fml, test.statistic = "F")

summary(Fml)

# model diagnosis
plot(Fml)
# little wedge-shaped
qqnorm(residuals(Fml))
qqline(residuals(Fml))
# not very great...

############
# Contrast #
############
# Note that contrast doesn't work with lmer so use lme
df <- within(mine, {
  co2 <- relevel(co2, "elev")
})

LmeMod <- lme(ammonification ~ co2 * time, random = ~1|block/ring/plot,
              data = df)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(mine$time), co2 = "amb"),
                  b = list(time = levels(mine$time), co2 = "elev"))
FACE_Mine_Amm_CntrstDf <- cntrstTbl(cntrst, data = mine, variable = "ammonification")

FACE_Mine_Amm_CntrstDf

## ----Stat_FACE_Mine_Ammonification_withSoilvar

##########
# ANCOVA #
##########
## plot all variables
scatterplotMatrix( ~ ammonification  + Moist + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")
scatterplotMatrix( ~ ammonification  + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")
# log moist seems slightly better

df2 <- within(postDF, {logMoist <- log(Moist)})


## Analysis
Iml_ancv1 <- lmer(ammonification ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)

Iml_ancv2 <- lmer(ammonification ~ co2 * (logMoist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = df2)
rsquared.glmm(list(Iml_ancv1, Iml_ancv2))
# log Moist is slightly better so use this

Iml_ancv <- Iml_ancv2
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)

AnvF_Amm <- Anova(Fml_ancv, test.statistic = "F")
AnvF_Amm

plot(Fml_ancv)
# wedged patter...

qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))
  # not very great...

##########################
## plot predicted value ##
##########################
# back transformation
par(mfrow = c(1,2))
l_ply(c("logMoist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv, variable = x, data = df2)
)

#############
## 95 % CI ##
#############
ciDF <- CIdf(model = Fml_ancv)

Est.val <- ciDF

# reshape Est.val and make a table
Est_Amm <- ANCV_Tbl(Est.val)


## ----Stat_FACE_Mine_AmmonificationSmmry
# The starting model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call

# Chi-squre
Anova(Fml)

# F test
AnvF_Amm_time

# Contrast
FACE_Mine_Amm_CntrstDf

## ----Stat_FACE_Mine_Ammonification_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call

# Chi-square
Anova(Fml_ancv)

# F test
AnvF_Amm

# squared R
rsquared.glmm(Fml_ancv)

# Plot predicted values
par(mfrow = c(1,2))
l_ply(c("logMoist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv, variable = x, data = df2)
)

# 95 % CI for estimates
Est.val
