## ----Stat_FACE_Mine_N_min

range(mine$n.min)

bxplts(value= "n.min", ofst= .5, data= mine)
  # inverse looks slightly better

# the inital model
Iml <- lmer(1/(n.min + .5) ~ co2 * time + (1|block) + (1|ring) + (1|id),
            data = mine)

Anova(Iml)
# marginally significant interactive effect so keep it

# The final model
Fml <- Iml
Anova(Fml)
AnvF_Nmin_time <- Anova(Fml, test.statistic = "F")

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
LmeMod <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|block/ring/plot,
              data = mine)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(mine$time), co2 = "amb"),
                  b = list(time = levels(mine$time), co2 = "elev"))
FACE_Mine_Nmin_CntrstDf <- cntrstTbl(cntrst, data = mine, variable = "n.min")

FACE_Mine_Nmin_CntrstDf

## ----Stat_FACE_Mine_N_mineralisation_withSoilvar

##########
# ANCOVA #
##########
## plot all variables
scatterplotMatrix( ~ I(1/(n.min + .5))  + Moist + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")
scatterplotMatrix( ~ I(1/(n.min + .5))  + log(Moist) + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")

## Analysis
Iml_ancv <- lmer(I(1/(n.min + .5)) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
Anova(Iml_ancv)
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_Nmin <- Anova(Fml_ancv, test.statistic = "F")
AnvF_Nmin
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

##########################
## plot predicted value ##
##########################
# back transformation
Rtr <- function(x) 1/x - .5

par(mfrow = c(1,2))
l_ply(c("Moist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv, variable = x, trans = Rtr, data = postDF)
)

#############
## 95 % CI ##
#############
ciDF <- CIdf(model = Fml_ancv)

Est.val <- ciDF

# reshape Est.val and make a table
Est_Nmin <- ANCV_Tbl(Est.val)


## ----Stat_FACE_Mine_N_minSmmry
# The starting model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call

# Chi-squre
Anova(Fml)

# F test
AnvF_Nmin_time

# Contrast
FACE_Mine_Nmin_CntrstDf

## ----Stat_FACE_Mine_N_min_withSoilvarSmmry
# The initial model:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call

# Chi-square
Anova(Fml_ancv)

# F test
AnvF_Nmin

# squared R
rsquared.glmm(Fml_ancv)

# Plot predicted values
Rtr <- function(x) 1/x - .5
par(mfrow = c(1,2))
l_ply(c("Moist", "Temp_Mean"), function(x) 
  PltPrdVal(model = Fml_ancv, variable = x, trans = Rtr, data = postDF)
)

# 95 % CI for estimates
Est.val
