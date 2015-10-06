## ----Stat_FACE_Mine_Nitrification_preCO2
boxplot(nitrification ~ co2, data = subset(mine, time == 1))
Iml_pre_nit <- lmer(nitrification ~ co2 + (1|block) + (1|ring), 
                    data = mine, 
                    subset = time == 1)
Anova(Iml_pre_nit, test.statistic = "F")
summary(Iml_pre_nit)

## ----Stat_FACE_Mine_Nitrification_postCO2

range(postDF$nitrification)

bxplts(value= "nitrification", ofst= .03, data= postDF)

# remove higher outlier
bxplts(value= "nitrification", ofst= .03, data= subset(postDF, nitrification < max(nitrification)))

NitRmOl <- subset(postDF, nitrification < max(nitrification))

# the inital model
Iml_post_nit <- lmer(sqrt(nitrification + .03) ~ co2 * time + (1|block) + (1|ring) + (1|id), 
                     data = NitRmOl)

Anova(Iml_post_nit)
Anova(Iml_post_nit, test.statistic = "F")

# The final model
Fml_post_nit <- Iml_post_nit
Anova(Fml_post_nit)
AnvF_Nit_time <- Anova(Fml_post_nit, test.statistic = "F")
AnvF_Nit_time

summary(Fml_post_nit)

# model diagnosis
plot(Fml_post_nit)
qqnorm(residuals(Fml_post_nit))
qqline(residuals(Fml_post_nit))

############
# Contrast #
############
# contrast doesn't work with lmer so use lme
LmeMod <- lme(sqrt(nitrification + .03) ~ co2 * time, random = ~1|block/ring/plot, data = NitRmOl)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(NitRmOl$time), co2 = "amb"),
                  b = list(time = levels(NitRmOl$time), co2 = "elev"))
FACE_Mine_Nit_CntrstDf <- cntrstTbl(cntrst, data = NitRmOl, variable = "nitrification")

FACE_Mine_Nit_CntrstDf

## ----Stat_FACE_Mine_Nitrification_withSoilvar

##########
# ANCOVA #
##########
## plot all variables
scatterplotMatrix( ~ log(nitrification + .1)  + Moist + Temp_Max + Temp_Min + Temp_Mean,
                   data = postDF, diag = "boxplot")

## Analysis
Iml_ancv_nit <- lmer(log(nitrification + .1) ~ co2 * (Moist + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
Fml_ancv_nit <- stepLmer(Iml_ancv_nit, alpha.fixed = .1)
Anova(Fml_ancv_nit)
AnvF_Nit <- Anova(Fml_ancv_nit, test.statistic = "F")
AnvF_Nit

plot(Fml_ancv_nit)
qqnorm(resid(Fml_ancv_nit))
qqline(resid(Fml_ancv_nit))

##########################
## plot predicted value ##
##########################
# back transformation
Rtr <- function(x) exp(x + .1)

PltPrdVal(model = Fml_ancv_nit, variable = "Moist", 
          by = "co2",
          trans = Rtr,
          data = postDF)

#############
## 95 % CI ##
#############
ciDF <- CIdf(model = Fml_ancv_nit)

Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  co2elev.Moist = ciDF[4, ] + ciDF[3, 3]
)

Est.val

# reshape Est.val and make a table
Est_Nit <- ANCV_Tbl(Est.val)


## ----Stat_FACE_Mine_NitrificationSmmry_preCO2
Iml_pre_nit@call
Anova(Iml_pre_nit, test.statistic = "F")

## ----Stat_FACE_Mine_NitrificationSmmry_postCO2
# The starting model is:
Iml_post_nit@call
Anova(Iml_post_nit)

# The final model is:
Fml_post_nit@call

# Chi-squre
Anova(Fml_post_nit)

# F test
AnvF_Nit_time

# Contrast
FACE_Mine_Nit_CntrstDf

## ----Stat_FACE_Mine_Nitrification_withSoilvarSmmry
# The initial model:
Iml_ancv_nit@call
Anova(Iml_ancv_nit)

# The final model is:
Fml_ancv_nit@call

# Chi-squre
Anova(Fml_ancv_nit)

# F test
AnvF_Nit

# squared R
rsquared.glmm(Fml_ancv_nit)

# Plot predicted values
Rtr <- function(x) exp(x + .1)
PltPrdVal(model = Fml_ancv_nit, variable = "Moist", 
          by = "co2",
          trans = Rtr,
          data = postDF)

# 95 % CI for estimates
Est_Nit
