## ----Stat_FACE_Mine_N_min

range(mine$n.min)

bxplts(value= "n.min", ofst= .5, data= mine)
  # inverse looks slightly better

# different random factor strucures
m1 <- lme(1/(n.min + .5) ~ co2 * time, random = ~1|block/ring/plot, data = mine)
RndmComp(m1)$anova
# m5 is better but just use m1 for time beting

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
# no need for correlation

Iml <- atml[[1]]

# The starting model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
# no factor was removed, but time:co2 is not 
# significant but marginal

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
  # not very great...

# Contrast
cntrst<- contrast(Fml, 
                  a = list(time = levels(mine$time), co2 = "amb"),
                  b = list(time = levels(mine$time), co2 = "elev"))
FACE_Mine_Nmin_CntrstDf <- cntrstTbl(cntrst, data = mine, digit = 2)

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
Anova(Fml_ancv, test.statistic = "F")
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



## ----Stat_FACE_Mine_N_minSmmry
# The starting model is:
Iml$call
xtable(Anova(Iml), floating = FALSE)

# The final model is:
Fml$call
xtable(Anova(Fml), floating = FALSE)

# Contrast
print(xtable(FACE_Mine_Nit_CntrstDf, floating = FALSE), 
      include.rownames = FALSE)
