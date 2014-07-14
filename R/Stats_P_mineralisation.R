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


Iml_ancv <- lmer((p.min + .1)^(-2) ~ co2 * log(Moist) +
                   (1|block/ring/plot), 
                 data = subsetD(mine, time != 1))

Iml_ancv <- lmer(log(p.min + .1) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|plot), data = postDF)
# Fml_ancv <- stepLmer(Iml_ancv)
# eror message cause random terms don't explain any variabtion

# model simplification by hand
Anova(Iml_ancv)
# no need for interaction
m2 <- lmer(log(p.min + .1) ~ co2 + Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|plot), data = postDF)
anova(Iml_ancv, m2)
Anova(m2)

# remove co2
m3 <- lmer(log(p.min + .1) ~ Moist + Temp_Mean 
           + (1|block) + (1|ring) + (1|plot), data = postDF)

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


########################
# plot predicted value #
########################

# reverse transormation
ReTrf <- function(x) x^(-1/2)-.1

PltPr <- function(){
  visreg(Fml_ancv, xvar = "Moist", 
         by = "co2", 
         trans = ReTrf, 
         overlay = TRUE, print.cond=TRUE,
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")))
  
  timePos <- seq(-0.02, 0.05, length.out = 6)
  times <- c(2:7)

  for (i in 1:6){
    lines(x = range(mine$Moist[mine$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(mine$Moist[mine$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty = 1, leg = "Moisture range", bty = "n")
}

PltPr()
## Plot predicted values for each block ##

# Create a data frame for explanatory
expDF <- with(mine, expand.grid(ring = unique(ring), 
                               plot = unique(plot),
                               Moist = seq(min(Moist), max(Moist), length.out= 100)))
expDF <- within(expDF, {
  block = recode(ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
  co2 = factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
})

# adjust moisture range for each block
boxplot(Moist ~ block, data = expDF)


########################################
# adjust moisture range for each block #
########################################
BlkminMoist <- function(variable, data){
  a <- range(subset(mine, time != 1 & block == variable)$Moist)
  df <- subset(data, block == variable & 
                 Moist <= a[2] & 
                 Moist >= a[1])
  return(df)
}
AdjexpDF <- ldply(list("A", "B", "C"), function(x) BlkminMoist(variable = x, data = expDF))
boxplot(Moist ~ block, data = AdjexpDF)

# predicted values
PredVal <- predict(Fml_ancv, re.form = ~ (1|block/ring/plot), newdata = AdjexpDF)
PredDF <- cbind(AdjexpDF, PredVal)

theme_set(theme_bw())

p <- ggplot(PredDF, aes(x = Moist, y = ReTrf(PredVal), col = co2))

PlPred <- p + geom_line() +
  geom_point(aes(x = Moist, y = p.min, col = co2), data = subsetD(mine, time != 1)) + 
  scale_color_manual("co2", values = c("blue", "red")) +
  facet_grid(.~block) +
  labs(y = "P_mineralisation")


p <- ggplot(PredDF, aes(x = log(Moist), y = PredVal, col = co2))
p + geom_line() +
  geom_point(aes(x = log(Moist), y = (p.min + .1)^(-2), col = co2), data = subsetD(mine, time != 1)) + 
  scale_color_manual("co2", values = c("blue", "red")) +
  facet_grid(.~block, scale = "free_x") + 
  labs(y = expression((p.min + .1)^(-2)))

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
PlPred
