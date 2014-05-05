rm(list=ls(all=TRUE))

#library
source("functions/list_library.R")
(.packages())


source("functions/function.R")

result<-read.csv("Data/mineralisation.kgbais.csv",colClasses=c("ring"="factor","plot"="factor","time"="factor", 
                                                               "coverage" = "NULL"))
result <- result[complete.cases(result), ]
result<-droplevels(result)
summary(result)

#reorder day factors
levels(result$day)
result$day <- factor(result$day, 
                     levels = c("Jul2012", "Oct2012", "Jan2013", "Apr2013", "Jul2013", "Oct2013", "Jan2014"))

# save
save(result, file = "output/data/mineralisation.Rdata")

#summary table
####
#ring mean
ring.mean <- ddply(result, .(time, day, ring, co2), summarise, 
                   nitrification = mean(nitrification, na.rm = TRUE),
                   n.min = mean(n.min, na.rm = TRUE),
                   p.min = mean(p.min, na.rm = TRUE))

#co2 mean
treat.mean<-summaryBy(nitrification + n.min + p.min ~ time + day + co2, 
                      FUN = c(mean, function(x) ci(x)[4], length),
                      data = ring.mean,
                      fun.names = c("mean", "SE", "Sample_size"))

#save summary tables as excel
source("functions/Mineralisation_summary_excel_table.R")


#analysis


#################
# nitrification #
#################
range(result$nitrification)

bxplts("nitrification", ofst= 0.03, data = result)

# sqrt looks better
# autocorrelation
model1 <- lme(sqrt(nitrification+0.03) ~ co2 * time,random=~1|ring/plot, data=result)
model2.2 <- update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3 <- update(model1,correlation=corARMA(q=2))
model2.4 <- update(model1,correlation=corAR1()) 
model2.5 <- update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model1 looks best

# model simplification
m1.smpl <- ana(model1)
m1.smpl$anova.reml

# co2 effect at each time
contrast(model1,
         a=list(time=levels(result$time),co2="amb"),
         b=list(time=levels(result$time),co2="elev")) 


####################
# N mineralisation #
####################
range(result$n.min)

bxplts("n.min", ofst= 0.2, data = result)

# log looks better
model1 <- lme(log(n.min+0.2) ~ co2 * time,random=~1|ring/plot, data=result)
model2.2 <- update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3 <- update(model1,correlation=corARMA(q=2))
model2.4 <- update(model1,correlation=corAR1()) 
model2.5 <- update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model1 looks better
# model simplification
m1.smpl <- ana(model1)
m1.smpl$anova.reml


####################
# p mineralisation #
####################
range(result$p.min)

bxplts("p.min", ofst= 0.02, data = result)
  # sqrt looks better

# autocorrelation
summary(result)

model1 <- lme(sqrt(p.min+0.02) ~ co2 * time,random=~1|ring/plot, data=result)
model2.2 <- update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3 <- update(model1,correlation=corARMA(q=2))
model2.4 <- update(model1,correlation=corAR1()) 
model2.5 <- update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model2.2 looks best but model gives a significant intercation of time:co2
m1 <- ana(model1)


# co2 effect at each time
contrast(model1,
         a=list(time=levels(result$time),co2="amb"),
         b=list(time=levels(result$time),co2="elev")) 

  # error --> relevel time. the model may not be appropriate
