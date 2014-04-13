rm(list=ls(all=TRUE))
#setwd("C:\\Users\\sh3410\\SkyDrive\\Documents\\PhD.HIE\\R\\Mineralisation\\Data")

#####
#library
#####
source("functions/list_library.R")
library(xlsx)
(.packages())
#####

source("functions/function.R")

result<-read.csv("mineralisation.kgbais.csv",colClasses=c("ring"="factor","plot"="factor","time"="factor"))
summary(result)
result<-result[complete.cases(result),]
result<-droplevels(result)

#reorder day factors
levels(result$day)
result$day <- factor(result$day, levels(result$day) [c(3,4,2,1)])

#remove coverage column
result <- result[,-5]
names(result)

# save
save(result, file = "output/data/mineralisation.R")

#summary table
####
summary(result)
contents(result)

#ring mean
names(result)
ring.mean<-with(result,aggregate(result[c("nitrification","n.min","p.min")],list(time=time,day=day,ring=ring,co2=co2),mean))
contents(ring.mean)
ring.mean
write.table(ring.mean,"mineralisation.ring.mean.(kg.soil_basis).csv")

#co2 mean
names(ring.mean)
treat.mean<-summaryBy(nitrification+n.min+p.min~time+day+co2,FUN=c(mean,function(x) ci(x)[4],length),data=ring.mean,
                      fun.names=c("mean","SE","Sample_size"))
treat.mean
write.table(treat.mean,"mineralisation.co2.mean&SE(kg.soil_basis).csv",sep=",")

#save summary tables as excel
source("functions/Mineralisation_summary_excel_table.R")


#re-structure for excel graph
#melt all response variables into one column
#ml<-melt(treat.mean,id.var=c("time","day","co2"))
#head(ml)
#cast
#tr.mean.cs<-cast(ml,time+day~co2+variable)
#head(tr.mean.cs)
#re-order columns
#vals<-names(tr.mean.cs)
#tr.mean.cs<-tr.mean.cs[,c(1,2,order(vals[3:length(vals)])+2)]
#tr.mean.cs
#save as csv
#write.csv(tr.mean.cs,"Mineralisation.CO2.mean(kg.soil_basis).excel.graph.table.csv")

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
