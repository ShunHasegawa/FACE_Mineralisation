rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")
(.packages())


# function
source("functions/function.R")

# model simplification
source("functions/model_simplification.R")

result <- read.csv("Data//extractable.csv", header=T, colClasses=c("ring"="factor","plot"="factor","time"="factor", 
                                                                   "coverage" = "NULL"))
result <- result[complete.cases(result), ]
result <- droplevels(result)
summary(result)

#format date
result$day <- as.Date(dmy(as.character(result$day)))

# save
save(result, file = "output/data/extractable.Rdata")


# summary table
# ring mean
ring.mean<-with(result,aggregate(result[,6:8],list(time=time,day=day,ring=ring,co2=co2),mean))

write.table(ring.mean,"extractable.ring.mean.csv",sep=",")

#co2 mean
treat.mean<-summaryBy(no3+nh4+po4~time+day+co2,FUN=c(mean,function(x) ci(x)[4],length),data=ring.mean,
                      fun.names=c("mean","SE","Sample_size"))
treat.mean
write.table(treat.mean,"extractable.co2.mean&SE.csv",sep=",")

#save summary table as excel
source("functions/Extractable_summary_excel_table.R")


##re-structure for excel graph

#melt all response variables into one column
#ml<-melt(treat.mean,id.var=c("time","day","co2"))
#cast
#head(ml)
#tr.mean.cs<-cast(ml,time+day~variable+co2)
#head(tr.mean.cs)
#save as csv
#write.csv(tr.mean.cs,"Extractable.CO2.mean.excel.graph.table.csv")

###########
# Nitrate #
###########

par(mfrow=c(2,2))
boxplot(log(no3)~co2*time, data = result)
boxplot(no3~co2*time, data = result)
boxplot(sqrt(no3)~co2*time, data = result)
boxplot(no3^(1/3)~co2*time, data = result)

model1 <- lme(log(no3) ~ time*co2, random =~ 1|ring/plot, data = result)
model2.2 <- update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3 <- update(model1,correlation=corARMA(q=2))
model2.4 <- update(model1,correlation=corAR1()) 
model2.5 <- update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model1 looks best
m1 <- ana(model1)
m2 <- update(m1$model.reml, ~.- time:co2)
m3 <- ana(m2)
fnl.ml <- m3$model.reml
anova(fnl.ml)

## pre and post co2 separately ##
# pre #

# pre = time = 1, 2
model1 <- lme(log(no3)~time*co2,random=~1|ring/plot,subset= time %in% c(1,2), data=result)
model2.2 <- update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3 <- update(model1,correlation=corARMA(q=2))
model2.4 <- update(model1,correlation=corAR1()) 
model2.5 <- update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model1 is best
# model simplification
m1 <- ana(model1)
m1
m2 <- update(m1$model.reml, ~. -time:co2)
m3 <- ana(m2)
fin.ml <- m3$model.reml
anova(fin.ml)

#mod4 is best
anova(mod4)
mod5<-update(mod4,method="REML")
summary(mod5)
anova(mod5)

## post ##
# note sep 2012 data was included as a baseline
model1<-lme(log(no3)~time*co2,random=~1|ring/plot,subset= time %in% c(2:5), data = result)

model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model1 is best
m1 <- ana(model1)
m1$model.reml
m1$anova.reml

fnl.model <- lme(log(no3)~time,random=~1|ring/plot,subset= time %in% c(2:5), data = result)
anova(fnl.model)

############
# Ammonium #
############
bxplts("nh4", result)

model1<-lme(nh4~time*co2,random=~1|ring/plot, data = result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model1 looks best
m1 <- ana(model1)

##########################
#pre and post separately #
##########################

## pre ##
model1<-lme(nh4~time*co2,random=~1|ring/plot,subset=time %in% c(1,2), data=result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model1 looks best
m1 <- ana(model1)
m1$anova.reml

## post ##
model1<-lme(nh4~time*co2,random=~1|ring/plot,subset=time %in% c(2:5), data=result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model2.5 looks best, however model1 gives significant co2xtime interaction
m1 <- ana(model1)
m1$anova.reml

# contrast
cntrst<- contrast(model1,
                  a=list(time=levels(result$time)[2:5],co2="amb"),
                  b=list(time=levels(result$time)[2:5],co2="elev"))

d <- unique(result$day)[2:5]
ds <- format(d, format = "%b-%Y")
post.nh.cntrst.df <- data.frame(
  date = ds,
  contrast  =  cntrst$Contrast,
  SE = cntrst$SE,
  t = cntrst$testStat,
  df = cntrst$df,
  P.value = cntrst$Pvalue)

post.nh.cntrst.df


#############
# Phosphate #
#############
bxplts("po4", result)

model1<-lme(log(po4)~co2*time,random=~1|ring/plot,data=result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)
#model1 looks best
mod2<-update(model1,method="ML")
mod3<-update(mod2,~.-co2:time)
anova(mod2,mod3)
lists<-list(mod2,mod3)
sapply(lists,AICc)
#mod2 is bettter
anova(mod2,type="marginal")
summary(model1)
library(contrast)
contrast(model1,
         a=list(time=levels(result$time),co2="amb"),
         b=list(time=levels(result$time),co2="elev"))

nfac<-interaction(result$co2,result$time)
nfac
length(ring)
length(po4)
length(nfac)
TukeyHSD(aov(mo.lmr))
mo.lmr<-lmer(log(po4)~nfac+(1|ring),data=result)
summary(glht(mo.lmr,linfc=mcp(nfac="Tukey")))

args <- list("Tukey")
names(args)<-"nfac"
cmp <- do.call(mcp, args)
glht(mo.lmr, linfct = cmp)

###############################
# pre and post co2 separately #
###############################

## pre ##
model1<-lme(log(po4)~co2*time,random=~1|ring/plot,subset= time %in% c(1,2), data=result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

#model1 looks best
m1 <- ana(model1)
m1$anova.reml

#contrast for each month
contrast(model1,
         a=list(time=levels(result$time)[1:2],co2="amb"),
         b=list(time=levels(result$time)[1:2],co2="elev"))


## post-co2 ##
model1<-lme(log(po4)~co2*time,random=~1|ring/plot,subset = time %in% c(2:5),data=result)
model2.2<-update(model1,corr=corCompSymm(form=~1|ring/plot))
model2.3<-update(model1,correlation=corARMA(q=2))
model2.4<-update(model1,correlation=corAR1()) 
model2.5<-update(model1,correlation=corARMA(q=1))
anova(model1,model2.2,model2.3,model2.4,model2.5)

# model1 looks best
m1 <- ana(model1)

# contrast
contrast(model1,
         a=list(time=levels(result$time)[2:5],co2="amb"),
         b=list(time=levels(result$time)[2:5],co2="elev"))




#########contrast pre vs post co2
ntime<-result$time
contrasts(ntime) <-cbind(c(3,3,-2,-2,-2))
contrasts(ntime)
model1<-lme(log(po4)~ntime*co2,random=~1|ring/plot,data=result)
summary(model1)
anova(model1,type="marginal")
mod2<-update(model1,method="ML")
mod3<-update(mod2,~.-ntime:co2)
anova(mod2,mod3)
##mod2 = model1
anova(model1)
ntime
contrast(model1,
         a=list(ntime=levels(ntime),co2="amb"),
         b=list(ntime=levels(ntime),co2="elev"))




###
contrasts(iem2$time)<-NULL
options(contrasts=c("contr.treatment","contr.poly"))



##combine post co2 data
time2<-result$time
levels(time2)[c(3:5)]<-c("post","post","post")
levels(time2)
model1.1<-lme(log(po4)~co2*time2,random=~1|ring/plot,data=result,method="ML")
anova(mod2,model1.1) #no significant difference --> model1.1
anova(model1.1)
mod3<-update(model1.1,~.-co2:time2)
anova(model1.1,mod3)
mod4<-update(model1.1,method="REML")
summary(mod4)
contrast(mod4,
         a=list(time2=levels(time2),co2="amb"),
         b=list(time2=levels(time2),co2="elev"))


model1.2<-lme(log(po4)~co2*time,random=~1|ring/plot,subset=time!="1",data=result,method="ML")
model1.1<-lme(log(po4)~co2*time2,random=~1|ring/plot,subset=time2!="1",data=result,method="ML")
anova(model1.1,model1.2)

anova(model1.2)
summary(model1.2)


time3<-result$time
levels(time3)[c(1,2)]<-"pre"
model1.1<-lme(log(po4)~co2*time3,random=~1|ring/plot,data=result,method="ML")
anova(mod2,model1.1) #no significant difference --> model1.1




TukeyHSD(aov(model1))

is.factor(newf)

library(multcomp)
newf<-interaction(result$time,result$co2)
newf
mod.lmr<-lmer(log(po4)~newf+(1|ring/plot),data=result)
summary(mod.lmr)
summary(glht(mod.lmr, linfct=mcp(newf="Tukey")))## don't know why but it doesn't work
glht(mod.lmr, linfct=mcp(newf="Tukey")) 







summary(model1)
contrasts(time) <-cbind(c(1,1,-2))
model1<-lme(log(po4)~time*co2,random=~1|ring/plot,method="ML")
summary(model1)
model2 <-update(model1,~.-time:co2)
model3 <-update(model2,~.-co2)
anova(model1,model2,model3)





########################

#################
boxplot(po4~coverage:ring)
boxplot(po4~day:ring)
boxplot(log(po4)~coverage)
model1<-lme(po4~ring*coverage*day,random=~1|plot2,method="ML")
model2<-update(model1,~.-ring:coverage:day)
anova(model1,model2)
model3<-update(model2,~.-ring:coverage)
anova(model2,model3)
model4<-update(model3,~.-ring:day)
anova(model3,model4)
AICc(model3)
AICc(model4)
model5<-update(model3,~.-coverage:day)
anova(model3,model5)
model6<-update(model5,~.-coverage)
anova(model5,model6)
summary(model6)
model9<-lme(po4~ring+day+ring:day,random=~1|plot2)
summary(model9)
summary(aov(po4~ring+day+ring:day+Error(plot2)))

##################################graph
library(gtools)
library(gdata)
library(gmodels)
jpeg(file="extractable.time5.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,0.5,0.5), oma=c(0,0,0,0))
xv<-as.numeric(time)
#####################################no3
  means<-t(tapply(no3,list(ring,xv),mean))
  ses<-t(tapply(no3,list(ring,xv),function(x) ci(x)[4]))
  xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),
  beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray90","gray60","gray30"))
  arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
  axis(2,las=1,cex.axis=2)
  legend("topleft",leg=c("JUN","SEP","DEC","Mar'13","Jun'13"),fill=c(0,1,"gray90","gray60","gray30"),
         col=c(0,1,"gray90","gray60","gray30"),bty="n",cex=2)
  mtext(2,text=expression(mg~N-NO[3]^"-"~kg^-1~dry~soil),line=4.5,cex=1.5)
  box(bty="o")
  ####################CO2 scatter plot
  no.means<-aggregate(no3,list(ring,time,co2),mean)
  colnames(no.means)<-c("ring","time","co2","no3")
  no.means
  means<-with(no.means,tapply(no3,list(co2,time),mean))
  ses<-with(no.means,tapply(no3,list(co2,time),function(x) ci(x)[4]))
  means
  ses
  new.time<-as.numeric(time)
  xval<-unique(new.time)
  m.elev<-means["elev",]
  se.elev<-ses["elev",]
  m.amb<-means["amb",]
  se.amb<-ses["amb",]
  plot(no3~new.time,type="n",
       axe=F,xlab="",ylab="",xlim=c(1,5),ylim=c(0,10))
  arrows(xval,m.elev-se.elev,xval,m.elev+se.elev,code=3,angle=90,len=0.05) 
  points(m.elev~xval,pch=16,col="white",type="b",lwd=1,lty=2,cex=2.5)
  points(m.elev~xval,pch=1,col="black",type="b",lwd=1,lty=2,cex=2.5)
  arrows(xval,m.amb-se.amb,xval,m.amb+se.amb,code=3,angle=90,len=0.05) 
  points(m.amb~xval,pch=19,type="b",lwd=1,lty=1,cex=2.5)
  axis(2,cex.axis=2,las=1)
  box(bty="o")
  legend("topright",leg=c(expression(eCO[2]),"ambient"),lty=0,pch=c(1,19),bty="n",cex=2)
  lines(c(2.2,2.2),c(0,8),lty=2)
  text(2.2,9,expression(CO[2]~on),cex=2)
  #######################################
  #######################################nh4
  means<-t(tapply(nh4,list(ring,xv),mean))
  ses<-t(tapply(nh4,list(ring,xv),function(x) ci(x)[4]))
  xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),
  beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray90","gray60","gray30"))
  arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
  axis(2,las=1,cex.axis=2)
  mtext(2,text=expression(mg~N-NH[4]^"+"~kg^-1~dry~soil),line=4.5,cex=1.5)
  box(bty="o")
  #######################co2 scatter plot
  nh.means<-aggregate(nh4,list(ring,time,co2),mean)
  colnames(nh.means)<-c("ring","time","co2","nh4")
  nh.means
  means<-with(nh.means,tapply(nh4,list(co2,time),mean))
  ses<-with(nh.means,tapply(nh4,list(co2,time),function(x) ci(x)[4]))
  new.time<-as.numeric(time)
  xval<-unique(new.time)
  m.elev<-means["elev",]
  se.elev<-ses["elev",]
  m.amb<-means["amb",]
  se.amb<-ses["amb",]
  plot(nh4~new.time,type="n",
       axe=F,xlab="",ylab="",xlim=c(1,5),ylim=c(0,20))
  arrows(xval,m.elev-se.elev,xval,m.elev+se.elev,code=3,angle=90,len=0.05) 
  points(m.elev~xval,pch=16,col="white",type="b",lwd=1,lty=2,cex=2.5)
  points(m.elev~xval,pch=1,col="black",type="b",lwd=1,lty=2,cex=2.5)
  arrows(xval,m.amb-se.amb,xval,m.amb+se.amb,code=3,angle=90,len=0.05) 
  points(m.amb~xval,pch=19,type="b",lwd=1,lty=1,cex=2.5)
  axis(2,cex.axis=2,las=1)
  lines(c(2.2,2.2),c(0,20),lty=2)
  box(bty="o")
  ########################################
  ########################################P
  means<-t(tapply(po4,list(ring,xv),mean))
  ses<-t(tapply(po4,list(ring,xv),function(x) ci(x)[4]))
  xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),
  beside=T,axes=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray90","gray60","gray30"))
  arrows(xs,means-ses,xs,means+ses,code=3,angle=90,len=0.05)
  axis(2,las=1,cex.axis=2)
  mtext(2,text=expression(mg~P-PO[4]^"3-"~kg^-1~dry~soil),line=4.5,cex=1.5)
  axis(1,at=apply(xs,2,median),lab=c("1","2","3","4","5","6"),padj=1,mgp=c(0,0,0),
  cex.axis=2)
  box(bty="o")
  mtext(1,text="Ring",line=5,cex=1.5)
  #############co2 scatter plot
  attach(result)
  p.means<-aggregate(po4,list(ring,time,co2),mean)
  colnames(p.means)<-c("ring","time","co2","po4")
  p.means    
  means<-with(p.means,tapply(po4,list(co2,time),mean))
  means
  ses<-with(p.means,tapply(po4,list(co2,time),function(x) ci(x)[4]))
  new.time<-as.numeric(time)
  xval<-unique(new.time)
  m.elev<-means["elev",]
  se.elev<-ses["elev",]
  m.amb<-means["amb",]
  se.amb<-ses["amb",]
  plot(po4~new.time,type="n",axe=F,xlab="",ylab="",xlim=c(1,5),ylim=c(0,3.5))
  arrows(xval,m.elev-se.elev,xval,m.elev+se.elev,code=3,angle=90,len=0.05) 
  points(m.elev~xval,pch=16,col="white",type="b",lwd=1,lty=2,cex=2.5)
  points(m.elev~xval,pch=1,col="black",type="b",lwd=1,lty=2,cex=2.5)
  arrows(xval,m.amb-se.amb,xval,m.amb+se.amb,code=3,angle=90,len=0.05) 
  points(m.amb~xval,pch=19,type="b",lwd=1,lty=1,cex=2)
  axis(1,cex.axis=1,at=xval,lab=c("Jun","Sep","Dec","Mar'13","Jun'13"),cex.axis=2)  
  axis(2,cex.axis=2,las=1)
  box(bty="o")
  mtext(1,text="Month",line=4,cex=2)
  lines(c(2.2,2.2),c(0,8),lty=2)
dev.off()

########################################
########################################
####################################no3
means<-tapply(no3,list(ring,xv),mean)
ses<-tapply(no3,list(ring,xv),function(x) ci(x)[4])
plot(no3~xv,type="n",axes=F,xlab="",ylab="")
points(means["1",],pch="1",type="b",lwd=1,cex=1)
points(means["2",],pch="2",type="b",lwd=1,cex=1)
points(means["3",],pch="3",type="b",lwd=1,cex=1)
points(means["4",],pch="4",type="b",lwd=1,cex=1)
points(means["5",],pch="5",type="b",lwd=1,cex=1)
points(means["6",],pch="6",type="b",lwd=1,cex=1)
xv2<-c(1,2)
arrows(xv2,means["1",]-ses["1",],xv2,means["1",]+ses["1",],code=3,angle=90,len=0.05)
arrows(xv2,means["2",]-ses["2",],xv2,means["2",]+ses["2",],code=3,angle=90,len=0.05)
arrows(xv2,means["3",]-ses["3",],xv2,means["3",]+ses["3",],code=3,angle=90,len=0.05)
arrows(xv2,means["4",]-ses["4",],xv2,means["4",]+ses["4",],code=3,angle=90,len=0.05)
arrows(xv2,means["5",]-ses["5",],xv2,means["5",]+ses["5",],code=3,angle=90,len=0.05)
arrows(xv2,means["6",]-ses["6",],xv2,means["6",]+ses["6",],code=3,angle=90,len=0.05)
#######################################



