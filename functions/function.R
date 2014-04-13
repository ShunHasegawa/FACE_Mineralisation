# produce box plots with transformed data (log, sqrt, power(1/3))
bxplts <- function(value, ofst = 0,data){
  dev.off()
  par(mfrow = c(2,2))
  y <- data[[value]] + ofst #ofst is added to make y >0
  boxplot(y ~ co2*time, data)
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
}

#function for mmodel simplification
ana<-function(model){
  mod2<-update(model,method="ML") #change method from REML to ML
  stai<-stepAIC(mod2,trace=FALSE) #model simplification by AIC
  dr<-drop1(stai,test="Chisq") #test if removing a factor even more significantly lowers model
  model<-update(stai,method="REML")
  ifelse(all(dr[[4]]<0.05,na.rm=TRUE),anr<-anova(model),anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic=stai$anova,drop1=dr,anova.reml=anr,model.reml=model,model.ml=stai))
}

######################################################
# process and combine aq 2 data, then create a table #
######################################################
prcsAQ2 <- function(data){
  # remove ccv, ccb, standard
  res <- data[-grep("^C|^STANDARD", as.character(data$Sample.ID)),]
  
  # remove dup, top, middle
  res <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # sample labels
  a <- strsplit(as.character(res$Sample.Details), "[.]")
  
  # turn this into data frame
  a.df <- ldply(a)
  names(a.df)[c(1, 4:6)] <- c("Date", "incubation", "ring", "plot")
  a.df$Date <- dmy(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "incubation", "ring", "plot", "Result")]
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(x, header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[6] <- "value"
  pr.cst <- cast(pr.df, Date + ring + plot ~ Test.Name + incubation)
  return(pr.cst)
}
