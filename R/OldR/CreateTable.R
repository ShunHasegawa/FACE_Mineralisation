#function which creates summary table
CreateTable <- function(dataset,fac,nutrient){
  #dataset=result for ring summary, ring.mean for co2 summary, fac=ring or co2, nutrient=no3/nh4/po4
  a <- dataset[c("day",fac,nutrient)] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a,date~variable,mean,na.rm=TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- merge(means,ses,by="date",sort=FALSE) #merge datasets, sort is false because I don't want the order of its levels to change
  mer <- mer[,c(1,order(names(mer)[-1])+1)] #re-order columns
  mer <- merge(mer,samples,by="date",sort=FALSE)
  mer
  return(mer)
}
