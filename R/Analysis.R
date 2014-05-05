rm(list=ls(all=TRUE))



source("functions/function.R")

result<-read.csv("Data/mineralisation.kgbais.csv",colClasses=c("ring"="factor","plot"="factor","time"="factor", 
                                                               "coverage" = "NULL"))
