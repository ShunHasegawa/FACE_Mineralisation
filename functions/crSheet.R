#function which creates excel worksheets
crSheet <- function(sheetname,dataset,fac,nutrient,unit){
  #create sheet
  sheet <- createSheet(wb,sheetName=sheetname)
  
  #add data to the sheet
  addDataFrame(CreateTable(dataset,fac,nutrient),sheet,showNA=TRUE,row.names=FALSE,startRow=2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname,unit)),sheet,startRow=1,row.names=FALSE,col.names=FALSE)
}