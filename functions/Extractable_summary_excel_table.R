#summary table for excel
head(result)

#function which creates summary table
source("functions/CreateTable.R")

#function which creates excel worksheets
source("functions/crSheet.R")

#create xcel workbook
wb <- createWorkbook()

#worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(result,sheet,showNA=TRUE,row.names=FALSE)

#crate excel workwheet for each varable
#worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate","Ammonium","Phosphate"),sep="")
nut <- c("no3","nh4","po4")

for (i in 1:3){
  crSheet(sheetname=shnames[i],dataset=result,fac="ring",nutrient=nut[i],unit="unit=mg kg^(-1)")
}

#worksheets for co2 summary
shnames <- paste("CO2_mean.",c("Nitrate","Ammonium","Phosphate"),sep="")
for (i in 1:3){
  crSheet(sheetname=shnames[i],dataset=ring.mean,fac="co2",nutrient=nut[i],unit="unit=mg kg^(-1)")
}

#save file
saveWorkbook(wb,"table/FACE_Extractable.Nutrients.xlsx")
