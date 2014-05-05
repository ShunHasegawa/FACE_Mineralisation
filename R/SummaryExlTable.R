# melt dataset
mineMlt <- melt(mine, id = names(mine)[which(!(names(mine) %in% c("nitrification", "n.min", "p.min")))])

# Ring summary table & mean
RngSmmryTbl <- dlply(mineMlt, .(variable), function(x) CreateTable(x, fac = "ring", digit = 3, nsmall = 3))
RngMean <- ddply(mineMlt, .(time, date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 3, nsmall = 3, scientific = FALSE))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(mine, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
vars <- c("Nitrification", "N_mineralisation", "P_mineralisation")
shnames <- paste("Ring_mean.",vars ,sep="")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("CO2_mean.", vars, sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"output/table/FACE_mineactable.xlsx")
