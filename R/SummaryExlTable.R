# remove outlier
boxplot(mine$nitrification)
RmOl <- mine
RmOl$nitrification[which(RmOl$nitrification == max(RmOl$nitrification))] <- NA

# melt dataset
mineMlt <- melt(RmOl, id = names(mine)[which(!(names(mine) %in% 
                                                 c("nitrification", "n.min", 
                                                   "ammonification", "p.min")))])

# chenge the order of variables
mineMlt$variable <- factor(mineMlt$variable, 
                           levels = c("n.min", "nitrification", 
                                      "ammonification", "p.min"))

# response ratio calculated for each block
BlockRatio(mineMlt)

# Ring summary table & mean
RngSmmryTbl <- dlply(mineMlt, .(variable), 
                     function(x) CreateTable(x, fac = "ring", digit = 3, nsmall = 3))
RngMean <- ddply(mineMlt, .(time, date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 3, nsmall = 3, scientific = FALSE))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata and rowdata without outlier
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(mine, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

sheet <- createSheet(wb,sheetName="raw_data_withoutOutlier")
addDataFrame(RmOl, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")


# worksheets for ring summary
vars <- c("N_mineralisation", "Nitrification", "Ammonification", "P_mineralisation")
shnames <- paste("Ring_mean.",vars ,sep="")
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for co2 trt summary
shnames <- paste("CO2_mean.", vars, sep = "")
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"output/table/FACE_mineactable.xlsx")
