library(data.table)

xwalk <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19_Unified-Dataset/master/COVID-19_LUT.csv", 
               colClasses = "character")

zcta <- xwalk[Level == "ZCTA", ]

zcta[, .N, by = ISO2_UID]

usa <- xwalk[Admin0 == "United States", ]

fwrite(xwalk, file = "data/Hopkins_UnifiedData_GeoXwalk.csv")
