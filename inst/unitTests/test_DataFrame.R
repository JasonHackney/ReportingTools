myDF <- DataFrame(swiss)

test_1toReportDF <- function(){
    testDF <- toReportDF(myDF)
    checkTrue(nrow(testDF) == 47, 
        "47 rows are returned in coercing the DataFrame to data.frame")
    checkTrue(ncol(testDF) == 6,
        "6 columns are returned in coercing the DataFrame to data.frame")
}
