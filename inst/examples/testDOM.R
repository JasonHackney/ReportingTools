library(ReportingTools)
options(error=recover)

mydf = data.frame(x=rnorm(10), y = rnorm(10))
myrep = htmlReportRef(reportDirectory = "tmp")


myrep[["df1"]] = mydf
myrep[["df2"]] = mydf
myrep[["im"]] = xyplot(y~x, data=mydf)
