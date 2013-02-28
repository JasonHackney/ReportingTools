library(ReportingTools)
mynewrep = readReport("./bigtest.html")


df = data.frame(x= rnorm(10), y = rnorm(10))
publish(df, mynewrep, "id2")
