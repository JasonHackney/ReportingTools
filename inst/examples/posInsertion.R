library(ReportingTools)

df1 = data.frame(x=1:10, y=rnorm(10))

df2 = data.frame(x=c("hi", "lo", "there", "here"), y=4:7)

df3 = data.frame(x=c("I snuck in!"), y=10)

myrep = htmlReportRef(reportDirectory="./", basePath="./", shortName="PosInsert", title="Positional Insertion Test",
  handlers=fileWIndexHandlers)

publish(df1, myrep, name="data.frame1")
publish(df2, myrep, name="data.frame2")
publish(df3, myrep, name="data.frame3", pos=2)

myrep$finish()
