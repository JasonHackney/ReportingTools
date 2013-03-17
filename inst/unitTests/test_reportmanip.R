library(XML)
test_posInsertion = function()
  {

    df1 = data.frame(x=1:10, y=rnorm(10))
    
    df2 = data.frame(x=c("hi", "lo", "there", "here"), y=4:7)
    
    df3 = data.frame(x=c("I snuck in!"), y=10)

    hnd = fileWIndexHandlers(ReportingTools:::makeReportPath("./", "./", "PosInsert"))
    
    myrep = HTMLReport(reportDirectory="./", basePath="./", shortName="PosInsert", title="Positional Insertion Test",
      handlers=list(hnd))
     
    publish(df1, myrep, name="data.frame1")
    publish(df2, myrep, name="data.frame2")
    publish(df3, myrep, name="data.frame3", pos=2)
    checkTrue(all(names(myrep$.report) == c("data.frame1", "data.frame3", "data.frame2")))
    rm(myrep)
  }


test_replacement = function()
  {
    df1 = data.frame(x=1:10, y=rnorm(10))

    hnd = fileWIndexHandlers(ReportingTools:::makeReportPath("./", "./", "PosInsert"))
    
    myrep = HTMLReport(reportDirectory="./", basePath="./", shortName="PosInsert", title="Positional Insertion Test",
      handlers=list(hnd))

    publish(df1, myrep, name = "mydf")

    publish("some text", myrep, name="mydf")

    checkTrue(xmlName(myrep$.report[["mydf"]][[1]]) == "p")
    rm(myrep)

  }
