library(ReportingTools)

myrep = HTMLReport(reportDirectory = "./",shortName="paratest", handlers = fileWIndexHandlers)

publish("text1", myrep)

publish("more text", myrep)

publish("some inline text", myrep, para=FALSE)

publish("more linline text", myrep, para=FALSE)

publish("regular text again", myrep)

finish(myrep)
