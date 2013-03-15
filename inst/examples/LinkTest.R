library(ReportingTools)
myrep1 = HTMLReport(shortName = "reportForIndex1", title = "My first report", reportDirectory="reports")
publish("Here is some stuff!!!", myrep1)
finish(myrep1)
myrep2 = HTMLReport(shortName = "reportForIndex2", title="My Second report", reportDirectory="reports")
publish("More stuff here!", myrep2)
finish(myrep2)
ind = HTMLReport(shortName = "index", reportDirectory="reports")
thing = Link(c(myrep1, myrep2), report=ind)

publish(thing, ind)
finish(ind)

thing2 = Link(c(myrep1, myrep2))
