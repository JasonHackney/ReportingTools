library(XML)
rep1 = HTMLReport(shortName = "report1", reportDirectory="reports")
rep2 = HTMLReport(shortName = "report2", reportDirectory="reports2")
index = HTMLReport(shortName = "indexTest", reportDirectory="reports")
finish(rep1)
finish(rep2)
test_relativelinks = function(){
  publish(Link(list(rep1, rep2), report=index), index)
  dom = index$.reportDOM
  oldwd = getwd()
  setwd(dirname(path(index))) #go to the directory of the index report to test relative location links
  linklocs = xpathSApply(dom, "//a/@href") #grab the link locations from the DOM
  checkTrue(all(sapply(linklocs, file.exists)))
}
