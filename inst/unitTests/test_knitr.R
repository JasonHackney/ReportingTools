library(knitr)

test_knitr = function(){

  #write to testRmd.html
  knit2html(system.file("examples/testRmd.Rmd", package="ReportingTools"))
  checkTrue(file.exists("testRmd.html"))
  lines = readLines("testRmd.html")
  checkTrue(length(grep("Error", lines)) == 0)
}
