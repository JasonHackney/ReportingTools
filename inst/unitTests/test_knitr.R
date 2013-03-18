library(knitr)

test_knitr = function(){
  
  fileout = knit2html(system.file("inst/examples/testRmd.Rmd", package="ReportingTools"))
  checkTrue(file.exists(fileout))
  lines = readLines(fileout)
  checkTrue(length(grep("Error", lines)) == 0)
}
