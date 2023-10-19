library(knitr)

test_knitr = function(){

  #write to testRmd.html
  rmarkdown::render(system.file("examples/testRmd.Rmd", package="ReportingTools"), output_dir = "./")
  checkTrue(file.exists("testRmd.html"))
  lines = readLines("testRmd.html")
  checkTrue(length(grep("Using ReportingTools", lines)) == 1)
}
