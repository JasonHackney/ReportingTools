library(rmarkdown)
render("testRmd.Rmd")

render("marraylmtest.Rmd")
render("pfamtest.Rmd")
render("rnaseqtest.Rmd")

render("knitrformattest.Rmd")
render("extensibility.Rmd")


#knit2html("../../vignettes/knitr.Rmd")
setwd("../../vignettes")
render("knitr.Rmd", output = "./reports/knitr.html")
