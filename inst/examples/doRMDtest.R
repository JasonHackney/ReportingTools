library(knitr)
knit2html("testRmd.Rmd")

knit2html("marraylmtest.Rmd")
knit2html("pfamtest.Rmd")
knit2html("rnaseqtest.Rmd")

knit2html("knitrformattest.Rmd")
knit2html("extensibility.Rmd")


#knit2html("../../vignettes/knitr.Rmd")
setwd("../../vignettes")
knit2html("knitr.Rmd", output = "./reports/knitr.html")
