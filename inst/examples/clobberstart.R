library(ReportingTools)
library(XML)

mydf = data.frame(x = 1:5, y = c("hi", "lo", "medium", "stuff", "goodbye"))

newStartPage = function(...)
    {
        htmlParse("<html><head></head><body><p>This initial page is WAY better! Too bad all the JS/CSS is now broken :(</p></body></html>")
    }

makeHTMLClobberer = function(basePath, reportDirectory, shortName, startPage, ...)
    {
        loc = ReportingTools:::makeReportPath(basePath, reportDirectory, paste0(shortName, "_clobbered"))
        new("ReportHandlers",
            init = function(node, args)
            {
                rnode = xmlRoot(node)
                realdom = do.call(args$startPage,args)
                newrnode = xmlRoot(realdom)
                removeChildren(rnode, kids = xmlChildren(rnode))
                addChildren(rnode, kids = xmlChildren(newrnode))
                node
            },
                                        #finish by writing the file
            finish =  function(rep, args)
            {
                saveXML(rep$.reportDOM, file=loc)
            },
            args = list(init = list(startPage = startPage, ...)),
            location = loc
            )
        
    }

myrep1 = HTMLReport(reportDirectory = "./reports")
publish(mydf, myrep1)
finish(myrep1)

myrep2 = HTMLReport(reportDirectory = "./reports", handlers = makeHTMLClobberer, startPage = newStartPage)
publish(mydf, myrep2)
finish(myrep2)
