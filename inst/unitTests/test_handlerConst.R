test_constructors1 = function(){
  #default handler construction
  myrep1 = HTMLReport(shortName = "something", basePath = ".", reportDirectory = "reports")
  checkTrue(path(myrep1) == "./reports/something.html")
}

test_constructors2 = function(){
  #passing  a constructor
  myrep = HTMLReport(shortName = "something", basePath = ".", reportDirectory = "reports",
    handlers = fileWIndexHandlers)

  checkTrue(path(myrep) == "./reports/something.html")
}


  
