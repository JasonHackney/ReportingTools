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

test_constructors3= function(){
  #passing  a constructor
  myrep = HTMLReport(shortName = "something", basePath = ".", reportDirectory = "reports",
    handlers = knitrHandlers, location="/lol/somelocation/stuff.html")

  checkTrue(path(myrep) == "/lol/somelocation/stuff.html")
}


  
