library(lattice)

my.df <- data.frame(
    EGID = c("103", "104", "105", "106", "107"),
    RPKM = c(4, 5, 3, 100, 75),
    DE = factor(c("Yes", "Yes", "No", "No", "No")),
    stringsAsFactors = FALSE
)

htmlRep <- HTMLReport("testhtmlPage", reportDirectory = 'testHTMLDirectory',
    title = "Test Report")
publish(my.df, htmlRep)

dataPkg <- DataPackage('testPkg', reportDirectory = 'testDataPackage',
    title = "Test Data Package", author='Jonas J. Nobody <jjnobody@nowhere.net>')

csvFile <- CSVFile('testCSV', reportDirectory = 'testCSVFile')
publish(my.df, csvFile)

test_1dataPkg <- function(){
    checkTrue(file.exists('testDataPackage'), 
        "The DataPackage reportDirectory is created.")
    checkTrue(file.exists('testDataPackage/testPkg'),
        "The DataPackage root directory is created.")
    checkTrue(file.exists('testDataPackage/testPkg/data'),
        "The DataPackage data directory is created.")
    checkTrue(file.exists('testDataPackage/testPkg/man'),
        "The DataPackage man directory is created.")
}

test_2dataPkgContents <- function()
{
    checkTrue(file.exists('testDataPackage/testPkg/DESCRIPTION'),
        "The DESCRIPTION file is made.")
    checkTrue(file.exists('testDataPackage/testPkg/NAMESPACE'),
        "The NAMESPACE file is made.")
    checkEquals(dependencies(dataPkg), c('Biobase'),
        "The initial dependencies list is correct")
    desc <- as.list(read.dcf('testDataPackage/testPkg/DESCRIPTION')[1,])
    
    ## Make sure all required fields are present
    checkTrue(all(c("Package", "Version", "License", "Description", "Title", 
        "Author", "Maintainer") %in% names(desc)),
        "All required fields are present in the DESCRIPTION file.")
    
    ## Does the version string look like 0.0.0
    checkEquals(grep('^[0-9]\\.[0-9]\\.[0-9]$', desc$Version, perl=TRUE), c(1),
        "The version string is valid.")
    
    ## Make sure dependencies are the same
    checkEquals(desc$Depends, paste(dependencies(dataPkg), collapse=','),
        "The dependencies in the DESCRIPTION file match the dependencies in the object.")
    
    ## Make sure an e-mail address is in the maintainer field
    maintainer <- as.person(desc$Maintainer)
    checkTrue(!(is.null(maintainer$email) | is.na(maintainer$email) ),
        "The maintainer field can be parsed by as.person")
}

test_3dataPkgPublish <- function()
{
    # publish('my.df', dataPkg) # This isn't working for some reason
    publish(my.df, dataPkg, objectName = 'my.df2')
    
    # checkTrue(file.exists("testDataPackage/testPkg/data/my.df.rda"))
    checkTrue(file.exists("testDataPackage/testPkg/data/my.df2.rda"),
        "The data.frame is published to the correct file.")
    
    ## Round trip on the object
    load('testDataPackage/testPkg/data/my.df2.rda')
    checkEquals(my.df, my.df2, 
        "The data.frame saved is the same as the original object")
    
    my.plot <- xyplot(1:10 ~ 1:10)
    publish(my.plot, dataPkg, objectName='my.plot')
    
    ## The lattice dependency is added
    checkTrue(all(dependencies(dataPkg) %in% c('Biobase', 'lattice')),
        "Publishing a trellis object add lattice to the dependency list.")
    ## There are no extra dependencies
    checkTrue(length(setdiff(dependencies(dataPkg), c('Biobase', 'lattice')))==0,
        "No additional dependencies are present in the list.")
    
    finish(dataPkg)
    
    ## Check the dependencies in the DESCRIPTION have updated to reflect new
    ## dependencies in the package
    
    desc <- as.list(read.dcf('testDataPackage/testPkg/DESCRIPTION')[1,])
    checkEquals(desc$Depends, paste(dependencies(dataPkg), collapse=','),
        "Calling finish recreates the dependencies list for the DataPackage, now including lattice.")
    
}

test_4csvFile <- function(){
    checkTrue(file.exists('testCSVFile'), 
        "The reportDirectory for the CSVFile is created.")
    checkTrue(file.exists('testCSVFile/testCSV.csv'),
        "The csv file is created after publication.")
    my.df3 <- read.csv('testCSVFile/testCSV.csv', 
        colClasses=c('character', 'character','numeric','factor'),
        row.names = 1)
    ## Need to fix this test so I can also check attributes
    checkTrue(all.equal(my.df3, my.df, check.attributes=FALSE),
        "Reading the csv file yields the same data as the original.")
}

test_5htmlRep <- function(){
    checkEquals(htmlRep$shortName, "testhtmlPage",
        "The name of the HTMLReport is correctly set.")
    checkEquals(htmlRep$title, "Test Report",
        "The title of the HTMLReport is correctly set.")
    checkEquals(path(htmlRep), 'testHTMLDirectory/testhtmlPage.html',
        "The URI for the HTMLReport is correctly formed.")
    checkTrue(file.exists('testHTMLDirectory'), 
        "The reportDirectory for the HTMLReport exists.")
    checkTrue(!file.exists('testHTMLDirectory/testhtmlPage.html'),
        "The HTML file isn't created created.")
}

test_6data.frames <- function(){
    df4 <- data.frame(RPKM = c(1, -2/100, 3/1000000, 9.03e-4, 0, NA,NA,NA,1,
        1e-44))
    df5 <- data.frame(probeId = c (NA, "A_23_P100022", "A_23_P100056",
            "A_23_P100074","A_23_P100103", "A_23_P100127",NA,"A","C","R"),
        RPKM = c(1, -2/100, 3/1000000, 9.03e-4, 0, NA,NA,NA,1, 1e-44))
    htmlRep2 <- HTMLReport("testhtmlPage2", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report")

    publish(df4, htmlRep2)
    publish(df5, htmlRep2)
    finish(htmlRep2)

    htmlRep3 <- HTMLReport("testhtmlPage3", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report")
    publish(df4, htmlRep3)
    publish(df5, htmlRep3)
    finish(htmlRep3)
}

test_zzzcloseHTMLConnection <- function(){
    finish(htmlRep)
    checkTrue(file.exists('testHTMLDirectory/testhtmlPage.html'),
        "The HTML file isn't created.")
}
