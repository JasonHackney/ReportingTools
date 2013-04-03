setMethod("objectToHTML",
    signature = signature(object = "ANY"),
    definition = function(object, report, .modifyDF, .toDF, ...)
    {
        if(!missing(.toDF) && is.function(.toDF))
            df = .toDF(object, report, ...) #XXX adding report here for the quick fix, this should be taken back out once the methods are properly sorted
        else
            df = toReportDF(object, report,...)

        if(!missing(.modifyDF) && !is.null(.modifyDF)){
            if(!is.list(.modifyDF))
                .modifyDF = list(.modifyDF)
                
            for(f in .modifyDF)
                df = f(df, report, object = object, ...)
        } else {
            df = modifyReportDF(df, report, object = object, ...)
        }
        objectToHTML(df, report = report, .toDF = NULL, .modifyDF = NULL)

    }
)

setMethod("objectToHTML", signature(object = "XMLInternalNode"),
    definition = function(object, report, ...)
    {
        object
    }
)


setMethod("objectToHTML",
    signature = signature(object = "character"),
    definition = function(object, report, para = TRUE,...)
    {
        if(length(object) > 1){
            toret = lapply(object, objectToHTML, para=para)
            toret = unlist(toret, recursive=FALSE)
            return(structure(toret, class="XMLNodeSet"))
        }
        # if it is HTML code it will be handled as such, if not it will be spit
        # out as text in the page (in a <p>)...
        ret = tryCatch(htmlParse(object), error=function(e) NULL)
        if(is.null(ret)){
            if(!para)
                newXMLNode("span", object)
            else
                #stuff our text into a <p> node
                newXMLNode("p", object)
        }
        else
          getNodeSet(ret, "//body/*")
    }
)

setMethod("objectToHTML",
    signature = signature(
        object          = "data.frame"),
    definition = function(object, report, .modifyDF,   tableTitle="",
      filter.columns = sapply(object, is.numeric), ...){

       if(!missing(.modifyDF) && !is.null(.modifyDF))
              {
                if(!is.list(.modifyDF))
                  .modifyDF = list(.modifyDF)

                for(f in .modifyDF)
                  object = f(object, report, object = object, ...)
              } #else {
                #object = modifyReportDF(object, report, object = object, ...)
              #}
            
        if(nrow(object) == 0)
            stop("No rows available in data.")
        
        if(ncol(object) == 0)
            stop("No columns available in data.")

        filter.columns <-
          IRanges:::normalizeSingleBracketSubscript(filter.columns,object)
        
        sort.class.map <- c(
            "numeric"   = "sort-num-robust",
            "integer"   = "sort-num-robust",
            "Date"      = "sort-date",
            "character" = "sort-string-robust",
            "factor"    = "sort-string-robust"
        )
        sort.classes <- sort.class.map[sapply(object, class)]
        sort.classes[is.na(sort.classes)] <- "sort-string-robust"
        
        filter.class.map <- c(
            "numeric" = "filter-num",
            "integer" = "filter-num",
            "logical" = "filter-cat",
            "factor"  = "filter-cat",
            "Date"    = "filter-date",
            "character" = "filter-string")
        filter.classes <- filter.class.map[sapply(object, class)]
        filter.classes[is.na(filter.classes)] <- "filter-string"
        sel.filter.classes <- filter.classes[filter.columns]
        col.classes <- sort.classes
        col.classes[filter.columns] <-
          paste(sel.filter.classes, col.classes[filter.columns])
        
        col.specs <- data.frame(
            column  = seq_along(object),
            label   = colnames(object),
            class   = col.classes,
            stringsAsFactors = FALSE
        )
        
        numeric.columns <- which(unlist(lapply(object, class)=="numeric"))
        for(col in numeric.columns){
            object[, col] <- signif(object[, col], 3)
        }
       # p <- .writeHTMLTable(object, tableTitle = tableTitle, col.specs, 
       #     p = page(publicationType))
       # invisible(p)
        #addToReport(object, publicationType, tableTitle = tableTitle, col.specs)
        html = .writeHTMLTable(object, tableTitle = tableTitle, col.specs)
        list(html=html, object = object)
    }
)

setMethod("objectToHTML",
    signature = signature(object = "trellis"),
    definition = function(object, report, ...)
    {
        .doImage(object, report, ...)
    }
)

if(require(ggplot2)){
    setMethod("objectToHTML",
        signature = signature(object = "ggplot"),
        definition = function(object, report, ...){
            .doImage(object, report, ...)
        }
    )
}

if(require(ggbio)){
    setMethod("objectToHTML",
        signature = signature(object = "ggbio"),
        definition = function(object, report, ...)
        {
            .doImage(object, report, ...)
        }
    )
}

setMethod("objectToHTML",
    signature = signature(object = "recordedplot"),
    definition = function(object, report, ...)
    {
        .doImage(object, report, ...)
    }
)


.doImage <- function(object, htmlRep, figureTitle=NULL, filename=NULL, 
    png.height = 480, png.width = 480, pdf.height = 7, pdf.width = 7, 
    br = TRUE, ...)
{
    
    figures.dirname <- paste0('figures', htmlRep$shortName)  
    figure.directory <- file.path(dirname(path(htmlRep)[1]), figures.dirname)
    .safe.dir.create(figure.directory)
    
    if(is.null(filename)){
        randomPart <- round(runif(1)*100000)
        filename <- paste(htmlRep$shortName, randomPart, sep='-')
    }
    
    filename <- gsub("\\.(png|jpg|pdf)", "", filename,ignore.case=TRUE)
    url.location <- getRelativePath(figure.directory, dirname(path(htmlRep)[1]))
    
    pdf.filename <- file.path(figure.directory, paste(filename, 'pdf', sep='.'))
    pdf.url <- paste(url.location, paste(filename, 'pdf', sep='.'), sep="/")
    
    png.filename <- file.path(figure.directory, paste(filename, 'png', sep='.'))
    png.url <- paste( url.location, paste(filename, 'png', sep='.'), sep="/")
        
    cairo_pdf(filename = pdf.filename, height = pdf.height, width = pdf.width)
    print(object)
    dev.off()
    
    png(filename = png.filename, height = png.height, width= png.width)
    print(object)
    dev.off()
    
     anode = newXMLNode("a", attrs=list(href = pdf.url), newXMLNode("img", attrs=list(src=png.url, alt=png.url)))
     anode
}


.PFAMhyperG.to.htmlDF2 <- function(object, report, selectedIDs,annotation.db, 
    pvalueCutoff = 0.01, categorySize=10)
{    
    tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
        {stop(paste0("Unable to find your annotation.db: ",annotation.db))}
    )
    check.eg.ids(selectedIDs,annotation.db)
    
    df<-summary(object, pvalue=pvalueCutoff, categorySize )
    if(dim(df)[1]<1) {stop("No PFAMs match your criteria.")}

    df$PFAMLink <- paste0('<a href="http://pfam.sanger.ac.uk/family/', 
        df$PFAMID, '">', df$PFAMID, '</a>')
    pfamEnv <- getAnnMap("DE", "PFAM", load=TRUE)
    df$PFAMDescription <- unlist(mget(df$PFAMID, pfamEnv, ifnotfound=NA))

    pages.dirname <- paste0('PFAMPages', report$shortName)  
    page.directory <- file.path(dirname(path(report)), pages.dirname)
    .safe.dir.create(page.directory)

    pfam.reportDirectory <- paste(report$reportDirectory, pages.dirname, sep="/")
    makeGeneListPages(object,reportDir=pfam.reportDirectory,  
        pvalueCutoff=pvalueCutoff, categorySize, selectedIDs, annotation.db, 
        GO=FALSE, basePath=report$basePath) 
    
    df$CountLink <- paste0('<a href="', pages.dirname, "/", df$PFAMID, ".html",
        '">', df$Count, '</a>')
    df$SizeLink <- paste0('<a href="', pages.dirname, "/", df$PFAMID, 
        "All.html",'">', df$Size, '</a>')
    ret <- data.frame(
        "PFAM ID" = df$PFAMLink, 
        "PFAM Term" = df$PFAMDescription,
        "PFAM Size" = df$SizeLink, 
        Image = rep("", nrow(df)),
        "Overlap" = df$CountLink, 
        "Odds Ratio" = signif(df$OddsRatio, 3), 
        "P-value" = signif(df$Pvalue, 3), 
        stringsAsFactors = FALSE, check.names = FALSE)
    
    figure.dirname <- paste0('PFAMFigures', report$shortName)
    figure.directory <- file.path(dirname(path(report)), figure.dirname)
    .safe.dir.create(figure.directory)

    numSelectedIDs<-length(selectedIDs)    
    largestTerm<-max(df$Size)
    for (i in 1:dim(df)[1]){
        PFAMID<-df$PFAMID[i]
        png.filename <- paste(PFAMID ,"png", sep='.')
        png.file <- file.path(figure.directory, png.filename)
        
        png(png.file)
        hyperGPlot(df$Size[i]-df$Count[i], numSelectedIDs-df$Count[i], 
            df$Count[i], PFAMID, df$PFAMDescription[i])
        dev.off()

        pdf.filename <- paste(PFAMID, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        pdf(pdf.file)
        hyperGPlot(df$Size[i]-df$Count[i], numSelectedIDs-df$Count[i], 
            df$Count[i], PFAMID, df$PFAMDescription[i])
        dev.off()

        ret$Image[i] <- hwriteImage(paste(figure.dirname,png.filename, sep="/"), 
            link=paste(figure.dirname,pdf.filename, sep="/"), table=FALSE,
            width=100, height=100)
    } 
    return(ret)
}


.GOhyperG.to.html2 <- function(object, htmlRep, selectedIDs, annotation.db, 
    pvalueCutoff = 0.01, categorySize = 10, makePlot = FALSE)
{    
    tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
        {stop(paste0("Unable to find your annotation.db: ",annotation.db))}
    )
    check.eg.ids(selectedIDs,annotation.db)
    
    df<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
    
    if(dim(df)[1]<1) {stop("No categories match your criteria.")}
    
    df$GOID <- df[,1]
    df$GOLink <- paste('<a href="http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=', 
        df$GOID, '">', df$GOID, '</a>', sep="")
    df$goName <- unlist(lapply(df$GOID, function(x) {strsplit(x, ":")[[1]][2]}))
    
    pages.dirname <- paste0('GOPages', htmlRep$shortName)  
    page.directory <- file.path(dirname(path(htmlRep)), pages.dirname)
    .safe.dir.create(page.directory)
    
    go.reportDirectory <- paste(htmlRep$reportDirectory, pages.dirname, sep="/")
    
    makeGeneListPages(object, reportDir = go.reportDirectory, 
        pvalueCutoff = pvalueCutoff, categorySize, selectedIDs, annotation.db, 
        GO=TRUE, basePath = htmlRep$basePath)  
        
    df$CountLink <- paste('<a href="', pages.dirname, "/" , 
        df$goName, ".html",'">', df$Count, '</a>', sep="")
    df$SizeLink <- paste('<a href="', pages.dirname, "/", 
        df$goName, "All.html",'">', df$Size, '</a>', sep="")
    ret <- data.frame(df$GOLink, df$Term, df$SizeLink, Image = rep("", nrow(df)), 
        df$CountLink, signif(df$OddsRatio, 3), signif(df$Pvalue, 3),
        stringsAsFactors = FALSE)
    colnames(ret) <- c("Accession", "GO Term", "Category Size", "Image", 
        "Overlap", "Odds Ratio", "P-value")
 
    figure.dirname <- paste0('GOFigures', htmlRep$shortName)
    figure.directory <- file.path(dirname(path(htmlRep)), figure.dirname)
    .safe.dir.create(figure.directory)
    
    if (makePlot==TRUE){
        plotGOResults(object,pvalueCutoff, categorySize, reportDir=figure.directory)
        plotret = hwrite(hwriteImage(paste(figure.dirname,"GOPlot.svg", sep="/"), link=paste(figure.dirname,"GOPlot.svg", sep="/"),width=400, height=400), br = TRUE)
    }
    
    numSelectedIDs<-length(selectedIDs)    
    largestTerm<-max(df$Size)
    
    for (i in 1:dim(df)[1]){
        GONum<-as.character(strsplit(df$GOID[i], ":")[[1]][2])
        png.filename <- paste(GONum ,"png", sep='.')
        png.file <- file.path(figure.directory, png.filename)
        
        png(png.file)
        hyperGPlot(df$Size[i]-df$Count[i], numSelectedIDs-df$Count[i], 
            df$Count[i], df$GOID[i], df$Term[i])
        dev.off()

        pdf.filename <- paste(GONum, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        pdf(pdf.file)
        hyperGPlot(df$Size[i]-df$Count[i], numSelectedIDs-df$Count[i], 
            df$Count[i], df$GOID[i], df$Term[i])
        dev.off()
        
        ret$Image[i] <- hwriteImage(paste(figure.dirname,png.filename, sep="/"), 
            link=paste(figure.dirname,pdf.filename, sep="/"), 
            table=FALSE,width=100, height=100)
    }
    
    if(makePlot)
        # If we have the plot we need to mash it all together into HTML here
        # because of how the dispatch is currently set up. We may want to change
        # this in the future...
        paste("<div>",plotret, objectToHTML(ret), "</div>", sep="\n") 
    else
        ret
}



.GeneSetCollection.to.html2 <- function(object, htmlRep, annotation.db = NULL, 
    setStats = NULL, setPValues = NULL, geneStats = NULL)
{
    pages.dirname <- paste0('GeneSetCollectionPages', htmlRep$shortName)  
    page.directory <- file.path(dirname(path(htmlRep)), pages.dirname)
    .safe.dir.create(page.directory)
    
    gs.reportDirectory <- paste(htmlRep$reportDirectory, pages.dirname, 
        sep = "/")
    
    makeGeneListPagesGSC(object, reportDir = gs.reportDirectory, 
        annotation.db, geneStats = geneStats, basePath = htmlRep$basePath)
   
    names2<-names(object)
    names2<-gsub(":","", names2)
    names2<-gsub(" ","", names2)
    names2<-gsub(";","", names2)
   
    setLink <- paste('<a href="',pages.dirname,"/", names2, 
        ".html",'">', names(object), '</a>', sep="")
    descriptions <- sapply(object, description)

    ret <- data.frame("GeneSet" = setLink, "Description" = descriptions,
        stringsAsFactors = FALSE)
    colnames(ret) <- c("Gene Set", "Description")
    
    if (descriptions[1]==""){
        ret <- data.frame("GeneSet" = setLink, stringsAsFactors = FALSE)
        colnames(ret) <- c("Gene Set")
    }
    if(!is.null(setStats)){
        ret <- cbind(ret, setStats)
        colnames(ret)[ncol(ret)] <- "Gene Set Statistic"
    }
    if(!is.null(setPValues)){
        ret <- cbind(ret, setPValues)
        colnames(ret)[ncol(ret)] <- "Gene Set P-value"
    }
    return(ret)
}



