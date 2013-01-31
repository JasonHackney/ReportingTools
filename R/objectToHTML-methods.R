setMethod("objectToHTML",
    signature = signature(
        object          = "data.frame"

      
    ),
    definition = function(object, report, tableTitle="",
      filter.columns = sapply(object, is.numeric), ...){
        
        
        if(nrow(object) == 0)
            stop("No rows available in data.")
        
        if(ncol(object) == 0)
            stop("No columns available in data.")

        filter.columns <-
          IRanges:::normalizeSingleBracketSubscript(filter.columns, object)
        
        sort.class.map <- c(
            "numeric"   = "sort-num",
            "integer"   = "sort-num",
            "Date"      = "sort-date"
        )
        sort.classes <- sort.class.map[sapply(object, class)]
        sort.classes[is.na(sort.classes)] <- "sort-string"
        
        filter.class.map <- c(
            "numeric" = "filter-num",
            "integer" = "filter-num",
            "logical" = "filter-cat",
            "factor"  = "filter-cat",
            "Date"    = "filter-date")
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
        object[, numeric.columns] <- signif(object[, numeric.columns], 3)        
        
       # p <- .writeHTMLTable(object, tableTitle = tableTitle, col.specs, 
       #     p = page(publicationType))
       # invisible(p)
        #addToReport(object, publicationType, tableTitle = tableTitle, col.specs)
        .writeHTMLTable(object, tableTitle = tableTitle, col.specs)
    }
)

setMethod("objectToHTML", 
          signature = signature(object = "HyperGResultBase"),
          definition = function(object, report,  
            pvalueCutoff = 0.01, categorySize = 10, ...){
            ## First, make a data.frame for publication,
            ## then call publish on that data.frame
            df <- .hyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff,
                                        categorySize = categorySize)
            objectToHTML(df)
   
    })


setMethod("objectToHTML",
    signature = signature(
        object = "GOHyperGResult"
      ),
    definition = function(object, 
        pvalueCutoff = 0.01, categorySize = 10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff,
            categorySize = categorySize)
        objectToHTML(df)
    }
)

setMethod("objectToHTML",
          signature = signature(
            object = "trellis"
            ),
          definition = function(object, ...)
          {
            .doImage(object, ...)
          })

setMethod("objectToHTML",
          signature = signature(
            object = "ggplot"
            ),
          definition = function(object, ...)
          {
            .doImage(object, ...)
          })

setMethod("objectToHTML",
          signature = signature(
            object = "ggbio"
            ),
          definition = function(object, ...)
          {
            .doImage(object, ...)
          })


.doImage = function(object, report, figureTitle=NULL, filename=NULL, 
            png.height = 480, png.width = 480, pdf.height = 7, pdf.width = 7, 
  br = TRUE, ...)
  {
                                        #figure.directory <- file.path(basePath(publicationType), 
            #                              reportDirectory(publicationType), 'figures')
    figure.directory <- file.path(report$basePath, 
                                  report$reportDirectory, 'figures')
    .safe.dir.create(figure.directory)
    if(is.null(filename)){
      randomPart <- round(runif(1)*100000)
                                        #filename <- paste(name(publicationType), randomPart, sep='-')
      filename <- paste(report$shortName, randomPart, sep='-')
    }
                                        #filename <- sub('.png', '', filename)
                                        #filename <- sub('.jpg', '', filename)
                                        #filename <- sub('.pdf', '', filename)
    filename = gsub("\\.(png|jpg|pdf)", "", filename,ignore.case=TRUE)
    pdf.filename <- file.path(figure.directory,
                              paste(filename, 'pdf', sep='.'))
    ## The URL is really hard to get right. What to do?
                                        #pdf.url <- paste(baseUrl(publicationType), 
                                        #                 reportDirectory(publicationType), 'figures', 
                                        #                   paste(filename, 'pdf', sep='.'), sep="/")
    pdf.url <- paste(report$baseUrl,
                     report$reportDirectory, 'figures', 
                     paste(filename, 'pdf', sep='.'), sep="/")
    
    png.filename <- file.path(figure.directory, 
                              paste(filename, 'png', sep='.'))
    png.url <- paste('figures', paste(filename, 'png', sep='.'), sep="/")
            
    cairo_pdf(filename = pdf.filename, height = pdf.height, width = pdf.width)
    print(object)
    dev.off()
    png(filename = png.filename, height = png.height, width= png.width)
    print(object)
    dev.off()
        
    img <- hwriteImage(png.url)
    hwrite(img, link=pdf.url, br=br)
  }



setMethod("objectToHTML",
          signature = signature(
        object = "GOHyperGResult"
      ),
    definition = function(object, report, selectedIDs,annotation.db,pvalueCutoff = 0.01, categorySize = 10, makePlot=FALSE, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.html(object, report, selectedIDs, annotation.db,pvalueCutoff = pvalueCutoff, categorySize=categorySize, makePlot=makePlot)
       objectToHTML(df)
    }
)


          

setMethod("objectToHTML",
    signature = signature(
        object = "PFAMHyperGResult"
      ),
    definition = function(object, report, selectedIDs,annotation.db,
        pvalueCutoff = 0.01,categorySize=10, name, path, ...){
        df <- .PFAMhyperG.to.htmlDF2(object, report,selectedIDs,annotation.db,pvalueCutoff = pvalueCutoff,categorySize )
        objectToHTML(df)
    }
)



.PFAMhyperG.to.htmlDF2 <- function(object, report, selectedIDs,annotation.db, pvalueCutoff = 0.01, categorySize=10)
{    
  	tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
	check.eg.ids(selectedIDs,annotation.db)

  	df<-summary(object, pvalue=pvalueCutoff, categorySize )
  	if(dim(df)[1]<1) {stop("No PFAMs match your criteria.")}

  	df$PFAMLink<-paste('<a href="http://pfam.sanger.ac.uk/family/', df$PFAMID, '">', df$PFAMID, '</a>', sep="")
  	pfamEnv <- getAnnMap("DE", "PFAM", load=TRUE)
  	df$PFAMDescription<-unlist(mget(df$PFAMID, pfamEnv, ifnotfound=NA))
  	
  	#pages.dirname <- paste0('PFAMPages', name(htmlRep))
        pages.dirname <- paste0('PFAMPages', report$shortName)  
   # page.directory <- file.path(basePath(htmlRep), 
    #    reportDirectory(htmlRep), pages.dirname)
        page.directory <- file.path(path, pages.dirname)
        .safe.dir.create(page.directory)
    pfam.reportDirectory <- paste(report$reportDirectory, 
        pages.dirname, sep="/")
    makeGeneListPages(object,reportDir=pfam.reportDirectory,  pvalueCutoff=pvalueCutoff,categorySize,selectedIDs, annotation.db, GO=FALSE, baseUrl=report$baseUrl, basePath=report$basePath)  

  	df$CountLink<-paste('<a href="',pages.dirname,"/", df$PFAMID, ".html",'">', df$Count, '</a>', sep="")
   	df$SizeLink<-paste('<a href="',pages.dirname,"/", df$PFAMID, "All.html",'">', df$Size, '</a>', sep="")
  	ret<-data.frame(df$PFAMLink,df$PFAMDescription,df$SizeLink,Image = rep("", nrow(df)),df$CountLink, signif(df$OddsRatio, 3), signif(df$Pvalue, 3), stringsAsFactors = FALSE)
 	colnames(ret)<-c("PFAM ID", "PFAM Term", "PFAM Size","Image","Overlap", "Odds Ratio", "P-value")

    figure.dirname <- paste0('PFAMFigures', report$shortName)
    figure.directory <- file.path(report$basePath, report$reportDirectory, figure.dirname)
    .safe.dir.create(figure.directory)

    numSelectedIDs<-length(selectedIDs)    
	largestTerm<-max(df$Size)
	for (i in 1:dim(df)[1]){
  		PFAMID<-df$PFAMID[i]
  		png.filename <- paste(PFAMID ,"png", sep='.')
 	 	png.file <- file.path(figure.directory, png.filename)
  		png(png.file)
  		hyperGPlot(df$Size[i]-df$Count[i],numSelectedIDs-df$Count[i], df$Count[i], PFAMID, df$PFAMDescription[i])
  		dev.off()
  		
  		pdf.filename <- paste(PFAMID, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        pdf(pdf.file)
  		hyperGPlot(df$Size[i]-df$Count[i],numSelectedIDs-df$Count[i], df$Count[i], PFAMID, df$PFAMDescription[i])
        dev.off()
        
        ret$Image[i] <- hwriteImage(paste(figure.dirname,png.filename, sep="/"), link=paste(figure.dirname,pdf.filename, sep="/"), table=FALSE,width=100, height=100)
	} 
    return(ret)
}





setMethod("objectToHTML", signature = signature(
                            object = "DGEExact"),
          definition = function(object, rep, ...)
            objectToHTML(.DGEExact.to.html2(object, rep, ...)))
          


.DGEExact.to.html2 <- function(object, htmlRep, countTable, conditions, 
    annotation.db = 'org.Hs.eg', pvalueCutoff = 0.01, n = 1000, lfc = 0, 
    adjust.method = 'BH', sort.method = 'p.value', make.plots = TRUE, ...)
{
    dat <- topTags(object, n = n, adjust.method = adjust.method, sort.by = sort.method)

    ## Check valid Entrez ids are passed in
    check.eg.ids(rownames(dat), annotation.db)
    
    selection <- as.numeric(rownames(dat$table))
    ##The following gives you all pvalues
    padj <- p.adjust(object$table$PValue, method = adjust.method)
    padj <- padj[match(selection, rownames(object$table))]
    dat <- data.frame(dat$table, padj)

    dat.adj <- dat[dat$padj < pvalueCutoff,]
    dat.lfc <- dat.adj[abs(dat.adj$logFC) > abs(lfc), ]

    if(length(rownames(dat.lfc)) == 0)
        stop("No genes meet the selection criteria. Try changing the log-fold change or p-value cutoff.")
  
    ann.map.available <- tryCatch(getAnnMap("SYMBOL", annotation.db), 
        error=function(e){ return(FALSE)})
    
    if (inherits(ann.map.available, "AnnDbBimap")){
      fdata <- data.frame(
        EntrezId = unlist(rownames(dat.lfc)),
        Symbol = unlist(mget(rownames(dat.lfc), 
            getAnnMap("SYMBOL", annotation.db), ifnotfound = NA)),
        GeneName = unlist(mget(rownames(dat.lfc), 
            getAnnMap("GENENAME", annotation.db), ifnotfound = NA)),
        stringsAsFactors = FALSE
      )

    fdata$EntrezId <- hwrite(fdata$EntrezId, 
        link=paste("http://www.ncbi.nlm.nih.gov/gene/", fdata$EntrezId, sep=''), table=FALSE)
      
    } else {
      IDs <- rownames(dat.lfc)
      fdata <- data.frame(IDs, stringsAsFactors = FALSE)
    }    
    
    
    if( ! make.plots ){
        ret <- data.frame(
            fdata,
            dat.lfc$logFC,
            dat.lfc$padj,
            stringsAsFactors = FALSE
        )
    } else {
        ret <- data.frame(
            fdata,
            Image = rep("", nrow(fdata)),
            dat.lfc$logFC,
            dat.lfc$padj,
            stringsAsFactors = FALSE
        )

        ## log2 transform count data and add one for psuedocount
        countTable <- log2(countTable + 1)
    
        figures.dirname <- paste('figures', htmlRep$shortName, sep='')  
        figure.directory <- file.path(htmlRep$basePath, 
            htmlRep$reportDirectory, figures.dirname)
        .safe.dir.create(figure.directory)
        
        .make.gene.plots(ret, countTable, conditions, figure.directory,
            ylab.type = "(log2 counts per million)")
        
        mini.image <- file.path(figures.dirname, 
            paste("mini", rownames(dat.lfc), "png", sep="."))
        pdf.image <- file.path(figures.dirname, 
            paste("boxplot", rownames(dat.lfc), "pdf", sep="."))
        ret$Image <- hwriteImage(mini.image, link=pdf.image, table=FALSE)
    
    }
    
    colnames(ret)[which(colnames(ret) == 'dat.lfc.logFC')] <- 'logFC'
    colnames(ret)[which(colnames(ret) == 'dat.lfc.padj')] <- 'p-Value'
    
    return(ret)
}








if(FALSE)
  {
setMethod("publish",
    signature = signature(
        object = "PFAMHyperGResult",
        publicationType = "ANY"
    ),
    definition = function(object, report, 
        pvalueCutoff = 0.01,categorySize=10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .PFAMhyperG.to.data.frame(object, report, pvalueCutoff = pvalueCutoff, categorySize)
        publish(df, publicationType, ...)
    }
)
}

