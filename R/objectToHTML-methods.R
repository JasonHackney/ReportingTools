setMethod("objectToHTML",
          signature = signature(
            object = "ANY"),
          definition = function(object, report, .addColumns, .toDF, ...)
          {
            #cat("I'm in the ANY method for objectToHTML")
            if(!missing(.toDF) && is.function(.toDF))
              df = .toDF(object, report, ...) #XXX adding report here for the quick fix, this should be taken back out once the methods are properly sorted
            else
              df = toReportDF(object, report,...)

            if(!missing(.addColumns) && !is.null(.addColumns))
              {
                if(!is.list(.addColumns))
                  .addColumns = list(.addColumns)

                for(f in .addColumns)
                  df = f(df, report, object = object, ...)
              } else {
                df = addReportColumns(df, report, object = object, ...)
              }
            objectToHTML(df, report = report, .toDF = NULL, .addColumns = NULL)

          })




setMethod("objectToHTML",
          signature = signature(
            object = "character"),
          definition = function(object, report, ...)
          {
            #if it is HTML code it will be handled as such, if not it will be spit out as text in the page (in a <p>)...
            ret = tryCatch(htmlParse(object), error=function(e) NULL)
            if(is.null(ret))
              #stuff our text into a <p> node
              newXMLNode(p, object)
            else
              getNodeSet(ret, "//body/*")
          })

setMethod("objectToHTML",
    signature = signature(
        object          = "data.frame"),
    definition = function(object, report, .addColumns,   tableTitle="",
      filter.columns = sapply(object, is.numeric), ...){

       if(!missing(.addColumns) && !is.null(.addColumns))
              {
                if(!is.list(.addColumns))
                  .addColumns = list(.addColumns)

                for(f in .addColumns)
                  object = f(object, report, object = object, ...)
              } #else {
                #object = addReportColumns(object, report, object = object, ...)
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

if(FALSE)
  {
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
}
setMethod("objectToHTML",
          signature = signature(
            object = "trellis"
            ),
          definition = function(object, report, ...)
          {
            .doImage(object, report, ...)
          })
if(require(ggplot2))
  {
setMethod("objectToHTML",
          signature = signature(
            object = "ggplot"
            ),
          definition = function(object, report, ...)
          {
            .doImage(object, report, ...)
          })
}
if(require(ggbio))
  {
    setMethod("objectToHTML",
          signature = signature(
            object = "ggbio"
            ),
          definition = function(object, report, ...)
          {
            .doImage(object, report, ...)
          })
  }
setMethod("objectToHTML",
          signature = signature(
            object = "recordedplot"
            ),
          definition = function(object, report, ...)
          {
            .doImage(object, report, ...)
          })


.doImage = function(object, htmlRep, figureTitle=NULL, filename=NULL, 
            png.height = 480, png.width = 480, pdf.height = 7, pdf.width = 7, 
  br = TRUE, ...)
  {

     figures.dirname <- paste0('figures', htmlRep$shortName)  
     figure.directory <- file.path(htmlRep$basePath, 
            htmlRep$reportDirectory, figures.dirname)
     .safe.dir.create(figure.directory)
        
    if(is.null(filename)){
      randomPart <- round(runif(1)*100000)
                                        #filename <- paste(name(publicationType), randomPart, sep='-')
      filename <- paste(htmlRep$shortName, randomPart, sep='-')
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
    pdf.url <- paste(htmlRep$baseUrl,
                     htmlRep$reportDirectory, figures.dirname, 
                     paste(filename, 'pdf', sep='.'), sep="/")
    
    png.filename <- file.path(figure.directory, 
                              paste(filename, 'png', sep='.'))
    png.url <- paste(htmlRep$baseUrl,
                     htmlRep$reportDirectory, figures.dirname, 
                     paste(filename, 'png', sep='.'), sep="/")
            
    cairo_pdf(filename = pdf.filename, height = pdf.height, width = pdf.width)
    print(object)
    dev.off()
    png(filename = png.filename, height = png.height, width= png.width)
    print(object)
    dev.off()
        
    img <- hwriteImage(png.url)
    hwrite(img, link=pdf.url, br=br)
  }


if(FALSE)
{
setMethod("objectToHTML",
          signature = signature(
        object = "GOHyperGResult"
      ),
    definition = function(object, report, selectedIDs,annotation.db,pvalueCutoff = 0.01, categorySize = 10, makePlot=FALSE, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.html2(object, report, selectedIDs, annotation.db,pvalueCutoff = pvalueCutoff, categorySize=categorySize, makePlot=makePlot)
       objectToHTML(df)
    }
)

}
          
if(FALSE)
{
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
}


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
        page.directory <- file.path(report$basePath, pages.dirname)
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



if(FALSE)
  {

setMethod("objectToHTML", signature = signature(
                            object = "DGEExact"),
          definition = function(object, rep, ...)
          {
            df = .DGEExact.to.html2(object, rep, ...)
            objectToHTML(df)
          }
          )
}


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
    colnames(ret)[which(colnames(ret) == 'dat.lfc.padj')] <- 'Adjusted p-Value'
    
    return(ret)
}


.GOhyperG.to.html2 <- function(object, htmlRep,selectedIDs,annotation.db,pvalueCutoff = 0.01, categorySize=10, makePlot=FALSE)
{    
	tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
	check.eg.ids(selectedIDs,annotation.db)

  	df<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
  	if(dim(df)[1]<1) {stop("No categories match your criteria.")}
  	df$GOID<-df[,1]
   	df$GOLink<-paste('<a href="http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=', df$GOID, '">', df$GOID, '</a>', sep="")
   	df$goName<-unlist(lapply(df$GOID, function(x) {strsplit(x, ":")[[1]][2]}))
   	
   	#pages.dirname <- paste0('GOPages', name(htmlRep))
        pages.dirname <- paste0('GOPages', htmlRep$shortName)  
   # page.directory <- file.path(basePath(htmlRep), 
    #    reportDirectory(htmlRep), pages.dirname)
        page.directory <- file.path(htmlRep$basePath,
                                    htmlRep$reportDirectory, pages.dirname)
    .safe.dir.create(page.directory)
    go.reportDirectory <- paste(htmlRep$reportDirectory, 
        pages.dirname, sep="/")
   	makeGeneListPages(object,reportDir=go.reportDirectory,  pvalueCutoff=pvalueCutoff,categorySize,selectedIDs, annotation.db, GO=TRUE, baseUrl=htmlRep$baseUrl, basePath=htmlRep$basePath)  
   	
   	df$CountLink<-paste('<a href="',pages.dirname, "/" ,df$goName, ".html",'">', df$Count, '</a>', sep="")
   	df$SizeLink<-paste('<a href="',pages.dirname, "/",df$goName, "All.html",'">', df$Size, '</a>', sep="")
 	ret<-data.frame(df$GOLink,df$Term,df$SizeLink,Image = rep("", nrow(df)), df$CountLink,signif(df$OddsRatio, 3), signif(df$Pvalue, 3),stringsAsFactors = FALSE)
 	colnames(ret)<-c("Accession", "GO Term","Category Size" ,"Image","Overlap", "Odds Ratio", "P-value" )
 
 	figure.dirname <- paste0('GOFigures', htmlRep$shortName)
    figure.directory <- file.path(htmlRep$basePath, htmlRep$reportDirectory, figure.dirname)
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
  		hyperGPlot(df$Size[i]-df$Count[i],numSelectedIDs-df$Count[i], df$Count[i], df$GOID[i], df$Term[i])
  		dev.off()
  		
  		pdf.filename <- paste(GONum, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        pdf(pdf.file)
  		hyperGPlot(df$Size[i]-df$Count[i],numSelectedIDs-df$Count[i], df$Count[i], df$GOID[i], df$Term[i])
        dev.off()
        
        ret$Image[i] <- hwriteImage(paste(figure.dirname,png.filename, sep="/"), link=paste(figure.dirname,pdf.filename, sep="/"), table=FALSE,width=100, height=100)
	}
        if(makePlot)
          #If we have the plot we need to mash it all together into HTML here because of how the dispatch is currently set up. We may want to change this in the future...
          paste("<div>",retplot, objectToHTML(ret), "</div>", sep="\n") 
        else
          ret
      }


if(FALSE)
  {
setMethod("objectToHTML",
    signature = signature(
        object = "MArrayLM"
      ),
    definition = function(object, publicationType, eSet, factor, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', 
        make.plots = TRUE, ..., .addColumns, .toDF){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .marrayLM.to.html2(object, publicationType, eSet, factor,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, make.plots = make.plots, ...)
        objectToHTML(df)
      }
          )
}

.marrayLM.to.html2 <- function(object, htmlRep, eSet = NULL, factor = NULL, 
    n = 1000, pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method='BH', 
    make.plots = !is.null(eSet), ...)
{
    
    dat <- topTable(object, number = n, p.value = pvalueCutoff, lfc = lfc,
        adjust.method = adjust.method, coef = coef, ...)
    if(is.null(coef))
        coef <- 1:ncol(object)
    
    if(length(coef) == 1){
        selection <- as.numeric(rownames(dat))
        if(length(selection) == 0)
            stop("No probes meet selection criteria. 
    Try changing the log-fold change or p-value cutoff.")
        padj <- apply(object$p.value, 2, p.adjust, method = adjust.method)
        padj <- padj[selection, coef]
        object <- object[selection, coef]
    } else {
        selection <- rownames(dat)
        if(length(selection) == 0)
            stop("No probes meet selection criteria. 
    Try changing the log-fold change or p-value cutoff.")
        padj <- dat$adj.P.Val
        object <- object[selection, coef]
    }
    
    
    fdata <- NULL
    if(!is.null(eSet)){
        eSet <- eSet[selection, ]
    
        ann.map.available <- tryCatch(getAnnMap("ENTREZID", annotation(eSet)), 
            error=function(e){ return(FALSE) })
        
        if(inherits(ann.map.available, "AnnDbBimap")){
            fdata <- data.frame(
                ProbeId = featureNames(eSet),
                EntrezId = unlist(mget(featureNames(eSet), 
                    getAnnMap("ENTREZID", annotation(eSet)))),
                Symbol = unlist(mget(featureNames(eSet), 
                    getAnnMap("SYMBOL", annotation(eSet)))),
                GeneName = unlist(mget(featureNames(eSet), 
                    getAnnMap("GENENAME", annotation(eSet)))),
                stringsAsFactors = FALSE
            )
        } else {
            if(ncol(fData(eSet)) > 0){
                fdata <- fData(eSet)
            } 
        }
    }
    if(is.null(fdata)){
        fdata <- object$genes
    }
    
    if("EntrezId" %in% colnames(fdata)){
        fdata$EntrezId <- hwrite(fdata$EntrezId, 
            link=paste("http://www.ncbi.nlm.nih.gov/gene/",
                fdata$EntrezId, sep=''), table=FALSE)
    }
    
    
    if(!make.plots){
        ret <- data.frame(
            fdata,
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        fc.cols <- (ncol(fdata)+1):(ncol(fdata)+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')

        pv.cols <- (ncol(fdata)+1+length(fc.cols)):ncol(ret)
        colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        
    } else {
        if(is.null(eSet) || is.null(factor)){
            stop("Can't make plots if either eSet or factor are not set.")
        }
        
        ret <- data.frame(
            fdata,
            Image = rep("", nrow(fdata)),
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        
        ret$Image <- rep("", nrow(fdata))
        
        figures.dirname <- paste('figures', htmlRep$shortName, sep='')  
        figure.directory <- file.path(htmlRep$basePath, 
            htmlRep$reportDirectory, figures.dirname)
        .safe.dir.create(figure.directory)
        
        .make.gene.plots(ret, eSet, factor, figure.directory)
        
        mini.image <- file.path(figures.dirname, 
            paste("mini", rownames(object), "png", sep="."))
        pdf.image <- file.path(figures.dirname, 
            paste("boxplot", rownames(object), "pdf", sep="."))
        ret$Image <- hwriteImage(mini.image, link=pdf.image, table=FALSE)
        
        fc.cols <- (ncol(fdata)+2):(ncol(fdata)+1+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')
        
        if(length(coef) == 1){
            pv.cols <- (ncol(fdata)+2+length(fc.cols)):ncol(ret)
            colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        } else {
            pv.col <- ncol(ret)
            colnames(ret)[pv.col] <- "Adjusted p-Value"
        }
        
    }
    return(ret)
}

if(FALSE)
{
setMethod("objectToHTML",
    signature = signature(
        object = "GeneSetCollection"
    ),
          definition = function(object, htmlRep, ...)
          objectToHTML(.GeneSetCollection.to.html2(object, htmlRep, ...))
          )
}

.GeneSetCollection.to.html2 <- function(object, htmlRep, annotation.db=NULL, 
    setStats=NULL, setPValues=NULL, geneStats = NULL)
{
    pages.dirname <- paste0('GeneSetCollectionPages', htmlRep$shortName)  
    page.directory <- file.path(htmlRep$basePath, 
        htmlRep$reportDirectory, pages.dirname)
    .safe.dir.create(page.directory)  
    gs.reportDirectory <- paste(htmlRep$reportDirectory, pages.dirname, sep="/")
    makeGeneListPagesGSC(object, reportDir=gs.reportDirectory, 
        annotation.db, geneStats = geneStats, baseUrl=htmlRep$baseUrl, 
        basePath=htmlRep$basePath)
   
    setLink <- paste('<a href="',pages.dirname,"/", names(object), 
        ".html",'">', names(object), '</a>', sep="")
    descriptions <- sapply(object, description)

    ret <- data.frame("GeneSet" = setLink, "Description" = descriptions,
        stringsAsFactors = FALSE)
    colnames(ret) <- c("Gene Set", "Description")

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

if(FALSE)
  {
setMethod("objectToHTML",
          signature = signature(object = "DGELRT"),
          definition = function(object, rep, ...)
           objectToHTML(.DGELRT.to.html2(object, rep, ...))
          )
}
.DGELRT.to.html2 <- function(object, htmlRep, countTable, conditions, 
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
    colnames(ret)[which(colnames(ret) == 'dat.lfc.padj')] <- 'Adjusted p-Value'
    
    return(ret)
}

