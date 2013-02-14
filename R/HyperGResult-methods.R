setMethod("publish",
    signature = signature(
        object = "HyperGResultBase",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, 
        pvalueCutoff = 0.01, categorySize = 10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .hyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff,
            categorySize = categorySize)
        publish(df, publicationType, ...)
    }
) 

setMethod("publish",
    signature = signature(
        object = "GOHyperGResult",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, 
        pvalueCutoff = 0.01, categorySize = 10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff,
            categorySize = categorySize)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "GOHyperGResult",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, selectedIDs,annotation.db,pvalueCutoff = 0.01, categorySize = 10, makePlot=FALSE, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.html(object,publicationType,selectedIDs, annotation.db,pvalueCutoff = pvalueCutoff, categorySize=categorySize, makePlot=makePlot)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
          signature = signature(
            object = "GOHyperGResult",
            publicationType = "HTMLReportRef"),
          definition = function(object, publicationType, ..., name)
          publicationType$addElement(name, value = object, ...)
          )



setMethod("publish",
    signature = signature(
        object = "PFAMHyperGResult",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, 
        pvalueCutoff = 0.01,categorySize=10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .PFAMhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff, categorySize)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "PFAMHyperGResult",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, selectedIDs,annotation.db,
        pvalueCutoff = 0.01,categorySize=10, ...){
        df <- .PFAMhyperG.to.html(object, publicationType,selectedIDs,annotation.db,pvalueCutoff = pvalueCutoff,categorySize )
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
          signature = signature(
            object = "PFAMHyperGResult",
            publicationType = "HTMLReportRef"),
          definition = function(object, publicationType, name,  ...)
          publicationType$addElement(name, value = object, ...)
          )


.hyperG.to.data.frame <- function(object, 
    pvalueCutoff = 0.01, 
    categorySize = 10)
{
    summary.tab <- summary(object, pvalue = pvalueCutoff, 
        categorySize = categorySize)
    return(summary.tab)
}

.GOhyperG.to.data.frame <- function(object,
    pvalueCutoff = 0.01,
    categorySize = 10)
{
  	summary.tab<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
  	summary.tab$GOID<-summary.tab[,1]
 	df<-data.frame(summary.tab$GOID,summary.tab$Term,signif(summary.tab$Pvalue, 3), signif(summary.tab$OddsRatio, 3), summary.tab$Count,summary.tab$Size)
 	colnames(df)<-c("Gene Ontology", "GO Term", "P-value", "Odds Ratio", "Count", "Ontology Size")
    return(df)                              
}


.GOhyperG.to.html <- function(object, htmlRep,selectedIDs,annotation.db,pvalueCutoff = 0.01, categorySize=10, makePlot=FALSE)
{    
	tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
	check.eg.ids(selectedIDs,annotation.db)

  	df<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
  	if(dim(df)[1]<1) {stop("No categories match your criteria.")}
  	df$GOID<-df[,1]
   	df$GOLink<-paste('<a href="http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=', df$GOID, '">', df$GOID, '</a>', sep="")
   	df$goName<-unlist(lapply(df$GOID, function(x) {strsplit(x, ":")[[1]][2]}))
   	
   	pages.dirname <- paste0('GOPages', name(htmlRep))  
    page.directory <- file.path(basePath(htmlRep), 
        reportDirectory(htmlRep), pages.dirname)
    .safe.dir.create(page.directory)
    go.reportDirectory <- paste(reportDirectory(htmlRep), 
        pages.dirname, sep="/")
   	makeGeneListPages(object,reportDir=go.reportDirectory,  pvalueCutoff=pvalueCutoff,categorySize,selectedIDs, annotation.db, GO=TRUE, baseUrl=baseUrl(htmlRep), basePath=basePath(htmlRep))  
   	
   	df$CountLink<-paste('<a href="',pages.dirname, "/" ,df$goName, ".html",'">', df$Count, '</a>', sep="")
   	df$SizeLink<-paste('<a href="',pages.dirname, "/",df$goName, "All.html",'">', df$Size, '</a>', sep="")
 	ret<-data.frame(df$GOLink,df$Term,df$SizeLink,Image = rep("", nrow(df)), df$CountLink,signif(df$OddsRatio, 3), signif(df$Pvalue, 3),stringsAsFactors = FALSE)
 	colnames(ret)<-c("Accession", "GO Term","Category Size" ,"Image","Overlap", "Odds Ratio", "P-value" )
 
 	figure.dirname <- paste0('GOFigures', name(htmlRep))
    figure.directory <- file.path(basePath(htmlRep), reportDirectory(htmlRep), figure.dirname)
    .safe.dir.create(figure.directory)
	if (makePlot==TRUE){
		plotGOResults(object,pvalueCutoff, categorySize, reportDir=figure.directory)
		hwrite(hwriteImage(paste(figure.dirname,"GOPlot.svg", sep="/"), link=paste(figure.dirname,"GOPlot.svg", sep="/"),width=400, height=400), page(htmlRep), br = TRUE)
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
    return(ret)
}

.PFAMhyperG.to.html <- function(object,htmlRep, selectedIDs,annotation.db, pvalueCutoff = 0.01, categorySize=10 )
{    
  	tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
	check.eg.ids(selectedIDs,annotation.db)

  	df<-summary(object, pvalue=pvalueCutoff, categorySize )
  	if(dim(df)[1]<1) {stop("No PFAMs match your criteria.")}

  	df$PFAMLink<-paste('<a href="http://pfam.sanger.ac.uk/family/', df$PFAMID, '">', df$PFAMID, '</a>', sep="")
  	pfamEnv <- getAnnMap("DE", "PFAM", load=TRUE)
  	df$PFAMDescription<-unlist(mget(df$PFAMID, pfamEnv, ifnotfound=NA))
  	
  	pages.dirname <- paste0('PFAMPages', name(htmlRep))  
    page.directory <- file.path(basePath(htmlRep), 
        reportDirectory(htmlRep), pages.dirname)
    .safe.dir.create(page.directory)
    pfam.reportDirectory <- paste(reportDirectory(htmlRep), 
        pages.dirname, sep="/")
    makeGeneListPages(object,reportDir=pfam.reportDirectory,  pvalueCutoff=pvalueCutoff,categorySize,selectedIDs, annotation.db, GO=FALSE, baseUrl=baseUrl(htmlRep), basePath=basePath(htmlRep))  

  	df$CountLink<-paste('<a href="',pages.dirname,"/", df$PFAMID, ".html",'">', df$Count, '</a>', sep="")
   	df$SizeLink<-paste('<a href="',pages.dirname,"/", df$PFAMID, "All.html",'">', df$Size, '</a>', sep="")
  	ret<-data.frame(df$PFAMLink,df$PFAMDescription,df$SizeLink,Image = rep("", nrow(df)),df$CountLink, signif(df$OddsRatio, 3), signif(df$Pvalue, 3), stringsAsFactors = FALSE)
 	colnames(ret)<-c("PFAM ID", "PFAM Term", "PFAM Size","Image","Overlap", "Odds Ratio", "P-value")

    figure.dirname <- paste0('PFAMFigures', name(htmlRep))
    figure.directory <- file.path(basePath(htmlRep), reportDirectory(htmlRep), figure.dirname)
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



.GOhyperG.to.data.frame <- function(object,
    pvalueCutoff = 0.01,
    categorySize = 10)
{
  	summary.tab<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
  	if(dim(summary.tab)[1]<1) {stop("No categories match your criteria.")}

  	summary.tab$GOID<-summary.tab[,1]
 	df<-data.frame(summary.tab$GOID,summary.tab$Term,signif(summary.tab$Pvalue, 3), signif(summary.tab$OddsRatio, 3), summary.tab$Count,summary.tab$Size)
 	colnames(df)<-c("Gene Ontology", "GO Term", "P-value", "Odds Ratio", "Count", "Ontology Size")
 	if(dim(df)[1]<1) {stop("No categories match your criteria.")}

    return(df)                              
}

.PFAMhyperG.to.data.frame <- function(object,
    pvalueCutoff = 0.01, categorySize=10 )
{    
  	summary.tab<-summary(object, pvalue=pvalueCutoff, categorySize)
  	if(dim(summary.tab)[1]<1) {stop("No PFAMs match your criteria.")}

  	pfamEnv <- getAnnMap("DE", "PFAM", load=TRUE)
  	summary.tab$PFAMName<-unlist(mget(summary.tab$PFAMID, pfamEnv, ifnotfound=NA))
  	df<-data.frame(summary.tab$PFAMID,summary.tab$PFAMName,signif(summary.tab$Pvalue, 3), signif(summary.tab$OddsRatio, 3), summary.tab$Count,df$Size)
 	colnames(df)<-c("PFAM ID", "PFAM Term", "P-value", "Odds Ratio", "Count", "PFAM Size")
    return(df)                              
}
