setMethod("modifyReportDF",
          signature = signature(
            object = "ANY"),
          definition=function(df, htmlRep, ...) df
          )

setMethod("modifyReportDF",
          signature = signature(
            object = "GOHyperGResult"),
          definition = function(df, htmlRep, object, annotation.db, selectedIDs, pvalueCutoff = 0.01, categorySize = 10,...)
          {
	tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
	check.eg.ids(selectedIDs,annotation.db)


  	if(dim(df)[1]<1) {stop("No categories match your criteria.")}
  	df$GOID<-df[,1]
   	df$GOLink<-paste('<a href="http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=', df$GOID, '">', df$GOID, '</a>', sep="")
   	df$goName<-unlist(lapply(df$GOID, function(x) {strsplit(x, ":")[[1]][2]}))
   	
        pages.dirname <- paste0('GOPages', htmlRep$shortName)  

        page.directory <- file.path(dirname(path(htmlRep)), 
            pages.dirname)
    .safe.dir.create(page.directory)
    go.reportDirectory <- paste(htmlRep$reportDirectory, 
        pages.dirname, sep="/")
   	makeGeneListPages(object,reportDir=go.reportDirectory,  pvalueCutoff=pvalueCutoff,categorySize,selectedIDs, annotation.db, GO=TRUE, basePath=htmlRep$basePath)  
   	
   	df$CountLink<-paste('<a href="',pages.dirname, "/" ,df$goName, ".html",'">', df$Count, '</a>', sep="")
   	df$SizeLink<-paste('<a href="',pages.dirname, "/",df$goName, "All.html",'">', df$Size, '</a>', sep="")
 	ret<-data.frame(df$GOLink,df$Term,df$SizeLink,Image = rep("", nrow(df)), df$CountLink,signif(df$OddsRatio, 3), signif(df$Pvalue, 3),stringsAsFactors = FALSE)
 	colnames(ret)<-c("Accession", "GO Term","Category Size" ,"Image","Overlap", "Odds Ratio", "P-value" )
 
 	figure.dirname <- paste0('GOFigures', htmlRep$shortName)
    figure.directory <- file.path(dirname(path(htmlRep)), figure.dirname)
    .safe.dir.create(figure.directory)
     #   if (makePlot==TRUE){
#		plotGOResults(object,pvalueCutoff, categorySize, reportDir=figure.directory)
#		plotret = hwrite(hwriteImage(paste(figure.dirname,"GOPlot.svg", sep="/"), link=paste(figure.dirname,"GOPlot.svg", sep="/"),width=400, height=400), br = TRUE)
#	}
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
#XXX We need to figure out another way to do this This logic doesn't belong in modifyReportDF
        
 #       if(makePlot)
          #If we have the plot we need to mash it all together into HTML here because of how the dispatch is currently set up. We may want to change this in the future...
  #        paste("<div>",retplot, objectToHTML(ret), "</div>", sep="\n") 
   #     else
          ret


          })

setMethod("modifyReportDF",
    signature = signature(
    object = "MArrayLM"),
    definition = function(df, htmlRep, object, eSet, factor, make.plots = TRUE, ...){
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        if(make.plots == TRUE){
            dots <- list(...)
            par.settings <- list()
            if("par.settings" %in% names(dots))
                par.settings <- dots$par.settings
            
            figure.dirname <- paste('figures', htmlRep$shortName, sep='')  
            figure.directory <- file.path(dirname(path(htmlRep)), 
                figure.dirname)
            .safe.dir.create(figure.directory)
            
            df <- eSetPlot(df, eSet, factor, figure.directory, figure.dirname, 
                par.settings = par.settings)
            df
        }
        df
    }
)

setMethod("modifyReportDF",
    signature = signature(
    object = "DGEExact"),
    definition = function(df, htmlRep, object, countTable, conditions, 
        make.plots = TRUE, ...){
        
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        if(make.plots == TRUE){
            dots <- list(...)
            par.settings <- list()
            if("par.settings" %in% names(dots))
                par.settings <- dots$par.settings
            
            figure.dirname <- paste('figures', htmlRep$shortName, sep='')  
            figure.directory <- file.path(dirname(path(htmlRep)), 
                figure.dirname)
            .safe.dir.create(figure.directory)
            
            df <- eSetPlot(df, countTable+1, conditions, figure.directory,
                figure.dirname, scales = list(y = list(log = 10)),
                ylab.type = "Normalized Counts", par.settings = par.settings)
            df
        }
        df
        
    }
)

setMethod("modifyReportDF",
    signature = signature(object = "GeneSet"),
    definition = function(df, htmlRep, object, ...){
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        df
    }
)

setMethod("modifyReportDF",
    signature = signature(
    object = "DGELRT"),
    definition = function(df, htmlRep, object, countTable, conditions, 
        make.plots = TRUE, ...){
        
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        if(make.plots == TRUE){
            dots <- list(...)
            par.settings <- list()
            if("par.settings" %in% names(dots))
                par.settings <- dots$par.settings
            
            figure.dirname <- paste('figures', htmlRep$shortName, sep='')  
            figure.directory <- file.path(dirname(path(htmlRep)), figure.dirname)
            .safe.dir.create(figure.directory)
            
            df <- eSetPlot(df, countTable+1, conditions, figure.directory,
                figure.dirname, scales = list(y = list(log = 10)), 
                ylab.type = "Normalized Counts", par.settings = par.settings)
            df
        }
        df
        
    }
)

setMethod("modifyReportDF",
    signature = signature(
    object = "DESeqResults"),
    definition = function(df, htmlRep, object, DataSet, factor, 
        make.plots = TRUE, ...)
    {
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        if(make.plots){
            dots <- list(...)
            par.settings <- list()
            if("par.settings" %in% names(dots))
                par.settings <- dots$par.settings
                
            figure.dirname <- paste('figures', htmlRep$shortName, sep='')  
            figure.directory <- file.path(dirname(path(htmlRep)), 
                figure.dirname)
            .safe.dir.create(figure.directory)
            
            df <- eSetPlot(df, DataSet, factor, figure.directory,
                figure.dirname, par.settings = par.settings, 
                ylab.type = "Normalized Counts")
            df
        }
        df
    }
)

setMethod("modifyReportDF",
    signature = signature(
    object = "DESeqDataSet"),
    definition = function(df, htmlRep, object, factor, make.plots = TRUE, ...)
    {
        if("EntrezId" %in% colnames(df)){
            df <- entrezGene.link(df)
        }
        if(make.plots){
            dots <- list(...)
            par.settings <- list()
            if("par.settings" %in% names(dots))
                par.settings <- dots$par.settings
                
            figure.dirname <- paste('figures', htmlRep$shortName, sep='')  
            figure.directory <- file.path(dirname(path(htmlRep)), 
                figure.dirname)
            .safe.dir.create(figure.directory)
            
            df <- eSetPlot(df, object, factor, figure.directory, figure.dirname,
                par.settings = par.settings, ylab.type = "Normalized Counts")
            df
        }
        df
    }
)

entrezGene.link <- function(df, ...)
{
    df$EntrezId <- hwrite(df$EntrezId, 
        link = paste0("http://www.ncbi.nlm.nih.gov/gene/", df$EntrezId),
        table = FALSE)
    df
}

eSetPlot <- function(df, eSet, factor, figure.directory, figure.dirname, ...)
{
    .make.gene.plots(df, eSet, factor, figure.directory, ...)
    mini.image <- file.path(figure.dirname, 
        paste("mini", rownames(df), "png", sep="."))
    pdf.image <- file.path(figure.dirname, 
        paste("boxplot", rownames(df), "pdf", sep="."))
    df$Image <- hwriteImage(mini.image, link=pdf.image, table=FALSE)
    df
}
