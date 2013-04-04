makeGeneListPages<-function(hg, reportDir, pvalueCutoff=0.01, categorySize=10, 
    selectedIDs, annotation.db, GO, basePath=""){
    df<-summary(hg, pvalue=pvalueCutoff, categorySize)
        num<-dim(df)[1]
    if (length(num)<1) {stop("No ontologies match your criteria")}
    geneIDs<-geneIdsByCategory(hg)
    geneIDsU<-geneIdUniverse(hg)
    for (i in 1:num){
        Oname<-df[i,1]
        locOfSet<-which(names(geneIDs)==Oname)
        entrez<-unique(geneIDs[[locOfSet]])
        countTable<-getNamesAndSymbols(entrez, annotation.db)
        countTable$entrezLink<-paste('<a href="http://www.ncbi.nlm.nih.gov/gene/', countTable$entrez, '">', countTable$entrez, '</a>', sep="")
        table_title<-paste("Included genes in ",Oname, sep="")   
        GeneTable<-data.frame(countTable$entrezLink,countTable$symbol,countTable$name)
        colnames(GeneTable)<-c("GeneEntrezId", "GeneSymbol","GeneName")
        if (GO==TRUE) {pageFile<-as.character(strsplit(Oname, ":")[[1]][2])}
        if (GO==FALSE){pageFile<-as.character(Oname)}
        Report <- HTMLReport(shortName = pageFile, title = table_title, reportDirectory = reportDir, basePath=basePath)
        publish(GeneTable, Report)
        finish(Report)

        #for the genes in the universe
        locOfSet<-which(names(geneIDsU)==Oname)
        entrez<-unique(geneIDsU[[locOfSet]])
        sizeTable<-getNamesAndSymbols(entrez, annotation.db)
        sizeTable$entrezLink<-paste('<a href="http://www.ncbi.nlm.nih.gov/gene/', sizeTable$entrez, '">', sizeTable$entrez, '</a>', sep="")
        table_title<-paste("All genes in ",Oname, sep="")   
        GeneUTable<-data.frame(sizeTable$entrezLink,sizeTable$symbol,sizeTable$name)
        colnames(GeneUTable)<-c("GeneEntrezId", "GeneSymbol","GeneName")
        if (GO==TRUE) {pageFile<-paste0(as.character(strsplit(Oname, ":")[[1]][2]), "All")}
        if (GO==FALSE){pageFile<-paste0(as.character(Oname), "All")}      
  		Report <- HTMLReport(shortName = pageFile, title = table_title, reportDirectory = reportDir, basePath=basePath)
        publish(GeneUTable, Report)
        finish(Report)
    }
}


makeGeneListPagesGSC <- function(geneSets, reportDir, annotation.db,
    geneStats = NULL, basePath="", .setToHTML = NULL, .setToDF = NULL, 
    .modifySetDF = NULL)
{
	ann.map <- NULL
	if(!is.null(annotation.db)) {
		tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
		ann.map <- getAnnMap("SYMBOL", annotation.db)
	} 
	
	numSets <- length(geneSets)
    for (i in 1:numSets){
        
        setName <- names(geneSets)[i]
        setName<-gsub(":", "", setName)
        setName<-gsub(" ","", setName)
   		setName<-gsub(";","", setName)
        description <- description(geneSets[[i]])
        entrez <- unlist(geneIds(geneSets[[i]]))
        setTable <- data.frame(GeneName=entrez)
        
        table_title <- paste("Genes in ", setName, " -- ", description , sep="")
        Report <- HTMLReport(shortName = setName, title = table_title, 
            reportDirectory = reportDir, basePath=basePath)
        publish(geneSets[[i]], Report, annotation.db = annotation.db, 
            geneStats = geneStats, .toHTML = .setToHTML, .toDF = .setToDF,
            .modifyDF = .modifySetDF)
        finish(Report)
    }
}


getNamesAndSymbols <- function(entrez, annotation.db){
    
    geneids <- entrez
    
    ## If the annotation.db has a mapping to Entrez Ids, then
    ## asking for the map returns the usual AnnDbBimap, but if it is
    ## an organism package, then asking for the Entrez map returns an object
    ## of class AnnotationDbMap. There is at least one case where getAnnMap 
    ## will fail for ENTREZID, but will succeed for SYMBOL and GENENAME: where
    ## the annotation.db is "org.Hs.eg", there is no object returned, and an 
    ## error is generated.  When the annotation.db is "org.Hs.eg.db", an
    ## object is correctly returned.
    entrezmap <- NULL
    tryCatch(entrezmap <- getAnnMap("ENTREZID", annotation.db), 
        error = function(e) {})
    if(is(entrezmap, "AnnDbBimap")){
        entrez <- unlist(mget(geneids, entrezmap, ifnotfound = NA))
    } 
    
    symbol <- unlist(mget(geneids, getAnnMap("SYMBOL", annotation.db),
        ifnotfound=NA))
    name <- unlist(mget(geneids, getAnnMap("GENENAME", annotation.db),
        ifnotfound=NA))
    countTable <- list(entrez=entrez, symbol=symbol, name=name)
    if(is(entrezmap, "AnnDbBimap")){
        countTable$geneids <- geneids
    }
    
    return(countTable)
}
