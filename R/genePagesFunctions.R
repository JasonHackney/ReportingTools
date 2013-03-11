makeGeneListPages<-function(hg, reportDir, pvalueCutoff=0.01, categorySize=10, 
    selectedIDs, annotation.db, GO, baseUrl = "", basePath=""){
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
        Report <- HTMLReport(shortName = pageFile, title = table_title, reportDirectory = reportDir, baseUrl = baseUrl, basePath=basePath)
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
  		Report <- HTMLReport(shortName = pageFile, title = table_title, reportDirectory = reportDir, baseUrl = baseUrl, basePath=basePath)
        publish(GeneUTable, Report)
        finish(Report)
    }
}


makeGeneListPagesGSC <- function(geneSets, reportDir, annotation.db,
    geneStats = NULL, baseUrl = "", basePath="")
{
	ann.map <- NULL
	if(!is.null(annotation.db)) {
		tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
		{stop(paste0("Unable to find your annotation.db: ",annotation.db))})
		ann.map <- getAnnMap("SYMBOL", annotation.db)
	} #else {
		#annotation.db <- annotation(geneIdType(geneSets[[1]]))
		#if(annotation.db == '') {
	#		annotation.db <- NULL
	#	} else {
	#		ann.map <- getAnnMap("SYMBOL", annotation.db)
	#	}
	#}
    numSets <- length(geneSets)
    for (i in 1:numSets){
        setName <- names(geneSets)[i]
        setName<-gsub(":", "", setName)
        description <- description(geneSets[[i]])
        entrez <- unlist(geneIds(geneSets[[i]]))
        setTable <- data.frame(GeneName=entrez)
        if(!is.null(annotation.db)) { 
        	check <- unlist(mget(entrez, ann.map, ifnotfound=NA))
			if ((sum(is.na(check)) < length(entrez)/2)==TRUE){
				setTable <- getNamesAndSymbols(entrez, annotation.db)
        		setTable$entrezLink <- paste('<a href="http://www.ncbi.nlm.nih.gov/gene/', 
            		setTable$entrez, '">', setTable$entrez, '</a>', sep="")  
        		setTable <- data.frame(EntrezId=setTable$entrezLink, 
            		Symbol=setTable$symbol, GeneName=setTable$name)
            }
        }
        if(!is.null(geneStats)){
            setTable$Gene.Statistic <- geneStats[entrez]
        }
        table_title <- paste("Genes in ", setName, " -- ", description , sep="")
        Report <- HTMLReport(shortName = setName, title = table_title, 
            reportDirectory = reportDir, baseUrl = baseUrl, basePath=basePath)
        publish(setTable, Report)
        finish(Report)
    }
}


getNamesAndSymbols<-function(entrez, annotation.db){
    symbol <- unlist(mget(entrez, getAnnMap("SYMBOL", annotation.db), ifnotfound=NA))
    name <- unlist(mget(entrez, getAnnMap("GENENAME", annotation.db), ifnotfound=NA))
    countTable <- list(entrez=entrez, symbol=symbol, name=name)
    return(countTable)
}
