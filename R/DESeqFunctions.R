makeDESeqDF<-function(object, countTable, pvalueCutoff, conditions,
    annotation.db, expName, reportDir, ...)
{
	figures.dirname <- paste0(reportDir,'/figures')  
    figure.directory <- file.path(figures.dirname)
    .safe.dir.create(figure.directory)
        
	sigGenes <- which(object$padj < pvalueCutoff)
	
	if(length(sigGenes) < 1)
        {stop("No genes meet the selection criteria. Try changing the p-value cutoff.")}
    
	resSig <- object[sigGenes,]
	countTableSig <- countTable[sigGenes,]
	eids <- resSig$id
	eidsLink <- hwrite(as.character(eids), 
    	link=paste("http://www.ncbi.nlm.nih.gov/gene/", as.character(eids), sep=''), table=FALSE)
	check.eg.ids(eids, annotation.db)
	
	gnamesAndSymbols <- getNamesAndSymbols(eids, annotation.db)
	
	imageFiles <- makeDESeqFigures(countTableSig, conditions,
	    gnamesAndSymbols$symbol, expName, reportDir)
    images <- hwriteImage(imageFiles[[1]], link=imageFiles[[2]], table=FALSE)
	ret <- data.frame(
	    eidsLink, 
	    gnamesAndSymbols$symbol, 
	    gnamesAndSymbols$name,
	    images,
	    resSig$log2FoldChange,
	    resSig$pval,
	    resSig$padj )
	colnames(ret) <- c("Entrez Id", "Symbol", "Gene Name", "Image", 
	    "Log2 Fold Change", "P-value","Adjusted p-value")
	return(ret)
}