makeDESeqDf<-function(res,countTable,pvalueCutoff,conditions,annotation.db, expName,reportDir){
	sigGenes<-which(res$padj<pvalueCutoff)
	if(length(sigGenes) < 1)
        {stop("No genes meet the selection criteria. Try changing the p-value cutoff.")}
	resSig<-res[sigGenes,]
	countTableSig<-countTable[sigGenes,]
	eids<-resSig$id
	check.eg.ids(eids, annotation.db)
	eidsLink<-hwrite(as.character(eids), 
    	link=paste("http://www.ncbi.nlm.nih.gov/gene/", as.character(eids), sep=''), table=FALSE)
	gnamesAndSymbols<-getNamesAndSymbols(eids, annotation.db)
	imageFiles<-makeDESeqFigures(countTableSig, conditions, gnamesAndSymbols$symbol, expName,reportDir)
    images <- hwriteImage(imageFiles[[1]], link=imageFiles[[2]], table=FALSE)
	ret<-data.frame(eidsLink, gnamesAndSymbols$symbol, gnamesAndSymbols$name, images,resSig$log2FoldChange,resSig$pval,resSig$padj )
	colnames(ret)<-c("Entrez Id", "Symbol", "Gene Name", "Image", "Log2 Fold Change", "P-value","Adjusted p-value")
	return(ret)
}



