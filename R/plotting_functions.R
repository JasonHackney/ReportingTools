.remove.axis.and.padding <- function (plot)
{
    plot$par.settings$layout.heights <- list(
        top.padding = 0,
        main.key.padding = 0,
        key.axis.padding = 0,
        axis.xlab.padding = 0,
        xlab.key.padding = 0,
        key.sub.padding = 0,
        bottom.padding = 0)
    plot$par.settings$layout.widths <- list(
        left.padding = 0,
        key.ylab.padding = 0,
        ylab.axis.padding = 0,
        axis.key.padding = 0,
        right.padding = 0)
    plot$x.scales$draw <- FALSE
    plot$y.scales$draw <- FALSE
    plot$xlab <- NULL
    plot$ylab <- NULL
    return(plot)
}

.make.gene.plots <- function(df, expression.dat, factor, figure.directory,
    ylab.type = "Expression Value", scales = list(), par.settings = list(),
    xlab = NULL, ...){
        
    scales <- c(scales, list(x = list(rot = 45)))
    
    if(is(expression.dat, "CountDataSet")){
        ## Get the normalized counts, but add a pseudocount to all of the
        ## entries, because we're going to plot on a log-scale
        
        expression.dat <- counts(expression.dat, normalized=TRUE)+1
        scales <- c(scales, list(y = list(log = 10)))
    } else if(inherits(expression.dat, "eSet")){
    
        expression.dat <- exprs(expression.dat)
        
    } else if(is(expression.dat, "DGEList")){
        ## If we get a DGEList, get the normalized cpm value. Again, we add a
        ## pseudocount to avoid problems with zero count data
        
        expression.dat <- cpm(expression.dat$counts) + 1
    } else if(is(expression.dat, "data.frame")){
        ## If it's a data.frame, try to coerce it to a matrix,
        ## but this might not work.
        
        expression.dat <- as.matrix(expression.dat)
    } else if(is(expression.dat, "DESeqDataSet")){
        expression.dat <- counts(expression.dat, normalized = TRUE) + 1
        scales <- c(scales, list(y = list(log = 10)))
    }
    
    
    for(probe in rownames(df)){
        if("Symbol" %in% colnames(df)){
            ylab <- paste(df[probe, 'Symbol'], ylab.type)
        } else {
            ylab <- paste(probe, ylab.type)
        }
        bigplot <- stripplot(expression.dat[ probe, ] ~ factor,
            panel = panel.boxandstrip, groups = factor, ylab = ylab,
            scales = scales, par.settings = par.settings, xlab = xlab)
        
        miniplot <- .remove.axis.and.padding(bigplot)
        minipng.filename <- paste("mini", probe ,"png", sep='.')
        minipng.file <- file.path(figure.directory, minipng.filename)
        
        png(minipng.file, height=40, width=200)
        grid.newpage()
        pushViewport(viewport(angle = 270, height = unit(220, 'points'), 
            width = unit(44, 'points'), name = "VP"))
        print(miniplot, newpage = FALSE)
        upViewport()
        dev.off()
        
        pdf.filename <- paste("boxplot", probe, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        
        pdf(pdf.file, height=4.5, width=4.5)
        print(bigplot)
        dev.off()
    }
}

miniplot <- function (x, data = NULL, panel = panel.boxandstrip, 
    scales=list(draw=FALSE), xlab=NULL, ylab=NULL, horizontal=TRUE, 
    par.settings = list(), ...)
{
    par.settings$layout.heights <- list(
        top.padding = 0,
        main.key.padding = 0,
        key.axis.padding = 0,
        axis.xlab.padding = 0,
        xlab.key.padding = 0,
        key.sub.padding = 0,
        bottom.padding = 0)
    par.settings$layout.widths <- list(
        left.padding = 0,
        key.ylab.padding = 0,
        ylab.axis.padding = 0,
        axis.key.padding = 0,
        right.padding = 0)
    
    args <- list(...)
    do.call("stripplot", c(list(x=x, data=data, panel = panel, 
            par.settings = par.settings, scales = scales, xlab = xlab, 
            ylab = ylab, horizontal=horizontal), args))
}

panel.boxandstrip <- function (x, y, jitter.data = T, do.out = FALSE, ...) 
{
    panel.bwplot(x, y, pch = "|", cex = 5, do.out = do.out, ...)
    panel.stripplot(x, y, jitter.data = jitter.data, ...)
}

reporting.theme <- function()
{
    op <- standard.theme()
    rib.palette <- colorRampPalette(c("darkblue", "ivory", "darkred"))
    op$background$col <- "transparent"
    op$regions$col <- rib.palette(128)
    op$plot.symbol$col <- "darkgrey"
    op$plot.symbol$pch <- 21
    op$plot.symbol$fill <- "#33333355"
    op$plot.line$col <- "black"
    op$superpose.symbol$col <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", 
        "#386CB0", "#F0027F", "#BF5B17", "#666666")
    op$superpose.symbol$pch <- 21
    op$superpose.symbol$fill <- c("#7FC97F55", "#BEAED455", "#FDC08655", 
        "#FFFF9955", "#386CB055", "#F0027F55", "#BF5B1755", "#66666655")
    op$superpose.polygon$col <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", 
        "#386CB0", "#F0027F", "#BF5B17", "#666666")
    op$superpose.line$col <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", 
        "#386CB0", "#F0027F", "#BF5B17", "#666666")
    op$strip.background$col <- "white"
    op$box.umbrella$col <- "black"
    op$box.umbrella$lty <- 1
    op$box.rectangle$col <- "black"
    op$box.dot$pch <- "|"
    op$dot.symbol$col <- "black"
    op$plot.polygon$col <- "lightgrey"
    invisible(op)
}

reporting.theme.alternate <- function()
{
    cols <- c("#FFD700","#FF0000", "#0000FF", "#006400","#FF00FF" ,"#000000", "#FFA500", "#BEBEBE")
    colsAlpha = sapply(cols, function(x) {paste(x,"75",sep="")})
    op <- standard.theme()
    rib.palette <- colorRampPalette(c("darkblue", "ivory", "darkred"))
    op$background$col <- "transparent"
    op$regions$col <- rib.palette(128)
    op$plot.symbol$col <- "darkgrey"
    op$plot.symbol$pch <- 21
    op$plot.symbol$fill <- "#33333355"
    op$plot.line$col <- "black"
    op$superpose.symbol$col <- "grey40"
    op$superpose.symbol$pch <- 21
    op$superpose.symbol$cex <- 1.25
    op$superpose.symbol$fill <- colsAlpha
    op$superpose.polygon$col <- "grey40"
    op$superpose.line$col <- cols
    op$strip.background$col <- "white"
    op$box.umbrella$col <- "black"
    op$box.umbrella$lty <- 1
    op$box.rectangle$col <- "black"
    op$box.dot$pch <- "|"
    op$dot.symbol$col <- "black"
    op$plot.polygon$col <- "lightgrey"
    invisible(op)
}



plotGOResults<-function(hgGO,pvalueCutoff=0.01,categorySize=10,reportDir){
    hgGOdf<-summary(hgGO, pvalue=pvalueCutoff)
    GOids<-hgGOdf[,1]
    g2<-inducedTermGraph(hgGO,id=GOids, children=FALSE, parents=TRUE)
    svg(filename=file.path(reportDir,"GOPlot.svg"), height=25, width=25)
    plotGOTermGraph(g2,  hgGO,node.colors=c(sig="peachpuff", not="white"), add.counts=TRUE, max.nchar=20, node.shape="ellipse")
    dev.off()
}


hyperGPlot<-function(nInGroup1,nInGroup2,nInBothGroups, ontologyNum, ontologyName){
	totalElements<-nInGroup1+nInGroup2+nInBothGroups
	x1<-nInGroup1/totalElements
	x2<-nInBothGroups/totalElements+x1

	plot(c(0,1), c(0,1.35), type="n", xlab="", ylab="", xaxt='n', yaxt='n',ann=FALSE)
	rect(xleft=0, ybottom=0,xright=x1,ytop=1, col="white")  
	rect(xleft=x1, ybottom=0,xright=x2,ytop=1, col="mediumvioletred") 
	rect(xleft=x2, ybottom=0,xright=1,ytop=1, col="gray65")
	text(x1/2,1/2, as.character(nInGroup1), font=2)
	text((x1+x2)/2,1/2, as.character(nInBothGroups), font=2, col="white")
	text((1+x2)/2,1/2, as.character(nInGroup2), font=2)
	if (nchar(ontologyName)>65){
		ontologyNameSplit<-unlist(strsplit(ontologyName, " "))
		name1<-paste(ontologyNameSplit[1:5], collapse=" ")
		name2<-paste(ontologyNameSplit[6:length(ontologyNameSplit)], collapse=" ")
		ontologyName<-paste(name1, name2, sep="\n")
	}
	rect(xleft=0, ybottom=1.22,xright=.05,ytop=1.27, col="white")
	text(.055,1.245, paste0("Genes in ", ontologyNum, ",\n", ontologyName), col="black", pos=4)
	rect(xleft=0, ybottom=1.12,xright=.05,ytop=1.17, col="mediumvioletred")
	text(.055,1.145, "Overlap ", col="black",pos=4)
	rect(xleft=0, ybottom=1.02,xright=.05,ytop=1.07, col="gray65")
	text(.055,1.045, "Selected Genes ", col="black", pos=4)	
}


makeDESeqFigures<-function(countTable, conditions,symbols,expName, reportDir){
	figures.dirname <- paste(reportDir,'figures', sep='')  
    figure.directory <- file.path(figures.dirname)
    .safe.dir.create(figure.directory)
	miniFiles<-c()
	pdfFiles<-c()
	countTable <- log2(countTable + 1)
	for(i in 1:length(symbols)){
		gene<-as.character(symbols[i])
		if (is.null(gene)==TRUE) {gene<-"NoSymbol"}
        ylab<-"log2 Counts"
        bigplot <- stripplot(as.numeric(countTable[i, ]) ~ conditions,
            panel=panel.boxandstrip, groups=conditions, ylab=ylab, main=gene,scales=list(x=list(rot=45)))
       
      	miniplot <- .remove.axis.and.padding(bigplot)
       	minipng.filename <- paste(expName,"mini", gene ,"png", sep='.')
        minipng.file <- paste0(reportDir, "/figures/", minipng.filename)
        if (!file.exists(minipng.file)) {
        	png(minipng.file, height=40, width=200)
        	grid.newpage()
        	pushViewport(viewport(angle = 270, height = unit(220, 'points'), 
            	width = unit(44, 'points'), name = "VP"))
        	print(miniplot, newpage = FALSE)
        	upViewport()
        	dev.off()
        }
        miniFiles[i]<-paste0("figures/", minipng.filename)
        
        pdf.filename <- paste(expName,"boxplot", gene, "pdf", sep=".")
        pdf.file <- paste0(reportDir, "/figures/", pdf.filename)
        if (!file.exists(pdf.file)) {
        	pdf(pdf.file, height=4.5, width=4.5)
        	print(bigplot)
        	dev.off()
        }
        pdfFiles[i]<-paste0("figures/", pdf.filename)
    }
	return(list(miniFiles, pdfFiles))
}
