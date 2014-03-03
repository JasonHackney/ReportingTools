\name{publish-methods}
\docType{methods}
\alias{publish-methods}
\alias{publish}
\alias{publish,data.frame,HTMLReport-method}
\alias{publish,MArrayLM,HTMLReport-method}
\alias{publish,DGEExact,HTMLReport-method}
\alias{publish,DGELRT,HTMLReport-method}
\alias{publish,HyperGResultBase,HTMLReport-method}
\alias{publish,PFAMHyperGResult,HTMLReport-method}
\alias{publish,GOHyperGResult,HTMLReport-method}
\alias{publish,GeneSetCollection,HTMLReport-method}
\alias{publish,GeneSet,HTMLReport-method}
\alias{publish,HTMLReport,HTMLReport-method}
\alias{publish,trellis,HTMLReport-method}
\alias{publish,list,HTMLReport-method}
\alias{publish,ANY,HTMLReport-method}


\alias{publish,data.frame,HTMLReportRef-method}
\alias{publish,MArrayLM,HTMLReportRef-method}
\alias{publish,DGEExact,HTMLReportRef-method}
\alias{publish,DGELRT,HTMLReportRef-method}
\alias{publish,DESeqDataSet,HTMLReportRef-method}
\alias{publish,DESeqResults,HTMLReportRef-method}
\alias{publish,HyperGResultBase,HTMLReportRef-method}
\alias{publish,PFAMHyperGResult,HTMLReportRef-method}
\alias{publish,GOHyperGResult,HTMLReportRef-method}
\alias{publish,GeneSetCollection,HTMLReportRef-method}
\alias{publish,HTMLReport,HTMLReportRef-method}
\alias{publish,trellis,HTMLReportRef-method}
\alias{publish,list,HTMLReportRef-method}


\alias{publish,data.frame,DataPackage-method}
\alias{publish,character,DataPackage-method}
\alias{publish,ANY,DataPackage-method}

\alias{publish,data.frame,CSVFile-method}

\alias{publish,DGEExact,ANY-method}
\alias{publish,DGELRT,ANY-method}
\alias{publish,GOHyperGResult,ANY-method}
\alias{publish,MArrayLM,ANY-method}
\alias{publish,PFAMHyperGResult,ANY-method}
\alias{publish,GeneSetCollection,ANY-method}

\alias{publish,ANY,list-method}

\title{ 
  Methods for publishing a variety of data types in selected output formats 
}
\description{
  These are a series of methods for taking various data types and coercing them into selected output formats. 
}
\section{Methods}{
\describe{

  Data types can be output in several formats. Exactly what is done to coerce a
  data type into the given output format is described below.

  \item{HTMLReport or HTMLReportRef\cr}{
        
        data.frame:\cr
        \code{publish(object, htmlReport, tableTitle = NULL, ...)}
        The most basic object to publish to an HTMLReport is a data.frame. Most
        other methods involve coercing their objects to a data.frame and then
        calling publish on that data.frame. As such, this is where all styling
        for publishing tables can be centrally controlled. If tableTitle is
        specified, the title is printed above the table.
    
        MArrayLM:\cr
        \code{publish(object, htmlReport, eSet, factor, n = 1000,
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method='BH',
        make.plots = TRUE, ...)}
        An MArrayLM object is coerced to a data.frame using a method similar to
        the topTable function from the limma package. The resulting table
        includes some selected feature data, and the log fold change and
        adjusted p-value from the linear model. The p-value adjustment method is
        set using the ``adjust.method'' argument. A glyph showing expression
        levels of each gene are also optionally plotted, based on the expression
        values from the ExpressionSet given in eSet, and the levels set in
        factor.
      
        DESeqDataSet:\cr
        \code{publish(object, publicationType, factor = NULL, n = 1000,
        pvalueCutoff = 0.01, lfc = 0, contrast = NULL, resultName = NULL, 
        make.plots = TRUE, ..., name)}
        To coerce a \code{\linkS4class{DESeqDataSet}} to a \code{data.frame},
        we use the \code{\link{results}} function from the \code{DESeq2}
        package, therefore \code{\link{DESeq}}, or something similar must be
        run prior to \code{publish}. \code{contrast} and \code{resultName} are
        passed to the \code{\link{results}} function as the \code{contrast}
        and \code{name} parameters, please consult the documentation for
        \code{\link{results}} to see how these are specified. If present,
        \code{annotation.db} is used to find gene-level annotations for the
        rows in the \code{DESeqDataSet}. Stripplots showing the expression
        values for each transcript are produced based on the normalized counts
        from the \code{\linkS4class{DESeqDataSet}}, grouped by the levels in
        \code{factor}.

        DESeqResults:\cr
        \code{publish(object, publicationType, DataSet = NULL, 
        annotation.db = NULL, n = 500, pvalueCutoff = 0.01, lfc = 0, 
        make.plots = TRUE, ..., name)}
        To coerce a \code{\linkS4class{DESeqResults}} object to a
        \code{data.frame}, we filter the results \code{DataFrame} such that
        the absolute  log fold changes is greater than \code{lfc} and the
        adjusted p-value is less than \code{pvalueCutoff}. If present,
        \code{annotation.db} is used to find gene-level annotations for the
        rows in the \code{DESeqResults}. If \code{make.plots} is TRUE,
        stripplots showing the expression values for each transcript are
        produced based on the normalized counts from the \code{DataSet}, which
        should be of class \code{\linkS4class{DESeqDataSet}}, grouped by the
        levels in \code{factor}.
        
        DGEExact:\cr
        \code{publish(object, htmlReport, countTable, conditions, 
        annotation.db = "org.Hs.eg", n = 1000, pvalueCutoff = 0.01, lfc = 0, 
        adjust.method = "BH", make.plots = TRUE, ...)}
        DGEExact objects are coerced to a data.frame using the topTags function
        from the edgeR package and filtered base on pvalueCutoff and lfc. The
        resulting table includes feature data, derived from the annotation
        package defined in annotation.db, along with the fold change and
        adjusted p-value. For this to work, the feature names in the DGEExact
        object and the countTable have to be the primary identifier from the
        annotation packge. In most cases, the primary identifier will be Entrez
        Gene IDs. If no valid annotation package name is provided in
        annotation.db, then feature data will come from the ``genes'' slot in
        the DGEExact object. The counts stored in countTable should be in units
        of counts per million provided by the cpm function in the edgeR package.
        The p-value adjustment method can be set using the ``adjust.method''
        argument.

        DGELRT:\cr
        \code{publish(object, htmlReport, countTable, conditions, 
        annotation.db = "org.Hs.eg", n = 1000, pvalueCutoff = 0.01, lfc = 0, 
        adjust.method = "BH", make.plots = TRUE, ...)}
        DGELRT objects are coerced to a data.frame using the topTags function
        from the edgeR package and filtered base on pvalueCutoff and lfc. The
        resulting table includes feature data, derived from the annotation
        package defined in annotation.db, along with the fold change and
        adjusted p-value. For this to work, the feature names in the DGELRT
        object and the countTable have to be the primary identifier from the
        annotation packge. In most cases, the primary identifier will be Entrez
        Gene IDs. If no valid annotation package name is provided in
        annotation.db, then feature data will come from the ``genes'' slot in
        the DGEExact object. The counts stored in countTable should be in units
        of counts per million provided by the cpm function in the edgeR package.
        The p-value adjustment method can be set using the ``adjust.method''
        argument.
      
        HyperGResultBase, GOHyperGResult, PFAMHyperGResult:\cr
        \code{publish(object, htmlReport, selectedIDs, annotation.db, 
        pvalueCutoff = 0.01,categorySize=10, makePlot=FALSE,...)}
        A HyperGResult object is coerced to a data.frame using a method similar
        to the summary function from the Category package. The resulting table
        includes for each classification the id, name, odds ratio of enrichment
        and p-value from the hypergeometric test. A glyph showing the level of
        overlap of each classification with the selected genes is also plotted.
        The number of genes found in this classification is listed in the table
        and links to another page with a table of the corresponding genes,
        symbols and names.  An additional page listing the overlap genes is also
        linked to the main output. For a GOHyperGResult object, a plot depicting
        the relationship between the significant ontologies and their parents is
        also plotted if makePlot is TRUE.  The selectedIDs are the Entrez ids of
        the genes of interest (i.e. from the gene universe); annotation.db is
        the species of the ids.
      
        GeneSetCollection:\cr
        \code{publish(object, htmlReport, annotation.db, 
        setStats=NULL, setPValues=NULL, geneStats = NULL, ..., 
        .setToHTML = NULL, .setToDF = NULL, .modifySetDF = NULL)}
        A GeneSetCollection object is coerced to a data.frame. If setStats
        and/or setPValues are provided, they are included in the table of gene
        sets. The resulting table includes links to additional pages containing
        the ids, names and symbols of each gene in the corresponding set. To get
        the appropriate annotations for the individual gene sets, either
        annotation.db has to be the name of the appropriate annotation package,
        or the geneIdType of the individual gene sets has to have a non-empty
        annotation slot. The user can provide custom functions for coercing the
        enclosed GeneSets to their own preferred HTML representation by
        providing a function for .setToHTML. Likewise, a user-provided function
        can be passed in for .setToDF, to control the coercion of individual
        GeneSets to a data.frame. Additional modifications to the individual
        GeneSet data.frames can be provided as a list of functions in
        .modifySetDF.
        
        GeneSet:\cr
        \code{publish(object, htmlReport, annotation.db, geneStats = NULL, ...)}
        A GeneSet object is coerced to a data.frame. If geneStats are provided,
        an extra column of values for each of the geneIds in the GeneSet is
        appended to the table. The data.frame will optionally include feature
        annotations, such as Entrez IDs, gene symbols and gene names, if either
        annotation.db is a valid annotation package name or if the annotation
        slot of the geneIdType of the GeneSet is a valid annotation package 
        name.
    
        trellis:\cr
        \code{publish(object, publicationType, figureTitle = NULL, 
        filename = NULL, png.height = 480, png.width = 480, pdf.height = 7, 
        pdf.width = 7, br = TRUE, ...)}
        Trellis objects, as created by lattice functions are published to an
        HTMLReport by first printing a pdf and png version of the object, and
        then including the png image in the HTML page, linking it to the pdf
        version of the plot.
    
        Special cases:\cr
        There are a few special types that can be published to a HTMLReport.
        Publishing an HTMLReport to another HTMLReport will create a link from
        one report to the other, using the title of the published report as the
        link text.
  }

  \item{DataPackage}{
        There are two ways to publish R objects to a DataPackage: by
        using a character vector of object names, or by calling publish
        on the object directly, providing a name to use in saving the
        object. For most object types, basic documentation is created
        for the object in the data package, using the \code{promptData}
        function.
  }

  \item{CSVFile}{
        data.frames can be published directly to
        csv files. Other data types, such as MArrayLM, DGEExact,
        GOHyperGResult, PFAMHyperGResult can be output as CSVFile, by
        means of coercion to a data.frame first.
  }

  }
}
\keyword{methods}
\examples{
my.df <- data.frame()
## html.report <- HTMLReport(shortName = "my_html_file",
##     reportDirectory = "reportDirectory")
# publish(my.df, html.report)
## finish(html.report)
}
