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
\alias{publish,HTMLReport,HTMLReport-method}
\alias{publish,trellis,HTMLReport-method}
\alias{publish,list,HTMLReport-method}
\alias{publish,ANY,HTMLReport-method}

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

  \item{HTMLReport\cr}{
        
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
        Gene IDs. The counts stored in countTable should be in units of counts
        per million provided by the cpm function in the edgeR package. The
        p-value adjustment method can be set using the ``adjust.method''
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
        Gene IDs. The counts stored in countTable should be in units of counts
        per million provided by the cpm function in the edgeR package. The
        p-value adjustment method can be set using the ``adjust.method''
        argument.
      
        HyperGResultBase, GOHyperGResult, PFAMHyperGResult:\cr
        \code{publish(object, htmlReport, selectedIDs, annotation.db, 
        pvalueCutoff = 0.01,categorySize=10, makePlot=FALSE,...)}
        A HyperGResult object is coerced to a data.frame using a method
        similar to the summary function from the Category package. The
        resulting table includes for each classification the id, name,
        odds ratio of enrichment and p-value from the
        hypergeometric test. A glyph showing the level of overlap of
        each classification with the selected genes is also plotted. The
        number of genes found in this classification is listed in
        the table and links to another page with a table of the
        corresponding genes, symbols and names.  An additional page
        listing the overlap genes is also linked to the main output. 
        For a GOHyperGResult object, a plot depicting the relationship
        between the significant ontologies and their parents is also
        plotted if makePlot is TRUE.  The selectedIDs are the Entrez
        ids of the genes of interest (i.e. from the gene universe); 
        annotation.db is the species of the ids.
      
        GeneSetCollection:\cr
        \code{publish(object, htmlReport, annotation.db, 
        setStats=NULL, setPValues=NULL,...)}
        A GeneSetCollection object is coerced to a data.frame. The
        resulting table of set names includes links to additional pages
        containing the ids, names and symbols of each gene in the
        corresponding set. If GSEA statistics and/or p-values are
        provided, then they are also included in the gene set table.
        The genes within the GeneSetCollection must be Entrez ids and
        annotation.db is the species of the ids.
    
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
