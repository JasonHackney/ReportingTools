setGeneric("getDefaultColumnClasses",
    function(object, ...)
        standardGeneric("getDefaultColumnClasses")
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "ANY"),
    definition = function(object, df = NULL, ...)
    {
        colClasses <- NULL
        if(!is.null(df)){
            colClasses <- getDefaultColumnClasses(df)
        }
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "data.frame"),
    definition = function(object, df = NULL, 
        filter.columns = sapply(object, is.numeric), ...)
    {
        
        filter.columns <- 
          normalizeSingleBracketSubscript(filter.columns,object)
    
        ## For all columns, get the appropriate CSS class for sorting the
        ## data type
        sort.class.map <- c(
            "numeric"   = "sort-num-robust",
            "integer"   = "sort-num-robust",
            "Date"      = "sort-date",
            "character" = "sort-string-robust",
            "factor"    = "sort-string-robust"
        )
        colClasses <- sort.class.map[sapply(object, class)]
        colClasses[is.na(colClasses)] <- "sort-string-robust"
    
        ## For filterable datatypes, also include the CSS class for
        ## filtering the appropriate type
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
        colClasses[filter.columns] <-
          paste(sel.filter.classes, colClasses[filter.columns])
    
        names(colClasses) <- colnames(object)
        
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "MArrayLM"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "ProbeId" = "sort-string-robust",
            "EntrezId" = "sort-num-robust",
            "Symbol" = "sort-string-robust",
            "GeneName" = "sort-string-robust",
            "Image" = "sort-off"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image"), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "DESeqDataSet"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "ProbeId" = "sort-string-robust",
            "EntrezId" = "sort-num-robust",
            "Symbol" = "sort-string-robust",
            "GeneName" = "sort-string-robust",
            "Image" = "sort-off"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image"), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "DGEExact"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "ProbeId" = "sort-string-robust",
            "EntrezId" = "sort-num-robust",
            "Symbol" = "sort-string-robust",
            "GeneName" = "sort-string-robust",
            "Image" = "sort-off"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image"), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "DGELRT"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "ProbeId" = "sort-string-robust",
            "EntrezId" = "sort-num-robust",
            "Symbol" = "sort-string-robust",
            "GeneName" = "sort-string-robust",
            "Image" = "sort-off"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image"), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "GeneSet"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "ProbeId" = "sort-string-robust",
            "EntrezId" = "sort-num-robust",
            "Symbol" = "sort-string-robust",
            "Gene Name" = "sort-string-robust"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image"), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "GeneSetCollection"),
    definition = function(object, df = NULL, ...)
    {
        colClasses <- c(
            "Set Statistic" = "sort-num-robust",
            "Set P-Value" = "sort-num-robust"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("Set Statistic", "Set P-Value"), 
                drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }

            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
        
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "GOHyperGResult"),
    definition = function(object, df = NULL, ...)
    {
        ## For the basic columns, these are the types we generally use.
        colClasses <- c(
            "Accession" = "sort-string-robust",
            "GO Term" = "sort-string-robust",
            "Category Size" = "sort-num-robust",
            "Image" = "sort-off",
            "Overlap" = "sort-num-robust",
            "Odds Ratio" = "sort-num-robust filter-num",
            "P-value" = "sort-num-robust filter-num"
            
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% names(colClasses), drop = FALSE]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
        
                colClasses <- c(colClasses, subColClasses)
            }
            
            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
    }
)

setMethod("getDefaultColumnClasses",
    signature = signature(object = "PFAMHyperGResult"),
    definition = function(object, df = NULL, ...)
    {
        colClasses <- c(
            "PFAM ID" = "sort-string-robust",
            "PFAM Term" = "sort-string-robust",
            "PFAM Size" = "sort-num-robust",
            "Image" = "sort-off",
            "Overlap" = "sort-num-robust",
            "Odds Ratio" = "sort-num-robust filter-num",
            "P-Value" = "sort-num-robust filter-num"
        )
        if(!is.null(df)){
            ## There might also be additional columns beyond these,
            subDF <- df[, ! colnames(df) %in% c("ProbeId", "EntrezId", "Symbol", 
                "GeneName", "Image")]
            if(ncol(subDF) > 0){
                subColClasses <- getDefaultColumnClasses(subDF)
                colClasses <- c(colClasses, subColClasses)
            }
            
            colClasses <- colClasses[colnames(df)]
        }
        
        colClasses
        
    }
)

