annotate.genes <- function(keys, annotation.db, keytype = "ENTREZID",
    columns = list(EntrezId = "ENTREZID", Symbol = "SYMBOL", 
        GeneName = "GENENAME"))
{
    if(class(annotation.db) == "character")
        annotation.db <- get(annPkgName(annotation.db))
    
    ## For each column, return the values for the keys of interest
    ## using the keys and keytype provided above.
    select.fn <- function(col){
        .filter.vals(suppressMessages(select(annotation.db, keys, 
            keytype = keytype, columns = col)))[keys]
    }
    
    ## Iterate across the list items in cols, getting the vals using select
    ## The names from the list are the column names in the data.frame.
    annotation.df <- data.frame(
        lapply(columns, select.fn), 
        stringsAsFactors = FALSE
    )
    
    ## There is a bug in select that returns factors if the column selected
    ## is the same as the keytype. Let's undo this.
    if(keytype %in% columns)
        annotation.df[, which(columns == keytype)] <- keys
    
    annotation.df
}

.filter.vals <- function(map)
{
    if(ncol(map) == 1){
        vals <- setNames(map[, 1], map[,1])
    } else {
        spl <- split(map[, 2], map[, 1])
        len <- sapply(spl, length)
        vals <- sapply(spl, "[[", 1)
        vals[len > 1] <- NA
    }
    vals
}
