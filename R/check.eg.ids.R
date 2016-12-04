##' Checks gene identifiers to make sure they are valid Entrez Ids.
##' 
##' @param ids character vector containing gene identifiers
##' @param annotation.db character string containing annotation database that matches with gene identifiers
check.eg.ids <- function(ids, annotation.db = 'org.Hs.eg'){
    ann.map <- getAnnMap("SYMBOL", annotation.db)
    check <- unlist(mget(ids, ann.map, ifnotfound=NA))
    if(sum(is.na(check))== length(ids)){
        stop("Ids do not appear to be Entrez Ids for the specified species.", call. = FALSE)
    } else if(sum(is.na(check)) >= length(ids)/2){
        warning("More than half of your IDs could not be mapped.", call. = FALSE)
    }
}

check.ids <- function(ids, annotation.db = org.Hs.eg.db, keytype = "ENTREZID"){
    allIds <- keys(annotation.db, keytype = keytype)
    check <- sum(ids %in% allIds)
    if(check == 0){
        stop("Ids do not appear to be the correct key for this annotation database.", call. = FALSE)
    } else if(check < length(ids)/2){
        warning("More than half of your IDs could not be mapped.", call. = FALSE)
    }
}
