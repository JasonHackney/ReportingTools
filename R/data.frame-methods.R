setMethod("publish",
    signature = signature(
        object          = "ANY",
        publicationType = "list"
    ),
    definition = function(object, publicationType, ...){
        for(pubType in publicationType){
            publish(object, pubType, ...)
        }
    }
)

setMethod("publish",
    signature = signature(
        object = "data.frame",
        publicationType = "DataPackage"
    ),
    definition = function(object, publicationType, objectName, ...){
        .save.rda.fn(object, publicationType, objectName, ...)
    }
)

setMethod("publish",
    signature = signature(
        object          = "data.frame", 
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, tableTitle="",
      filter.columns = sapply(object, is.numeric), ...){
        
        if(! validConnection(publicationType))
            stop("Cannot write to closed connection.")
        
        if(nrow(object) == 0)
            stop("No rows available in data.")
        
        if(ncol(object) == 0)
            stop("No columns available in data.")

        filter.columns <-
          IRanges:::normalizeSingleBracketSubscript(filter.columns, object)
        
        sort.class.map <- c(
            "numeric"   = "sort-num",
            "integer"   = "sort-num",
            "Date"      = "sort-date"
        )
        sort.classes <- sort.class.map[sapply(object, class)]
        sort.classes[is.na(sort.classes)] <- "sort-string"
        
        filter.class.map <- c(
            "numeric" = "filter-num",
            "integer" = "filter-num",
            "logical" = "filter-cat",
            "factor"  = "filter-cat",
            "Date"    = "filter-date")
        filter.classes <- filter.class.map[sapply(object, class)]
        filter.classes[is.na(filter.classes)] <- "filter-string"
        sel.filter.classes <- filter.classes[filter.columns]
        col.classes <- sort.classes
        col.classes[filter.columns] <-
          paste(sel.filter.classes, col.classes[filter.columns])
        
        col.specs <- data.frame(
            column  = seq_along(object),
            label   = colnames(object),
            class   = col.classes,
            stringsAsFactors = FALSE
        )
        
        numeric.columns <- which(unlist(lapply(object, class)=="numeric"))
        object[, numeric.columns] <- signif(object[, numeric.columns], 3)        
        
        p <- .writeHTMLTable(object, tableTitle = tableTitle, col.specs, 
            p = page(publicationType))
        invisible(p)
    }
)

setMethod("publish",
    signature = signature(object = "data.frame", 
        publicationType = "CSVFile"),
    definition = function(object, publicationType, ...){
        fn <- path(publicationType)
        .safe.dir.create(reportDirectory(publicationType))
        write.csv(object, file=fn, ...)
    }
)


.writeHTMLTable <- function(df,
                          tableTitle,
                          column.specs = NULL,
                          p = NULL) 
{
    ## order columns based on column.specs
    df <- df[, column.specs$column, drop=FALSE]
    colnames(df) <- column.specs$label    

    ## add class called "table-header" to column.specs
    ## to allow all the columns to be styled by css
    if ( any(is.na(column.specs$class)) ) {
      column.specs[is.na(column.specs$class),]$class <- ""
    }

    ## make sure there are no leading spaces
    column.specs$class <- gsub("^\\s+","",column.specs$class,perl=TRUE)

    ## add top-header-row to be styled in css
    ## (this row will be styled to have font-soze:0
    column.specs$class <- paste(column.specs$class,"top-header-row", sep = " ")

    col.class <- data.frame(do.call(cbind,
                                lapply(column.specs$class,
                                       function(z) c(z, rep("table-text", nrow(df))))
                                ), stringsAsFactors=FALSE)
    names(col.class) <- column.specs$label

    ## write the title html
    titleHtml <- hwrite(tableTitle, heading=2)

    ## change titleHtml to a <p> and add class page-header for styling
    titleHtml <- sub("<.*?>","<p class=\"page-header\">",titleHtml)
    titleHtml <- sub("</.*?>","</p>",titleHtml)

    ## mainHtml is the table html for the page
    tableHtml <- hwrite(df, col.class=as.list(col.class), row.names=FALSE,
                        table.class="dataTable pretty")

    ## make the top row of the table html a header row
    ## (sub only replaces on first match)
    tableHtml <- sub("border=\"1\"","",tableHtml)
    tableHtml <- sub("<tr>","<thead><tr>",tableHtml)
    tableHtml <- sub("</tr>","</tr></thead><tbody>",tableHtml)

    ## change the "td class" in thead to "th class" so that the icons will show up
    ## if you modify this, note the ungreedy regexp with the ? 
    topTableHtml <- sub("(.*?)</thead>.*","\\1", tableHtml)

    ## pull our components of page to modify them independently
    topHtml <- sub("(.*?)<thead>.*","\\1", topTableHtml)
    topHeaderRow <- sub(".*<thead>(.*?)","\\1", topTableHtml)

    ## add "nowrap" to the header row so that the text won"t wrap without
    ## specifying a <br>
    topHeaderRow <- gsub("<td", "<th nowrap=\"nowrap\"",topHeaderRow)
    topHeaderRow <- gsub("</td>", "</th>",topHeaderRow)

    ## make the bottom header row
    ## the bottom header row will contain the class needed for searching
    bottomHeaderRow <- gsub("top-header-row","bottom-header-row",topHeaderRow)
        
    ##  Because of the js library, filterRowHtml needs to go in 2 places.
    ## Once in the footer (actually required by code)
    ## and the other in the second row of the head of the table
    ## the footer code will be styled with font-size:0
    headHtml <- paste(topHtml,"<thead>",topHeaderRow,bottomHeaderRow,"</thead>",sep="")
        
    ## paste together the final html to print
    ## note the ungreedy regexp with the ? 
    bottomHtml <- sub(".*?</thead>(.*)","\\1", tableHtml)
    bottomHtml <- sub("</table>","</tbody></table>",bottomHtml)


    ## make final html for page    
    html <-  paste("<div id=\"container\">",titleHtml,headHtml,bottomHtml,"<foot>",bottomHeaderRow,"</foot>","</div>",sep = "")

    #write the html to a txt file
    #html to return will be <iframe src="stuff"></iframe>
    
    if ( !missing(p) ) {
      cat(html, file = p, sep = "\n")
      return(p)
    }
    else {      
      return(html) 
    }
}
