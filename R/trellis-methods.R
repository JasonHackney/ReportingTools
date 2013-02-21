setMethod("publish",
    signature = signature(
        object          = "trellis", 
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, figureTitle=NULL, filename=NULL, 
            png.height = 480, png.width = 480, pdf.height = 7, pdf.width = 7, 
            br = TRUE, ...){
        
        if(! validConnection(publicationType))
            stop("Cannot write to closed connection.")

       figures.dirname <- paste('figures', name(htmlRep), sep='')  
        figure.directory <- file.path(basePath(htmlRep), 
            reportDirectory(htmlRep), figures.dirname)
        .safe.dir.create(figure.directory)


        if(is.null(filename)){
            randomPart <- round(runif(1)*100000)
            filename <- paste(name(publicationType), randomPart, sep='-')
        }
        filename <- sub('.png', '', filename)
        filename <- sub('.jpg', '', filename)
        filename <- sub('.pdf', '', filename)
        
        pdf.filename <- file.path(figure.directory,
            paste(filename, 'pdf', sep='.'))
        ## The URL is really hard to get right. What to do?
        pdf.url <- paste(baseUrl(publicationType), 
            reportDirectory(publicationType), 'figures', 
            paste(filename, 'pdf', sep='.'), sep="/")
        
        png.filename <- file.path(figure.directory, 
            paste(filename, 'png', sep='.'))
        png.url <- paste('figures', paste(filename, 'png', sep='.'), sep="/")
        
        cairo_pdf(filename = pdf.filename, height = pdf.height, width = pdf.width)
        print(object)
        dev.off()
        png(filename = png.filename, height = png.height, width= png.width)
        print(object)
        dev.off()
        
        img <- hwriteImage(png.url)
        hwrite(img, link=pdf.url, page=page(publicationType), br=br)
    }
)
