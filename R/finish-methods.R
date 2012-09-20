setMethod("finish",
    signature = signature(
        publicationType = "HTMLReport"
    ),
    definition = function(publicationType, ...){
        closePage(page(publicationType))
    }
)

setMethod("finish",
    signature = signature(
        publicationType = "DataPackage"
    ),
    definition = function(publicationType, ...){
        pkg.dir <- path(publicationType)
        description.fn <- file.path(pkg.dir, 'DESCRIPTION')
        description.str <- paste(
            "Package: ", name(publicationType),
            "\nVersion: ", publicationType@version,
            "\nTitle: ", title(publicationType), 
            "\nAuthor: ", publicationType@author,
            "\nMaintainer: ", publicationType@maintainer,
            "\nDepends: ", paste(dependencies(publicationType), collapse=','),
            "\nLicense: ", publicationType@license,
            "\nType: Package",
            "\nLazyLoad: yes\nDescription: ", publicationType@description, sep="")
        cat(description.str, file=description.fn)
    }
)

