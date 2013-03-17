nullFun = function(...) TRUE
setClassUnion("characterOrNULL", c("character","NULL"))

#I'm envisioning each ReportHandlers set representing a distinct target (eg internal DOM, write to pipe, etc)
#Currently args expects a named list of named lists, up to one for each function, eg list(finish = list(file = "myfile.html"))
#we might want to make this an S4 class later?
setClass("ReportHandlers",
         representation = list(init = "function",
           addElement = "function",
           removeElement = "function",
           finish = "function",
           location = "ANY",
           args = "ANY"),
         prototype = list(init = nullFun,
           addElement = nullFun,
           removeElement = nullFun,
           finish= nullFun,
           location = NULL,
           args = NULL)
         )



baseReport <- setRefClass("BaseReportRef",
  fields = list(
    .shortName = "character",
    shortName = function(val)
    {
      if(missing(val))
        .shortName
      else
        .self$.shortName <- val
    },
    .title = "character",
    title = function(val)
    {
      if(missing(val))
        .title
      else
        {
          .self$.title <- val
          #if we have an HTML document, change/add the title node
          if(is(.reportDOM, "XMLInternalDocument"))
            {
              tnode = getNodeSet(.reportDOM, "/html/head/title")
              if(length(tnode))
                xmlValue(tnode[[1]]) = val
              else
                {
                  head = getNodeSet(.reportDOM, "/html/head")[[1]]
                  addChildren(head, newXMLNode("title", val))}
            }
          .title
              
        }
    },
    .reportDirectory = "character",
    reportDirectory = function(val)
    {
      if(missing(val))
        .reportDirectory
      else
        #.self$.reportDirectory <- val
        .reportDirectory <<- val
    },
    .report = "list",
    .reportDOM = "ANY",
    .basePath = "characterOrNULL",
    basePath = function(val)
    {
      if(missing(val))
        .basePath
      else
        .self$.basePath <- val
    },
    .baseUrl = "character",
    baseUrl = function(val)
    {
      if(missing(val))
        .baseUrl
      else
        .self$.baseUrl <- val
    },
    .handlers = "list",
    handlers = function(val)
    {
      if(missing(val))
        .handlers
      else
        {
        #stop("The addHandlers/removeHandlers methods must be used to manipulate event handlers.")
          if(!is.list(val))
            val = list(val)
        .self$.handlers <- val
        }
      }),
  methods = list(
    addHandlers = function(handlers, init = nullFun, addElement = nullFun, 
        removeElement = nullFun, finalize = nullFun)
    {
      if(missing(handlers) | is.null(handlers))
        handlers2 = new("ReportHandlers", init = init, addElement = addElement, 
          removeElement = removeElement, finalize = finalize)
      else
        handlers2 = handlers
      .self$.handlers <- c(.handlers, handlers2)
    },
    removeHandlers = function(pos = 1)
    {
      if (!(pos %in% seq(along = .handlers)))
        stop(sprintf("There is no set of handlers at position %d  to remove.", pos))
      .self$.handlers <- .handlers[-pos]
    }
    )
  )

htmlReport <- setRefClass("HTMLReportRef", contains = "BaseReportRef",
  fields = list(
    .toHTML = "list",
    .toDF = "list",
    .modifyDF = "list"),
  methods = list(
    
    prepare = function(obj,.toHTML = NULL, .toDF = NULL, .modifyDF = NULL, ... )
    {
      #if the user has overridden  the html conversion for this class, we use that
      #we grab the first class if there are more than one (ie with XMLInternalNode)
      klass = class(obj)[1]
      f = if(missing(.toHTML) || is.null(.toHTML)) .self$.toHTML[[klass]] else .toHTML
      if(is.function(f))
        {
          ret = f(obj, ...)
        }
      else
        {
          if(missing(.toDF) || is.null(.toDF))
            .toDF2 = .self$.toDF[[klass]]
          else
            .toDF2 = .toDF
          if(missing(.modifyDF) || is.null(.modifyDF))
            .modifyDF2 = .self$.modifyDF[[klass]]
          else
            .modifyDF2 = .modifyDF
          html = objectToHTML(obj, .self, ..., .toDF=.toDF2, .modifyDF = .modifyDF2 )
          
          #prepping for conversion from text HTML nubuilding to XML construction Once the
          #conversion is complete objectToHTML methods will be returning XMLInternalNode
          #objects
          
          #this is a bit hacky but oh well!! We may be getting a list with elements "html"
          #and "object"
          if(is(html, "XMLNodeSet"))
          {
              htmlcode <- html
          } else if(is.list(html))
            {
              htmlcode = html$html
              obj = html$object
          }
          else
            {
              htmlcode = html
              obj = NULL
            }
          if(is.character(htmlcode))
            {  
              htmlcode = htmlParse(htmlcode)
              ret = getNodeSet(htmlcode, "//body/*")
            } else {
              ret = htmlcode
            }
        }
      list(html = ret, object = obj)
    },
    finish = function()
    {
      sapply(.handlers, function(fs) fs@finish(.self, fs@args$finish))
      # do we want to force a saveXML call here, or just assign one as a finalize
      # event handler by default? For now we make people assign a handler,
      # because sometimes we only want to send the content down a connection and
      # not write a file.
    },
    addElement = function(name, value, .toHTML = NULL, .toDF = NULL, 
        .modifyDF = NULL, pos = NA, ... )
    {
      
      if(missing(name))
        name = paste("id", length(getNodeSet(.self$.reportDOM,"//body/div")) + 1, sep="")
      if(is.character(name))
        nodes = getNodeSet(.self$.reportDOM, sprintf("//div[@id='%s']", name))
      else if (is.numeric(name))
        stop("positional insertion is not yet supported")
      
      if(length(nodes))
        {
          if(!is.na(pos))
            stop("Attempt to specify a position (pos) when replacing an existing element")
          node = nodes[[1]]
          #remove whatever was assigned to this name previously
          removeChildren(node, kids = xmlChildren(node))
          
        } else {
          #create new div with the specified id and add it to the body of the HTML page
          node = newXMLNode("div", attrs= list(id=name, class = "ReportingTools"))
          if(is.na(pos))
            {
              #No position means it gets added to the end of the document
              body = getNodeSet(.self$.reportDOM, "//body")[[1]]
              addChildren(body, node)
              
            } else {
              #If we are given a position n, find the nth report element in the current report and insert our new element directly ahead of it.
              addSibling(.self$.report[[pos]], after=FALSE, node)
            }
        }

      #turn value into html nodes to add to DOM
      newcontent = .self$prepare(value, .toHTML = .toHTML, .toDF = .toDF, 
          .modifyDF = .modifyDF,... )
      obj = newcontent$object
      newcontent=newcontent$html
      if(is.list(newcontent))
        addChildren(node, kids = newcontent)
      else
        addChildren(node, newcontent)
      # call all currently assigned addElement handlers with the node for the div
      # containing the new content
      sapply(.self$.handlers, function(fs, node, name) 
        fs@addElement(node, name, fs@args$addElement), node= node, name=name)
      if(is.na(pos))
        .self$.report[[name]] = node
      else
        {
          #insert our new element in the right place and shift everything else around.
          oldlist = .self$.report
          oldnames = names(oldlist)
          before = which(seq(along=oldlist) < pos)
          after = which(seq(along=oldlist) >= pos)
          newlist = vector("list", length(oldlist) + 1)
          newlist[before] = oldlist[before]
          newlist[[pos]] = node
          newlist[after + 1] = oldlist[after]
          names(newlist ) = c(oldnames[before], name, oldnames[after])
          .self$.report = newlist

        }
      invisible(obj)


    },
    initialize = function(...)
    {
      args = list(...)
      
      handlers2 = args$handlers
      if(is.null(handlers))
        handlers2 = list(toFileHandlers)

      #if we only have one set of handler sometimes it won't come in the form of a list
      if(!is.list(handlers2))
        handlers2 = list(handlers2)
      dom = startHTMLReport(...)
      .self$.reportDOM = dom
      sapply(handlers2, function(h) h@init(dom, h@args$init))
      .self$initFields(shortName = args$shortName, title = args$title,
          reportDirectory = args$reportDirectory, handlers = handlers2, 
          basePath = args$basePath, baseUrl = args$baseUrl)

    }
    )
  )
    
HTMLReport <- function(shortName = "coolProject",
  title = shortName, 
  reportDirectory = ".",
  basePath = NULL,
  baseUrl = "localhost",
  handlers = list(fileHandlers(makeReportPath(basePath, reportDirectory, shortName))),
  .toHTML = list(),
  .toDF = list(),
  .modifyDF= list(),
  link.css = NULL,
  link.javascript=NULL,
  overwrite.js = TRUE,
  ...
  )
  {
    
    shortName <- sub(".html$", "", shortName)
    if(is.null(title))
        title <- shortName

    if(substr(reportDirectory,1,1) == "~"){
        reportDirectory <- path.expand(reportDirectory)
    }
    if(!is.list(handlers))
      handlers = list(handlers)
    
    allargs = list(shortName = shortName, basePath = basePath, reportDirectory = reportDirectory, ...)
    handlers = lapply(handlers, function(hnd)
      {
        if(is(hnd, "ReportHandlers"))
          hnd
        else if (is(hnd, "function"))
          do.call(hnd, allargs)
        else
          stop("handlers must be a list containing only ReportHandler objects and/or functions which return such objects.")
      })
    
    if(substr(reportDirectory, 1, 1) == "/")
        stop("Non-NULL baseDirectory in combination with absolute reportDirectory is not supported.")
    if(sum(grepl("[A-Za-z]:", reportDirectory)) > 0 & .Platform$OS.type == "windows")
        stop("Non-NULL baseDirectory in combination with absolute reportDirectory is not supported.")
    
    force(handlers)
    htmlReport$new(title = title, shortName = shortName, 
        reportDirectory = reportDirectory, handlers = handlers, 
        basePath = basePath, baseUrl = baseUrl, .toHTML = .toHTML, 
        .toDF = .toDF, .modifyDF  = .modifyDF, link.css= link.css, 
        link.javascript = link.javascript, ovewrite.js = overwrite.js)
  }




### See AllOldClasses.R for old S4 based Report classes.
