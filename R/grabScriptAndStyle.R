grabScriptAndStyle = function(dom)
  {
    nodeschar = getNodeSet(dom, "//script|//style|//link[@rel='stylesheet']", fun = saveXML)
    cat(paste(nodeschar, collapse="\n"))
  }
    
