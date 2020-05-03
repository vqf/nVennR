# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @useDynLib nVennR
#' @importFrom Rcpp sourceCpp
NULL

#' Example data frame.
#'
#' A dataset containing programming preferences from 18 employees. This data set was
#' provided by user Krantz to inquire about nVennR
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{Employee}{Employee ID}
#'   \item{SAS}{Employee uses SAS}
#'   \item{Python}{Employee uses Python}
#'   \item{R}{Employee uses R}
#' }
#' @source \url{https://stackoverflow.com/questions/49471565/transforming-data-to-create-generalized-quasi-proportional-venn-diagrams-using}
"exampledf"



#' Show Venn diagram. Automatically called from plotVenn.
#'
#' @param nVennObj Object with nVennR information. Can be obtained from a plotVenn call.
#' @param opacity Fill opacity for the sets. Defaults to 0.4.
#' @param borderWidth Width of set borders. Defaults to 1.
#' @param showLegend Boolean stating whether the resulting figure should contain a legend. Defaults to true.
#' @param labelRegions Show region identifiers. These are numbers in parentheses inside each region
#' indicating which sets that region belongs to. Defaults to true
#' @param showNumbers Show how many elements belong to each region (large numbers in the figure).
#' Defaults to true
#' @param setColors Vector with the color of each set in order. Color names must be CSS-compatible.
#' @param outFile File name to save SVG figure. If empty, a temp file will be created and
#' sent to the graphic device.
#' @param systemShow Show the result in the system SVG viewer (i. e., Inkscape).
#' @param fontScale Multiplier for font sizes. The font size of both numbers and region labels will
#' be multiplied by this factor. Values larger than 2 will probably make labels clash.
#' @return Nothing. Creates a Venn diagram in svg as a side effect.
#' @export
showSVG <- function(nVennObj, opacity=0.4, borderWidth = 1, showLegend=T, outFile='', systemShow=FALSE,
                    labelRegions=T, showNumbers=T, setColors=NULL, fontScale=1){
  nSets <- nVennObj$def[[2]]
  dleg <- ifelse(showLegend, "inline", "none")
  tfile = outFile
  if (tfile == "") tfile <- tempfile(fileext = ".svg")
  tfile2 <- tempfile(fileext = ".svg")
  nVennObj$svg <- refineVenn(nVennObj)
  # transform SVG
  nVennObj$svg <- sub("fill-opacity: *([^ ;]+) *;", paste("fill-opacity: ", opacity, ";"), nVennObj$svg)
  nVennObj$svg <- sub("stroke-width: *([^ ;]+) *;", paste("stroke-width: ", borderWidth, ";"), nVennObj$svg)
  nVennObj$svg <- sub("display: *([^ ;]+) *;", paste("display: ", dleg, ";"), nVennObj$svg)
  if (!labelRegions){
    nVennObj$svg <- sub("belong *\\{", paste("belong \\{", "\n", "display: none;"), nVennObj$svg)
  }
  if (!showNumbers){
    nVennObj$svg <- sub("nLabel *\\{", paste("nLabel \\{", "\n", "display: none;"), nVennObj$svg)
  }
  if (fontScale != 1){
    cFs <- 10 * fontScale
    nVennObj$svg <- sub("(nLabel *\\{.*?font-size *: *)[^;]+", paste("\\1", cFs, "px", sep = ""), nVennObj$svg)
    cFs <- 5 * fontScale
    nVennObj$svg <- sub("(belong *\\{.*?font-size *: *)[^;]+", paste("\\1", cFs, "px", sep = ""), nVennObj$svg)
  }
  if (length(setColors) > 0){
    n <- min(length(setColors), nSets)
    for (i in 1:n){
      ccol <- setColors[[i]]
      topaste <- paste("(q", i - 1, " *\\{.*?stroke *: *)[^ ;]+ *;", sep = "")
      nVennObj$svg <- sub(topaste, paste("\\1", ccol, ";"), nVennObj$svg)
      topaste <- paste("(p", i - 1, " *\\{.*?fill *: *)[^ ;]+ *;", sep = "")
      nVennObj$svg <- sub(topaste, paste("\\1", ccol, ";"), nVennObj$svg)
    }
  }
  #print(nVennObj$svg)
  ###############
  cat(nVennObj$svg, file=tfile)
  if (requireNamespace("rsvg", quietly = TRUE) && requireNamespace("grImport2", quietly = TRUE)) {
    out <- tryCatch(
      {
        rsvg::rsvg_svg(svg = tfile, tfile2)
        p <- grImport2::readPicture(tfile2)
        grImport2::grid.picture(p)
      },
      error=function(cond){
        message(paste("rsvg or grImport2 reported an error: ", cond))
        message("The figure cannot be rendered in the plot window. Please, use the arguments outFile and/or systemShow.")
      }
    )
  } else {
    if (systemShow == FALSE && outFile == ''){
      message("The figure cannot be rendered in the plot window. Please, use the arguments outFile and/or systemShow.")
    }
  }
  if (systemShow){
    utils::browseURL(tfile)
  }
}


#' Create Venn diagram using the nVenn algorithm.
#'
#' This algorithm is based on a simulation
#' that compacts the figure. If the resulting diagram is not compact enough, the simulation can be tweaked
#' in two ways: changing the number of simulation cycles (`nCycles`) and executing this function repeatedly.
#'
#'
#' @param sNames List of set names, in the same order as the input lists. If the input has tables or
#' data frames and the name exists, it will select the corresponding column.
#' @param nVennObj Object returned from previous run. If provided, the function will improve the
#' diagram by running more cycles on the previous result. If nVennObj is provided, do not feed
#' additional input lists, as they will be ignored
#' @param nCycles Number of cycles for the simulation. For up to 4 sets, the default number of 7000
#' should be enough. Even for more complex scenarios, it may be better to run the function repeatedly,
#' as a large number of cycles may take up too many resources.
#' @param showPlot Show the result in the graphic device.
#' @param sets List of lists with the input sets.
#' @param ... Options for `showSVG`
#' If input lists have names, those names will be used for the legend. If not, names can be
#' provided with \code{sNames}.
#' @return nVennObj with the result of the simulation. As a side effect, The result can be drawn in the
#' graphical device.
#' @examples
#' set1 <- list(set1 = c('a', 'b', 'c'))
#' set2 <- list(set2 = c('e', 'f', 'c'))
#' set3 <- list(set3 = c('c', 'b', 'e'))
#' myNV <- plotVenn(list(set1, set2, set3), sNames=c("One", "Two", "Three"))
#' showSVG(myNV, opacity=0.2)
#' @export
plotVenn <- function(sets, nVennObj=NULL, nCycles=7000, sNames=NULL,
                     showPlot=T, ...){
  lresult <- NULL
  if (is.null(nVennObj)){
    sets <- .flattenInput(sets, sNames=sNames)
    nBits <- .getNBits(sets)
    if (nBits == 0){
      stop("You must provide at least one list (and seriously consider providing more than one)")
    }
    lresult <- .processVenn(sets, nBits)
  }
  else{
    lresult <- nVennObj
  }
  rg <- .getRegions(lresult)
  if (!(any(rg > 0))){
    message("All regions are zero. The resulting diagram will probably be blank.")
    message("If you still want the diagram, you can set nCycles to zero.")
  }
  myVenn <- makeVenn(lresult, nCycles)
  class(myVenn) <- append(class(myVenn), "nVennObj")
  if (showPlot == T) showSVG(myVenn, ...)
  return(myVenn)
}


#' Get elements in a region
#' @param nVennObj Object describing an nVenn job.
#' @param region Description of the region. This can be a vector with the names of the groups the region
#' belongs to or a vector describing whether the region belongs to each set in order (i. e., c(1, 0, 0)
#' means the region belongs to set 1 and does not belong to sets 2 and 3).
#' @return list of the elements belonging to the specified region
#' @export
getVennRegion <- function(nVennObj, region){
  tRegions <- nVennObj$reg
  result <- NULL
  reg <- .interpretRegion(nVennObj, region)
  r <- .fromBin(reg)
  if (length(tRegions[r]) > 0){
    result <- as.vector(unlist(tRegions[r]))
  }
  return(result)
}

#' List elements in every region
#'
#' @param nVennObj Object to list.
#' @param na.rm If true, empty regions are not listed.
#' @export
listVennRegions <- function(nVennObj, na.rm=T){
  nBits <- as.integer(nVennObj$def[[2]])
  nReg <- bitwShiftL(1, nBits) - 1
  result <- list()
  for (i in 0:nReg){
    rg <- .toBin(i, nBits)
    s <- paste0(toString(rg), ' (', toString(.regionToString(nVennObj, rg)), ')')
    toadd <- getVennRegion(nVennObj, rg)
    if (is.null(toadd)){
      if (na.rm){
        result[[s]] <- NULL
      }
      else{
        result[[s]] <- NA
      }
    }
    else{
      result[[s]] <- toadd
    }
  }
  return(result)
}

#' Set number of elements in a region
#' @param nVennObj Object describing an nVenn job.
#' @param region Description of the region. This can be a vector with the names of the groups the region
#' belongs to or a vector describing whether the region belongs to each set in order (i. e., c(1, 0, 0)
#' means the region belongs to set 1 and does not belong to sets 2 and 3).
#' @param value Size of the region.
#' @return Modified nVennObj
#' @export
setVennRegion <- function(nVennObj, region, value){
  reg <- .interpretRegion(nVennObj, region)
  r <- .fromBin(reg)
  nSets <- as.numeric(nVennObj$def[[2]])
  offset <- nSets + 3
  if (class(value) != "numeric"){
    value <- 0
    message("Value must be numeric. It has been set to 0.")
  }
  ind <- offset + r
  nVennObj$def[[ind]] <- value
  return(nVennObj)
}


#' Create nVennObj from scratch
#'
#' @param nSets Number of sets.
#' @param sNames List of names.
#' @param sSizes List of sizes for all the regions (from `0` to `2**nSets - 1`). To understand the order
#' of the regions, one can think of a region as a binary number. Each bit tells whether the region belongs
#' (1) or not (0) to a given set. For instance, with 4 sets we have 4 bits. The number 7 with 4 bits is
#' 0111, which describes a region belonging to sets 2, 3, and 4 and not to set 1. To pass the values
#' of the regions, those values are sorted according to the number describing the region. Thus, with
#' four sets, the first element corresponds to region 0 (0000), the second to region 1 (0001), the third
#' to region 2 (0010), ... The last corresponds to region 15 (1111), which belongs to all the sets.
#' @return nVennObj with set information. To plot, it must be sent to `toVenn`. Sending it to `showSVG`
#' will render the diagram before simulation.
#' @export
createVennObj <- function(nSets=1, sNames=NULL, sSizes=NULL){
  result <- list()
  result$def <- c(result$def, "nVenn")
  nNames <- length(sNames)
  nSizes <- length(sSizes)
  if (nSets > 0){
    result$def <- c(result$def, ceiling(nSets))
  }
  else{
    stop("The number of sets is not valid")
  }
  for (i in 1:nSets){
    cName <- paste("Group", i, sep="")
    if (nNames >= i){
      cName <- sNames[[i]]
    }
    result$def <- c(result$def, cName)
  }
  nReg <- bitwShiftL(1, ceiling(nSets))
  for (i in 1:nReg){
    cSize <- 0
    if (nSizes >= i){
      cSize <- sSizes[[i]]
    }
    result$def <- c(result$def, cSize)
  }
  class(result) <- append(class(result), "nVennObj")
  return(result)
}

.getRegions <- function(nVennObj){
  nSets <- as.numeric(nVennObj$def[[2]])
  offs <- nSets + 3
  n <- bitwShiftL(1, nSets) + offs - 1
  return(nVennObj$def[offs:n])
}

.getSets <- function(nVennObj){
  n <- as.numeric(nVennObj$def[[2]]) + 2
  return(nVennObj$def[3:n])
}

.regionToString <- function(nVennObj, region){
  s <- as.data.frame(list(sets=.getSets(nVennObj), bins=region))
  result <- s[s$bins == 1,]$sets
  return(result)
}

.interpretRegion <- function(nVennObj, region){
  topGroups <- as.numeric(nVennObj$def[[2]]) + 2
  sets <- as.list(nVennObj$def[3:topGroups])
  nGroups <- .getNBits(sets)
  reg <- vector(length = nGroups, mode = "numeric")
  if (class(region) == "character"){
    for (s in region){
      pos <- match(s, sets)
      if (is.na(pos)){
        message(paste("Element ", s, "does not describe a set"))
      }
      else{
        reg[[pos]] <- 1
      }
    }
  }
  else{
    if (length(region) == nGroups){
      reg <- as.vector(unlist(lapply(X = region, FUN = function(x) ifelse(as.numeric(x) == 0, 0, 1))))
    }
    else{
      stop("Cannot interpret input.")
    }
  }
  return(reg)
}


.getNBits <- function(sets){
  return(length(sets))
}

.flattenInput <- function(sets, sNames=NULL){
  result <- list()
  nNames <- length(sNames)
  oNames <- names(sets)
  onNames <- length(names(sets))
  i <- 1
  if (class(sets) != "list"){
    stop("Input must be a list of lists")
  }
  else{
    for (set in sets){
      nm <- paste("Group", i)
      if (nNames >= i && sNames[[i]] != ''){
        nm <- sNames[[i]]
      }
      if (onNames > 0 && onNames >= i && oNames[[i]] != ""){
        nm <- oNames[[i]]
      }
      i <- i + 1
      result[nm] <- list(unlist(set))
    }
  }
  return(result)
}


.processVenn <- function(sets, nBits){
  nRegions <- bitwShiftL(1, nBits)
  regions <- list()
  result <- c("nVenn1.2", toString(nBits))
  al <- unlist(sets[[1]])
  for (i in 2:nBits){
    al <- union(al, unlist(sets[[i]]))
  }
  result <- c(result, names(sets))
  result <- c(result, toString(0))
  for (i in 1:(nRegions - 1)){
    start <- al
    belongs <- .toBin(i, nBits)
    for (j in 1:length(belongs)){
      k <- belongs[[j]]
      if (k == 1){
        start <- intersect(start, unlist(sets[[j]]))
      }
      else{
        start <- setdiff(start, unlist(sets[[j]]))
      }
    }
    if (length(start) > 0){
      regions[[i]] <- start
    }
    else{
      regions[[i]] <- NULL
    }
    result <- c(result, length(start))
  }
  lresult <- list()
  lresult$def <- result
  lresult$reg <- regions
  lresult$orig <- sets
  return(lresult)
}


.toBin <- function(n, Nbits){
  result <- c()
  for (i in (0 : (Nbits - 1))){
    t <- bitwShiftL(1, i)
    bit <- bitwAnd(n, t)
    if (bit > 0){
      bit <- 1
    }
    result <- c(bit, result)
  }
  return(result)
}

.fromBin <- function(binList){
  result <- 0
  for (b in binList){
    result <- bitwShiftL(result, 1)
    result <- result + b
  }
  return(result)
}
