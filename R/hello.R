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



#' Show Venn diagram. Automatically called from toVenn.
#'
#' @param nVennObj Object with nVennR information. Can be obtained from a toVenn call.
#' @param opacity Fill opacity for the sets. Defaults to 0.4.
#' @param borderWidth Width of set borders. Defaults to 1.
#' @param outFile File name to save SVG figure. If empty, a temp file will be created and
#' shown in the viewer, if possible.
#' @param systemShow Show the result in the system SVG viewer (i. e., Inkscape).
#' @param showProgress Show progress bar during computation.
#' @return Nothing. Creates a Venn diagram in svg as a side effect.
#' @export
showSVG <- function(nVennObj, opacity=0.4, borderWidth = 1, outFile='', systemShow=FALSE, showProgress=FALSE){
  tfile = outFile
  if (tfile == "") tfile <- tempfile(fileext = ".svg")
  tfile2 <- tempfile(fileext = ".svg")
  nVennObj$svg <- refineVenn(nVennObj, showProgress)
  # transform SVG
  nVennObj$svg <- sub("fill-opacity: *([^ ;]+) *;", paste("fill-opacity: ", opacity, ";"), nVennObj$svg)
  nVennObj$svg <- sub("stroke-width: *([^ ;]+) *;", paste("stroke-width: ", borderWidth, ";"), nVennObj$svg)
  ###############
  cat(nVennObj$svg, file=tfile)
  rsvg::rsvg_svg(svg = tfile, tfile2)
  #s <- magick::image_read(tfile)
  #print(s)
  p <- grImport2::readPicture(tfile2)
  grImport2::grid.picture(p)
  if (systemShow){
    utils::browseURL(tfile)
  }
  #cat("Saved to ", tfile)
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
#' @param showProgress Show progress bar during computation.
#' @param ... One list or vector (possibly mixed) per set. The function also accepts tables and data frames.
#' If input lists have names, those names will be used for the legend. If not, names can be
#' provided with \code{sNames}.
#' @return nVennObj with the result of the simulation. As a side effect, The result can be drawn in the
#' graphical device.
#' @examples
#' set1 <- list(set1 = c('a', 'b', 'c'))
#' set2 <- list(set2 = c('e', 'f', 'c'))
#' set3 <- list(set3 = c('c', 'b', 'e'))
#' mySVG <- toVenn(set1, set2, set3, sNames=c("One", "Two", "Three"))
#' showSVG(mySVG=mySVG, opacity=0.2)
#' @export
toVenn <- function(..., nVennObj=NULL, nCycles=7000, sNames=NULL,
                     showPlot=T, showProgress=F){
  lresult <- NULL
  if (is.null(nVennObj)){
    sets <- .flattenInput(..., sNames=sNames)
    nBits <- .getNBits(sets)
    if (nBits == 0){
      stop("You must provide at least one list (and seriously consider providing more than one)")
    }
    lresult <- .processVenn(sets, nBits)
  }
  else{
    lresult <- nVennObj
  }
  myVenn <- makeVenn(lresult, nCycles, showProgress)
  class(myVenn) <- append(class(myVenn), "nVennObj")
  if (showPlot == T) showSVG(myVenn, showProgress = showProgress)
  return(myVenn)
}


.getNBits <- function(sets){
  return(length(sets))
}

.flattenInput <- function(..., sNames=NULL){
  sets <- list(...)
  result <- list()
  nNames <- length(sNames)
  i <- 1
  for (set in sets){
    if (class(set) == "table" || class(set) == "matrix" || class(set) == "data.frame"){
      nSet <- list()
      mtx <- FALSE
      if (class(set) == "matrix"){
        mtx <- TRUE
      }
      for (n in colnames(set)){
        l <- levels(set[,n])
        if (mtx){ l <- set[,n] }
        nSet[[n]] <- list(unlist(l))
      }
      result <- c(result, nSet)
    }
    else if (class(set) == "list"){
      print("yo")
    }
    else{
      i <- 1
      iNames <- names(set)
      niNames <- length(iNames)
      cname <- paste('name', i, sep='')
      if (nNames >= i && sNames[[i]] != ""){
        cname <- sNames[[i]]
      }
      if (niNames >= i && iNames[[i]] != ""){
        cname <- iNames[[i]]
      }
      result[[cname]] <- set
      i <- i + 1
    }
  }
  print(result)
  stop()
  return(result)
}

#' Get elements in a region
#' @param nVennObj Object describing an nVenn job.
#' @param region Description of the region.
#' @return list of the elements belonging to the specified region
#' @export
getVennRegion <- function(nVennObj, region){
  topGroups <- as.numeric(nVennObj$def[[2]]) + 2
  sets <- as.list(nVennObj$def[3:topGroups])
  nGroups <- .getNBits(sets)
  tRegions <- nVennObj$reg
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
  r <- .fromBin(reg)
  result <- as.vector(unlist(tRegions[r]))
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
    regions[[i]] <- start
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
