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
#' @param mySVG SVG code defining the diagram. Can be retrieved from toVenn.
#' @param opacity Fill opacity for the sets. Defaults to 0.4.
#' @param outFile File name to save SVG figure. If empty, a temp file will be created and
#' shown in the viewer, if possible.
#' @param systemShow Show the result in the system SVG viewer (i. e., Inkscape).
#' @param showProgress Show progress bar during computation.
#' @return Nothing. Creates a Venn diagram in svg as a side effect.
#' @export
showSVG <- function(mySVG, opacity=0.4, outFile='', systemShow=FALSE, showProgress=FALSE){
  tfile = outFile
  if (tfile == "") tfile <- tempfile(fileext = ".svg")
  tfile2 <- tempfile(fileext = ".svg")
  mySVG$svg <- refineVenn(mySVG, showProgress)
  # transform SVG
  mySVG$svg <- sub("fill-opacity: *([^ ;]+) *;", paste("fill-opacity: ", opacity, ";"), mySVG$svg)
  ###############
  cat(mySVG$svg, file=tfile)
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
#' that compacts the figure. To avoid clogging the system, the simulation stops every 7000
#' cycles and asks the user if it sould go on. Only answering 'y' continues. Once the
#' diagram is compact, it will be slightly embellished. This function is different in use and
#' output from toVenn, which will probably deprecated.plotVenn
#'
#' @param sNames List of set names, in the same order as the input lists. If the input has tables or
#' data frames and the name exists, it will select the corresponding column.
#' @param nVennObj Object returned from previous run. If provided, the function will improve the
#' diagram by running more cycles on the previous result.
#' @param nCycles Number of cycles for the simulation. For up to 4 sets, the default number of 7000
#' should be enough. Even for more complex scenarios, it may be better to run the function repeatedly,
#' as a large number of cycles may take up too many resources.
#' @param showPlot Show the result in the graphic device.
#' @param showProgress Show progress bar during computation.
#' @param ... One list or vector (possibly mixed) per set. The function also accepts tables and data frames.
#' If input lists have names, those names will be used for the legend. If not, names can be
#' provided with \code{sNames}.
#' @return nVennObj with the result of the simulation. As a side effect, The result is drawn in the
#' plot window.
#' @examples
#' set1 <- list(set1 = c('a', 'b', 'c'))
#' set2 <- list(set2 = c('e', 'f', 'c'))
#' set3 <- list(set3 = c('c', 'b', 'e'))
#' mySVG <- plotVenn(set1, set2, set3, sNames=c("One", "Two", "Three"))
#' showSVG(mySVG=mySVG, opacity=0.2)
#' @export
plotVenn <- function(..., nVennObj=NULL, nCycles=7000, sNames=NULL,
                     showPlot=T, showProgress=F){
  sets <- list(...)
  nBits <- length(sets)
  lresult <- NULL
  if (is.null(nVennObj)){
    if (nBits == 0){
      stop("You must provide at least one list (and seriously consider providing more than one)")
    }
    nNames <- 0
    if (!missing(sNames)) nNames <- length(sNames)
    nRegions <- bitwShiftL(1, nBits)
    result <- c("nVenn1.2", toString(nBits))
    for (i in 1:nBits){
      cname <- paste('name', i, sep='')
      if (nNames >= i && sNames[[i]] != ""){
        cname = sNames[[i]]
      }
      if (length(names(sets[[i]])) > 0){
        cname <- names(sets[[i]])
      }
      result <- c(result, cname)
    }
    al <- unlist(sets[[1]])
    for (i in 2:nBits){
      al <- union(al, unlist(sets[[i]]))
    }
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
      result <- c(result, length(start))
    }
    lresult <- list(def=result)
  }
  else{
    lresult <- nVennObj
  }
  myVenn <- makeVenn(lresult, nCycles, showProgress)
  class(myVenn) <- append(class(myVenn), "nVennObj")
  if (showPlot == T) showSVG(myVenn, showProgress = showProgress)
  return(myVenn)
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
