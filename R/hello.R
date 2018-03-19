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

#' Create Venn diagram using the nVenn algorithm.
#'
#' @param ... One list or vector (possibly mixed) per set. If the input
#' is a list with a name, that name will be used for the legend
#' @return Nothing. Creates a Venn diagram in svg as a side effect.
#' @examples
#' set1 <- c('a', 'b', 'c')
#' set2 <- c('e', 'f', 'c')
#' set3 <- c('c', 'b', 'e')
#' toVenn(set1, set2, set3)
toVenn <- function(...){
  sets <- list(...)
  nBits <- length(sets)
  nRegions <- bitwShiftL(1, nBits)
  result <- c("nVenn1.2", toString(nBits))
  for (i in 1:nBits){
    cname <- paste('name', i, sep='')
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
  cat(result, sep="\n")
  mySVG <- drawVenn(result)
  tfile <- "tmp.svg"
  cat(mySVG, file=tfile)
  image_read(tfile)
  #viewer <- getOption("viewer")
  #if (!is.null(viewer)){
  #  viewer(tfile)
  #}
  #else{
  #  utils::browseURL(tfile)
  #}
  #cat("Saved to ", tfile)
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
