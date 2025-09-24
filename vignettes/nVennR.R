## ----setup, F, include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.show='hide'
)
library(knitr)
uuid <- function() {
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- toupper(hex_digits)
  paste(sample(hex_digits, 8), collapse='')
}

subsuid <- function(regex, strng){
  l <- gregexpr(regex, strng, perl = T)
  for (x in regmatches(strng, l)){ 
    m <- regexpr('([^\\{ \\.\\#]+)', x, perl = T)
    names <- regmatches(x, m)
    gstr = strng
    for (name in names){
      nname <- paste('([^\\d\\w<>]', name, ')', sep="")
      gstr <- gsub(nname, paste('\\1', '_', uuid(), sep=""), gstr, perl = T) 
    }
    return(gstr)
  }
}

knit_print.nVennR = function(x, ...) {
  if (is.null(x$svg)){
    x <- showSVG(x)
  }
  s <- subsuid('[\\.\\#](.+?)\\s*\\{', x$svg)
  s <- subsuid('bl\\d+', s)
  knitr::asis_output(s)
}
# register the method
registerS3method("knit_print", "nVennObj", knit_print.nVennR)
local({
  hook_source <- knitr::knit_hooks$get('source')
  knitr::knit_hooks$set(source = function(x, options) {
    x <- x[!grepl('#noshow$', x)]
    hook_source(x, options)
  })
})

## ----"plotVenn"---------------------------------------------------------------
library(nVennR)
exampledf
sas <- subset(exampledf, SAS == "Y")$Employee
python <- subset(exampledf, Python == "Y")$Employee
rr <- subset(exampledf, R == "Y")$Employee
myV <- plotVenn(list(SAS=sas, PYTHON=python, R=rr), nCycles = 2000)
myV #noshow


## ----"Iterative"--------------------------------------------------------------
myV2 <- plotVenn(list(SAS=sas, PYTHON=python, R=rr, c("A006", "A008", "A011", "Unk"), c("A011", "Unk", "A101", "A006", "A000"), c("A101", "A006", "A008")))
myV2 <- plotVenn(nVennObj = myV2)
myV2 #noshow


## ----"Low-level"--------------------------------------------------------------
myV3 <- createVennObj(nSets = 5, sSizes = c(rep(1, 32)))
myV3 <- plotVenn(nVennObj = myV3, nCycles = 5000)
myT <- myV3 #noshow
myV3 <- plotVenn(nVennObj = myV3, nCycles = 5000)
myT #noshow
myV3 #noshow

## ----"setVennRegion"----------------------------------------------------------
myV3 <- setVennRegion(myV3, region = c("Group1", "Group3", "Group4"), value = 4) # region equivalent to c(1, 0, 1, 1, 0)
myV3 <- setVennRegion(myV3, region = c(0, 1, 0, 0, 1), value = 8) # region equivalent to c("Group2", "Group5")
myV3 <- plotVenn(nVennObj = myV3, nCycles = 3000)
myV3 #noshow

## ----"opacity"----------------------------------------------------------------
showSVG(nVennObj = myV3, opacity = 0.1, borderWidth = 3)

## ----"setColors"--------------------------------------------------------------
showSVG(nVennObj = myV3, setColors = c('#d7100b', 'teal', 'yellow', 'black', '#2b55b7'))

## ----"showLabels"-------------------------------------------------------------
showSVG(nVennObj = myV3, opacity = 0.1, labelRegions = F, fontScale = 3) # Avoid overlaps by hiding region labels

## ----"directPlot"-------------------------------------------------------------
myV4 <- plotVenn(list(a=c(1, 2, 3), b=c(3, 4, 5), c=c(3, 6, 1)), nCycles = 2000, setColors=c('red', 'green', 'blue'), labelRegions=F, fontScale=2, opacity=0.2, borderWidth=2)
myV4 #noshow

## ----"getVennRegion"----------------------------------------------------------
getVennRegion(myV, c("R", "SAS"))
getVennRegion(myV, c(1, 1, 1))

## ----"listVennRegions"--------------------------------------------------------

listVennRegions(myV4)
listVennRegions(myV4, na.rm = F)

