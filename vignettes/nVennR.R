## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----"plotVenn", dev='svg', fig.show='hold', fig.keep='all'--------------
library(nVennR)
exampledf
sas <- subset(exampledf, SAS == "Y")$Employee
python <- subset(exampledf, Python == "Y")$Employee
rr <- subset(exampledf, R == "Y")$Employee
myV <- plotVenn(list(sas, python, rr), sNames = c('SAS', '', 'R')) #Second name empty
myV <- plotVenn(list(SAS=sas, PYTHON=python, R=rr))


## ----"Iterative", dev='svg', fig.show='hold', fig.keep='all'-------------
myV2 <- plotVenn(list(SAS=sas, PYTHON=python, R=rr, c("A006", "A008", "A011", "Unk"), c("A011", "Unk", "A101", "A006", "A000"), c("A101", "A006", "A008")))
myV2 <- plotVenn(nVennObj = myV2)


## ----"Low-level", dev='svg', fig.show='hold', fig.keep='all'-------------
myV3 <- createVennObj(nSets = 5, sSizes = c(rep(1, 32)))
myV3 <- plotVenn(nVennObj = myV3)
myV3 <- plotVenn(nVennObj = myV3)

## ----"setVennRegion", dev='svg'------------------------------------------
myV3 <- setVennRegion(myV3, region = c("Group1", "Group3", "Group4"), value = 4) # region equivalent to c(1, 0, 1, 1, 0)
myV3 <- setVennRegion(myV3, region = c(0, 1, 0, 0, 1), value = 8) # region equivalent to c("Group2", "Group5")
myV3 <- plotVenn(nVennObj = myV3)

## ----"opacity", dev='svg', fig.show='hold', fig.keep='all'---------------
showSVG(nVennObj = myV3, opacity = 0.1)
showSVG(nVennObj = myV3, opacity = 0.1, borderWidth = 3)

## ----"setColors", dev='svg', fig.show='hold', fig.keep='all'-------------
showSVG(nVennObj = myV3, setColors = c('#d7100b', '#ff4e02', '#eeed2f', '#067e1d', '#2b55b7'))
showSVG(nVennObj = myV3, setColors = c('#d7100b', 'teal', 'yellow', 'black', '#2b55b7'))

## ----"showLabels", dev='svg', fig.show='hold', fig.keep='all'------------
showSVG(nVennObj = myV3, opacity = 0.1, showNumbers = F, fontScale = 2) # Avoid overlaps by hiding size labels
showSVG(nVennObj = myV3, opacity = 0.1, labelRegions = F, fontScale = 3) # Avoid overlaps by hiding region labels

## ----"getVennRegion"-----------------------------------------------------
getVennRegion(myV, c("R", "SAS"))
getVennRegion(myV, c(1, 1, 1))

## ----"listVennRegions"---------------------------------------------------
listVennRegions(myV)
listVennRegions(myV, na.rm = F)

