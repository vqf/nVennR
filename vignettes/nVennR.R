## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- dev='svg', fig.show='hold', fig.keep='all'-------------------------
library(nVennR)
exampledf
sas <- subset(exampledf, SAS == "Y")$Employee
python <- subset(exampledf, Python == "Y")$Employee
rr <- subset(exampledf, R == "Y")$Employee
myV <- toVenn(list(sas, python, rr), sNames = c('SAS', '', 'R')) #Second name empty
myV <- toVenn(list(SAS=sas, PYTHON=python, R=rr))


## ---- dev='svg', fig.show='hold', fig.keep='all'-------------------------
#myV <- toVenn(list(SAS=sas, PYTHON=python, R=rr, c("A006", "A008", "A011", "Unk"), c("A011", "Unk", "A101", "A006", "A000"), c("A101", "A006", "A008")))
#myV <- toVenn(nVennObj = myV)


## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

