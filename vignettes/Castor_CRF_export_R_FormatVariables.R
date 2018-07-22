## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(comment = NA, message = FALSE, warning = FALSE, width = 100)
knitr::opts_chunk$set(fig.align = "center", fig.height = 4, fig.width = 6)
knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_chunk$set(cache = TRUE, autodep=TRUE)  #$
knitr::opts_chunk$set(cache = FALSE) #, autodep=TRUE)  #$

## ---- eval=FALSE---------------------------------------------------------
## devtools::install_github("erikerhardt/CastorR")

## ------------------------------------------------------------------------
library(CastorR)
dat <- castor_format_R(fn = "CRF_export_2.xlsx")
str(dat)

