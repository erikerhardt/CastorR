# CastorR
Format Castor EDC export.xlsx file for R.  For data exported from https://www.castoredc.com/ platform.

Install remotely with the following line:
```
devtools::install_github("erikerhardt/CastorR")
```

See [vignette](https://github.com/erikerhardt/CastorR/blob/master/vignettes/Castor_CRF_export_R_FormatVariables.pdf) for details.  Basic use is seen below.
```
library(CastorR)
dat <- castor_format_R(fn = "CRF_export.xlsx")
str(dat)
```
