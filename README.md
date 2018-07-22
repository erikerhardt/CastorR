# CastorR
Format Castor EDC CRF_export.xlsx file for R

Install remotely with the following line:
```
devtools::install_github("erikerhardt/CastorR")
```

See vignette for details.  Basic use is seen below.
```
library(CastorR)
dat <- castor_format_R(fn = "CRF_export.xlsx")
str(dat)
```
