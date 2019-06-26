Get attributes subset of a table
================

Here we provide attributes subset of a table based on space time range information.

Attributes of a table includes a) type of variables, i.e., quantitative, qualitative, time, b) size of the table, c) five number summary of numberical variables;

``` r
library(DBI,odbc)
library(dplyr)
library(dbplyr)
library(magrittr)
source('~/Dropbox (Simons Foundation)/CMAP/dataAccess/CMAP-Access/getAttributeFun.R')
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")




## Input: Table name; variable name, space time range information
## 
table.name = 'tblsst_AVHRR_OI_NRT'      # table name
variables = 'sst'                       # choose variable 
range.var <- list()
range.var$date <- c('2016-04-30', '2016-04-30')
range.var$lat <- c(10,70)
range.var$lon <- c(-180,-80)
# depth.range <- c(0,0.5)

tbl.connect <- tbl(con,table.name)


## Space, time,  number of observations summary
tbl.subsetSpaceTimeSummary <- tbl.connect %>%
  filter(between(time, range.var$date[1], range.var$date[2]),
         between(lat,range.var$lat[1],range.var$lat[2]),
         between(lon,range.var$lon[1],range.var$lon[2])) %>%
  summarise(nObs = n(), MaxLat = max(lat, na.rm = TRUE),
      MinLat = min(lat, na.rm = TRUE),
      MaxLon = max(lon, na.rm = TRUE),
      MinLon = min(lon, na.rm = TRUE),
      MaxTime = max(time, na.rm = TRUE),
      MinTime = min(time, na.rm = TRUE)) %>% collect()

tbl.subsetSpaceTimeSummary
```

    ## # A tibble: 1 x 7
    ##    nObs MaxLat MinLat MaxLon MinLon MaxTime    MinTime   
    ##   <int>  <dbl>  <dbl>  <dbl>  <dbl> <date>     <date>    
    ## 1 96000   69.9   10.1  -80.1  -180. 2016-04-30 2016-04-30

``` r
# Summary of the data:
var.name <- c('lat','lon','sst')
tbl.subsetSummar <- getSubsetRangeNumVar(con,table.name,range.var,var.name)
tbl.subsetSummar
```

    ##   Variable       Max         Min
    ## 1      lat  69.87500   10.125000
    ## 2      lon -80.12500 -179.875000
    ## 3      sst  31.41999   -1.800006

``` r
dbDisconnect(con)
```
