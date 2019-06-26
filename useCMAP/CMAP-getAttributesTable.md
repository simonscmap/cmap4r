Get attributes of a table
================

Attributes of a table includes a) type of variables, i.e., quantitative, qualitative, time, b) size of the table, c) five number summary of numberical variables;

``` r
library(DBI,odbc)
library(dplyr)
library(dbplyr)
library(magrittr)
source('~/Dropbox (Simons Foundation)/CMAP/dataAccess/CMAP-Access/getAttributeFun.R')
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")

## Choose table:
table.name <-  "tblsst_AVHRR_OI_NRT"
# Variable name in the table
tbl.fields <- dbListFields(con,table.name)


# Space time information of the table
tbl.spaceTimeInfo <- getSpaceTimeRange(con,table.name)
print(tbl.spaceTimeInfo)
```

    ## # A tibble: 1 x 6
    ##   MaxLat MinLat MaxLon MinLon MaxTime    MinTime   
    ##    <dbl>  <dbl>  <dbl>  <dbl> <date>     <date>    
    ## 1   89.9  -89.9   180.  -180. 2019-04-21 2009-01-01

``` r
# Class of each column in the table
tbl.colClass <- getColClass(con,table.name)
print(tbl.colClass)
```

    ##   Variable      Type
    ## 1      lat   numeric
    ## 2      lon   numeric
    ## 3     time      Date
    ## 4      sst   numeric
    ## 5       ID integer64

``` r
# Numeric variable range:
var.name <- c('lat','lon','sst')
tbl.rangeNumvar <- getRangeNumVar(con,table.name,var.name)
print(tbl.rangeNumvar)
```

    ##         Max         Min Variable
    ## 1  89.87500  -89.875000      lat
    ## 2 179.87500 -179.875000      lon
    ## 3  38.14999   -1.800006      sst

``` r
dbDisconnect(con)
```
