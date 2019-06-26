From data preprocessing to retrieval
================

Here we provide examples of data preprocessing on CMAP using *dbplyr*. The package facilitates SQL functions like *GROUP BY*, *ORDER*, *SUBSET* using *group\_by*, *arrange*, *filter* subroutines. In addition to this, *summarize* module of the package facilitates column operations.

``` r
library(DBI,odbc)
library(dplyr)
library(dbplyr)
library(magrittr)
source('~/Dropbox (Simons Foundation)/CMAP/dataAccess/CMAP-Access/getAttributeFun.R')
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")


## Input: Table name; variable name, space time range information
table.name = 'tblsst_AVHRR_OI_NRT'      # table name
sel.var = 'sst'                       # choose variable 
range.var <- list()
range.var$lat <- c(10,70)
range.var$lon <- c(-180,-80)
range.var$time <- c('2016-04-30', '2016-04-30')
# range.var$depth <- c(0,0.5)


## Subset selection
tbl.connect <- tbl(con,table.name)
rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]))

# Data retrieval step
tbl.subset <-  tbl.query %>% collect()              
head(tbl.subset)
```

    ## # A tibble: 6 x 4
    ##     lat   lon time         sst
    ##   <dbl> <dbl> <date>     <dbl>
    ## 1  10.1 -180. 2016-04-30  27.9
    ## 2  10.1 -180. 2016-04-30  27.9
    ## 3  10.1 -179. 2016-04-30  27.8
    ## 4  10.1 -179. 2016-04-30  27.8
    ## 5  10.1 -179. 2016-04-30  27.8
    ## 6  10.1 -179. 2016-04-30  27.8

``` r
## Ordering of the numeric variable:
ord.var <- c('time')                       # 
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%
   arrange(desc(sql(ord.var[1])))

# Data retrieval step
tbl.subset <-  tbl.query %>% collect()              
head(tbl.subset)
```

    ## # A tibble: 6 x 4
    ##     lat   lon time         sst
    ##   <dbl> <dbl> <date>     <dbl>
    ## 1  10.1 -180. 2016-04-30  27.9
    ## 2  10.1 -180. 2016-04-30  27.9
    ## 3  10.1 -179. 2016-04-30  27.8
    ## 4  10.1 -179. 2016-04-30  27.8
    ## 5  10.1 -179. 2016-04-30  27.8
    ## 6  10.1 -179. 2016-04-30  27.8

``` r
## Mutate a column and change define a new one:
## mutate function allow us to perform operations like +, *, -, / and many more on elements of a column variable.
ord.var <- c('time')                       # 
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%
    mutate( temVar1 = sst^2,temVar2 = sst/2, temVar3 = sst/mean(sst,na.rm = TRUE)) %>%              
    arrange(desc(sql(ord.var[1])))

tbl.query %>% show_query()

# Data retrieval step
tbl.subset <-  tbl.query %>% collect()              
head(tbl.subset)
```

    ## # A tibble: 6 x 7
    ##     lat   lon time         sst temVar1 temVar2 temVar3
    ##   <dbl> <dbl> <date>     <dbl>   <dbl>   <dbl>   <dbl>
    ## 1  10.1 -180. 2016-04-30  27.9    779.    14.0    1.72
    ## 2  10.1 -180. 2016-04-30  27.9    778.    13.9    1.72
    ## 3  10.1 -179. 2016-04-30  27.8    776.    13.9    1.72
    ## 4  10.1 -179. 2016-04-30  27.8    774.    13.9    1.72
    ## 5  10.1 -179. 2016-04-30  27.8    773.    13.9    1.71
    ## 6  10.1 -179. 2016-04-30  27.8    772.    13.9    1.71

``` r
## Aggregation of the variable:
## Aggregating data collected at different time point:
table.name <- 'tblSST_AVHRR_OI_NRT'
sel.var <- 'sst'                                  
range.var <- list()
range.var$lat <- c(25,30)
range.var$lon <- c(-160,-155)
range.var$time <- c('2016-03-29', '2016-05-29')         ##
# range.var$depth <- c(0,5)


rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')

agg.var <- 'time'               ## aggregation variable 
indaggvar <-  match( agg.var, field.var)    ## index of the aggregating variable
tbl.query <- tbl.connect %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%  
      select(indsel) %>%
  group_by_at(indaggvar) %>%
  summarise_at( tbl.var[setdiff(indsel,indaggvar)],mean,na.rm=TRUE) %>%
  select(c(agg.var,sel.var))

tbl.query %>% show_query()

# Data retrieval step
tbl.subset <-  tbl.query %>% collect()              
head(tbl.subset)
```

    ## # A tibble: 6 x 2
    ##   time         sst
    ##   <date>     <dbl>
    ## 1 2016-04-28  23.1
    ## 2 2016-04-23  21.8
    ## 3 2016-05-08  21.9
    ## 4 2016-05-27  23.8
    ## 5 2016-04-02  20.8
    ## 6 2016-05-06  22.7

``` r
## Custom - funs - min max sd, 
table.name <- 'tblSST_AVHRR_OI_NRT'
sel.var <- 'sst'                                  
range.var <- list()
range.var$lat <- c(25,30)
range.var$lon <- c(-160,-155)
range.var$time <- c('2016-03-29', '2016-05-29')         ##
# range.var$depth <- c(0,5)


rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')

agg.var <- 'time'               ## aggregation variable 
indaggvar <-  match( agg.var, field.var)    ## index of the aggregating variable
tbl.query <- tbl.connect %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%  
      select(indsel) %>%
  group_by_at(indaggvar) %>%
  select(c(agg.var,sel.var)) %>%
  summarise_at( vars(sel.var),funs(min, count_big, mean , max)) 

tbl.query %>% show_query()

# Data retrieval step
tbl.subset <-  tbl.query %>% collect()              
head(tbl.subset)
```

    ## # A tibble: 6 x 5
    ##   time         min count_big        mean   max
    ##   <date>     <dbl> <S3: integer64> <dbl> <dbl>
    ## 1 2016-04-28  21.7 400              23.1  24.8
    ## 2 2016-04-20  18.9 400              21.4  23.6
    ## 3 2016-05-24  21.0 400              23.4  24.5
    ## 4 2016-04-07  18.3 400              20.7  23.9
    ## 5 2016-05-11  19.2 400              22.0  24.3
    ## 6 2016-03-30  17.9 400              20.6  23.2

``` r
dbDisconnect(con)
```
