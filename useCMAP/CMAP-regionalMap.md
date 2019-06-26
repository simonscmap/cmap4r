Regional map
================

Example I
---------

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
# Inpit variable:
# 
table.name = 'tblsst_AVHRR_OI_NRT'   
sel.var = 'sst'                 
range.var <- list()
range.var$lat <- c(10,70)
range.var$lon <- c(-180,-80)
range.var$time <- c('2016-04-30', '2016-04-30')
# range.var$depth <- c(0,0.5)


## Subset selection: data retrieval
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
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]))%>%
  arrange(desc( sql("lat , lon")  ))
tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  

head(tbl.subset,20)  ## sample data 
```

    ## # A tibble: 20 x 4
    ##      lat   lon time         sst
    ##    <dbl> <dbl> <date>     <dbl>
    ##  1  10.1 -80.1 2016-04-30  29.6
    ##  2  10.1 -80.4 2016-04-30  29.8
    ##  3  10.1 -80.6 2016-04-30  29.9
    ##  4  10.1 -80.9 2016-04-30  29.9
    ##  5  10.1 -81.1 2016-04-30  29.8
    ##  6  10.1 -81.4 2016-04-30  29.8
    ##  7  10.1 -81.6 2016-04-30  29.7
    ##  8  10.1 -81.9 2016-04-30  29.6
    ##  9  10.1 -82.1 2016-04-30  29.5
    ## 10  10.1 -82.4 2016-04-30  29.4
    ## 11  10.1 -82.6 2016-04-30  29.4
    ## 12  10.1 -82.9 2016-04-30  29.5
    ## 13  10.1 -83.1 2016-04-30  29.5
    ## 14  10.1 -83.4 2016-04-30  NA  
    ## 15  10.1 -83.6 2016-04-30  NA  
    ## 16  10.1 -83.9 2016-04-30  NA  
    ## 17  10.1 -84.1 2016-04-30  NA  
    ## 18  10.1 -84.4 2016-04-30  NA  
    ## 19  10.1 -84.6 2016-04-30  NA  
    ## 20  10.1 -84.9 2016-04-30  NA

``` r
## Plot - regional map 
library(plotly)
lat.ref <- data.frame(lat=unique(tbl.subset$lat))
lat.ref$ind <- 1:nrow(lat.ref)
lon.ref <- data.frame(lon=unique(tbl.subset$lon))
lon.ref$ind <- 1:nrow(lon.ref)
ind.lat <- match(tbl.subset$lat,lat.ref$lat)
ind.lon <- match(tbl.subset$lon,lon.ref$lon)

sst.mat <- matrix(NA,nrow = nrow(lon.ref), ncol = nrow(lat.ref))
for (i in 1:length(ind.lon)) 
  sst.mat[ind.lon[i],ind.lat[i]] <- tbl.subset$sst[i]


p <- plot_ly(
  x = lon.ref$lon, 
  y = lat.ref$lat, 
  z = t(sst.mat), 
  # type = "contour" 
  type = "heatmap"
)
p %>% layout(title='Regional map', 
             xaxis = list(title = 'Longitude'),
             yaxis = list(title= 'Latitude'))
```

<img src="CMAP-regionalMap_files/figure-markdown_github/unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```

Example II
----------

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
library(plotly)
library(magrittr)


table.name <- "tblPisces_NRT"   
sel.var <-  "Fe"               
range.var <- list()
range.var$lat <- c(10,70)
range.var$lon <- c(-180,-80)
range.var$depth <- c(0,0.5)
range.var$time <- c('2016-04-30', '2016-04-30')



## Subset selection: data retrieval
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
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]),
             between(sql(rvarName[4]), range.var[[rvarName[4]]][1], range.var[[rvarName[4]]][2]))%>%
  arrange(desc( sql("lat , lon")  ))
tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  

head(tbl.subset,20)                           ## sample data 
```

    ## # A tibble: 20 x 5
    ##      lat   lon depth time               Fe
    ##    <dbl> <dbl> <dbl> <date>          <dbl>
    ##  1    10 -80   0.494 2016-04-30  0.000725 
    ##  2    10 -80.5 0.494 2016-04-30  0.000705 
    ##  3    10 -81   0.494 2016-04-30  0.000658 
    ##  4    10 -81.5 0.494 2016-04-30  0.000635 
    ##  5    10 -82   0.494 2016-04-30  0.000640 
    ##  6    10 -82.5 0.494 2016-04-30  0.000603 
    ##  7    10 -83   0.494 2016-04-30 NA        
    ##  8    10 -83.5 0.494 2016-04-30 NA        
    ##  9    10 -84   0.494 2016-04-30 NA        
    ## 10    10 -84.5 0.494 2016-04-30 NA        
    ## 11    10 -85   0.494 2016-04-30  0.000252 
    ## 12    10 -85.5 0.494 2016-04-30 NA        
    ## 13    10 -86   0.494 2016-04-30  0.0000383
    ## 14    10 -86.5 0.494 2016-04-30  0.0000150
    ## 15    10 -87   0.494 2016-04-30  0.0000150
    ## 16    10 -87.5 0.494 2016-04-30  0.0000150
    ## 17    10 -88   0.494 2016-04-30  0.0000153
    ## 18    10 -88.5 0.494 2016-04-30  0.0000179
    ## 19    10 -89   0.494 2016-04-30  0.0000167
    ## 20    10 -89.5 0.494 2016-04-30  0.0000151

``` r
# Plot - Regional map
# 
# unique(tbl.subset$depth)
lat.ref <- data.frame(lat=unique(tbl.subset$lat))
lat.ref$ind <- 1:nrow(lat.ref)
lon.ref <- data.frame(lon=unique(tbl.subset$lon))
lon.ref$ind <- 1:nrow(lon.ref)
ind.lat <- match(tbl.subset$lat,lat.ref$lat)
ind.lon <- match(tbl.subset$lon,lon.ref$lon)
Fe.mat <- matrix(NA,nrow = nrow(lon.ref), ncol = nrow(lat.ref))
for (i in 1:length(ind.lon)) {
  Fe.mat[ind.lon[i],ind.lat[i]] <- tbl.subset$Fe[i]
}
p <- plot_ly(
  z = t(Fe.mat), 
  x = lon.ref$lon,
  y = lat.ref$lat,
  type = "heatmap" 
) 
p %>%
  layout(title='Regional map',
         xaxis = list(title = 'Longitude'),
         yaxis = list(title= 'Latitude'))
```

<img src="CMAP-regionalMap_files/figure-markdown_github/unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```
