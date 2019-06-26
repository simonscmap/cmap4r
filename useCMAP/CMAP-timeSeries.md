Time series map
================

Example I
---------

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
# Inpit variable:
# 
table.list <- c('tblSST_AVHRR_OI_NRT', 'tblAltimetry_REP', 'tblPisces_NRT')  
var.list <- c('sst', 'sla', 'NO3')   
selIndex <- 1
table.name <- table.list[selIndex]
sel.var <- var.list[selIndex]   


range.var <- list()
range.var$lat <- c(25,30)
range.var$lon <- c(-160,-155)
# range.var$depth <- c(0,5)
range.var$time <- c('2016-03-29', '2016-05-29')

## Subset selection: data retrieval
tbl.connect <- tbl(con,table.name)
rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')
agg.var <- 'time'                           ## aggregation variable 
indaggvar <-  match( agg.var, field.var)    ## index of the aggregating variable
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%
  group_by_at(indaggvar) %>%
  summarise_at( tbl.var[setdiff(indsel,indaggvar)],mean,na.rm=TRUE) %>%
  arrange(( sql(agg.var)  ))
  
tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  
head(tbl.subset,20)                           ## sample data 
```

    ## # A tibble: 20 x 4
    ##    time         lat   lon   sst
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-03-29  27.5 -158.  21.3
    ##  2 2016-03-30  27.5 -158.  20.6
    ##  3 2016-03-31  27.5 -158.  20.6
    ##  4 2016-04-01  27.5 -158.  20.7
    ##  5 2016-04-02  27.5 -158.  20.8
    ##  6 2016-04-03  27.5 -158.  20.9
    ##  7 2016-04-04  27.5 -158.  21.1
    ##  8 2016-04-05  27.5 -158.  20.9
    ##  9 2016-04-06  27.5 -158.  20.9
    ## 10 2016-04-07  27.5 -158.  20.7
    ## 11 2016-04-08  27.5 -158.  20.9
    ## 12 2016-04-09  27.5 -158.  21.0
    ## 13 2016-04-10  27.5 -158.  21.2
    ## 14 2016-04-11  27.5 -158.  21.1
    ## 15 2016-04-12  27.5 -158.  21.1
    ## 16 2016-04-13  27.5 -158.  21.1
    ## 17 2016-04-14  27.5 -158.  21.2
    ## 18 2016-04-15  27.5 -158.  21.4
    ## 19 2016-04-16  27.5 -158.  21.4
    ## 20 2016-04-17  27.5 -158.  21.4

``` r
## Plot -- Time series:
tbl.subset <- data.frame(tbl.subset)
p <- plot_ly(x = ~tbl.subset$time, y = ~tbl.subset[,sel.var], 
             mode = 'lines+markers', type = 'scatter')
p %>% layout(title='Time Series Plot',
         xaxis = list(title = 'Time'),
         yaxis = list(title= sel.var))
```

<img src="CMAP-timeSeries_files/figure-markdown_github/unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
selIndex <- 2
table.name <- table.list[selIndex]
sel.var <- var.list[selIndex] 


## Subset selection: data retrieval
tbl.connect <- tbl(con,table.name)
rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')
agg.var <- 'time'                           ## aggregation variable 
indaggvar <-  match( agg.var, field.var)    ## index of the aggregating variable
summariseIndex <- match(setdiff(indsel,indaggvar),indsel)
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2])) %>%
  group_by_at(indaggvar) %>%
  summarise_at( tbl.var[summariseIndex], mean,na.rm=TRUE) %>%
  arrange(( sql(agg.var)  ))
  
tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  
head(tbl.subset,20)                           ## sample data 
```

    ## # A tibble: 20 x 4
    ##    time         lat   lon    sla
    ##    <date>     <dbl> <dbl>  <dbl>
    ##  1 2016-03-29  27.5 -158. 0.0242
    ##  2 2016-03-30  27.5 -158. 0.0228
    ##  3 2016-03-31  27.5 -158. 0.0215
    ##  4 2016-04-01  27.5 -158. 0.0208
    ##  5 2016-04-02  27.5 -158. 0.0197
    ##  6 2016-04-03  27.5 -158. 0.0189
    ##  7 2016-04-04  27.5 -158. 0.0180
    ##  8 2016-04-05  27.5 -158. 0.0173
    ##  9 2016-04-06  27.5 -158. 0.0167
    ## 10 2016-04-07  27.5 -158. 0.0161
    ## 11 2016-04-08  27.5 -158. 0.0155
    ## 12 2016-04-09  27.5 -158. 0.0149
    ## 13 2016-04-10  27.5 -158. 0.0141
    ## 14 2016-04-11  27.5 -158. 0.0135
    ## 15 2016-04-12  27.5 -158. 0.0131
    ## 16 2016-04-13  27.5 -158. 0.0128
    ## 17 2016-04-14  27.5 -158. 0.0128
    ## 18 2016-04-15  27.5 -158. 0.0125
    ## 19 2016-04-16  27.5 -158. 0.0122
    ## 20 2016-04-17  27.5 -158. 0.0122

``` r
## Plot -- Time series:
tbl.subset <- data.frame(tbl.subset)
p <- plot_ly(x = ~tbl.subset$time, y = ~tbl.subset[,sel.var], 
             mode = 'lines+markers', type = 'scatter')
p %>% layout(title='Time Series Plot',
         xaxis = list(title = 'Time'),
         yaxis = list(title= sel.var))
```

<img src="CMAP-timeSeries_files/figure-markdown_github/unnamed-chunk-1-2.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```
