Histogram plot
================

Example I
---------

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
# Inpit variable:
# 
table.list <- c('tblSST_AVHRR_OI_NRT', 'tblArgoMerge_REP', 'tblArgoMerge_REP') 
var.list <-  c('sst', 'argo_merge_temperature_adj', 'argo_merge_salinity_adj')   
selIndex <- 1
table.name <- table.list[selIndex]
sel.var <- var.list[selIndex]   
#
range.var <- list()
range.var$lat <- c(20, 24)
range.var$lon <- c(-170, 150)
# range.var$depth <- c(0, 1500)
range.var$time <- c('2016-04-30', '2016-04-30')

tbl.connect <- tbl(con,table.name)
rvarName <- names(range.var)
tbl.var <- c(rvarName,sel.var)
field.var <- dbListFields(con,table.name)
indsel <- match( tbl.var, field.var)
if(any(is.na(indsel)))
  stop('Check range and select variable:')
agg.var <- 'depth'                          ## aggregation variable 
indaggvar <-  match( agg.var, field.var)    ## index of the aggregating variable
summariseIndex <- match(setdiff(indsel,indaggvar),indsel)
groupByIndex <- which(indaggvar == indsel)
tbl.query <- tbl.connect %>%
    select(indsel) %>%
      filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
             between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]))

tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  
head(tbl.subset,20)                           ## sample data 
```

    ## # A tibble: 20 x 4
    ##      lat   lon time         sst
    ##    <dbl> <dbl> <date>     <dbl>
    ##  1  20.1 -170. 2016-04-30  25.7
    ##  2  20.1 -170. 2016-04-30  25.7
    ##  3  20.1 -169. 2016-04-30  25.8
    ##  4  20.1 -169. 2016-04-30  25.8
    ##  5  20.1 -169. 2016-04-30  25.9
    ##  6  20.1 -169. 2016-04-30  25.9
    ##  7  20.1 -168. 2016-04-30  25.8
    ##  8  20.1 -168. 2016-04-30  25.7
    ##  9  20.1 -168. 2016-04-30  25.6
    ## 10  20.1 -168. 2016-04-30  25.6
    ## 11  20.1 -167. 2016-04-30  25.6
    ## 12  20.1 -167. 2016-04-30  25.7
    ## 13  20.1 -167. 2016-04-30  25.9
    ## 14  20.1 -167. 2016-04-30  26.1
    ## 15  20.1 -166. 2016-04-30  26.2
    ## 16  20.1 -166. 2016-04-30  26.2
    ## 17  20.1 -166. 2016-04-30  26.1
    ## 18  20.1 -166. 2016-04-30  26.1
    ## 19  20.1 -165. 2016-04-30  26.0
    ## 20  20.1 -165. 2016-04-30  26.0

``` r
tbl.subset <- data.frame(tbl.subset)
p <- plot_ly(x = ~tbl.subset$sst[!is.na(tbl.subset$sst)],
             type = "histogram",
             histnorm = "probability")
p %>% layout(title='Histogram',
         xaxis = list(title = sel.var),
         yaxis = list(title= 'Density'))
```

<img src="CMAP-histogramPlot_files/figure-markdown_github/unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```
