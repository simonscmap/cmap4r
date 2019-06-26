Depth profiles map
================

Example I
---------

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
# Inpit variable:
# 
table.list <- c('tblArgoMerge_REP', 'tblPisces_NRT', 'tblDarwin_Chl_Climatology') 
var.list <-  c('argo_merge_chl_adj', 'CHL', 'chl01_darwin_clim')    
selIndex <- 1
table.name <- table.list[selIndex]
sel.var <- var.list[selIndex]   
#
range.var <- list()
range.var$lat <- c(20,24)
range.var$lon <- c( -170, -150)
range.var$depth <- c(0, 1500)
range.var$time <- c('2016-04-30', '2016-04-30')
#
## Subset selection: data retrieval
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
             between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]),
             between(sql(rvarName[4]), range.var[[rvarName[4]]][1], range.var[[rvarName[4]]][2])) %>%
  group_by_at(groupByIndex) %>%
  summarise_at( sql(sel.var),mean,na.rm=TRUE) %>%
  arrange(sql(agg.var)  )
  
tbl.query %>% show_query()
# Data retrieval step
tbl.subset <-  tbl.query %>% collect()  
head(tbl.subset,20)                           ## sample data 
```

    ## # A tibble: 20 x 2
    ##    depth argo_merge_chl_adj
    ##    <dbl>              <dbl>
    ##  1  4.10            NA     
    ##  2  4.20            NA     
    ##  3  5.90            NA     
    ##  4  6               NA     
    ##  5  7.20             0.0180
    ##  6  7.80            NA     
    ##  7  7.90            NA     
    ##  8  8               NA     
    ##  9 10               NA     
    ## 10 11.7             NA     
    ## 11 11.8              0.0180
    ## 12 12               NA     
    ## 13 14               NA     
    ## 14 16               NA     
    ## 15 16.2             NA     
    ## 16 16.7              0.0108
    ## 17 18               NA     
    ## 18 20               NA     
    ## 19 21.5              0.0216
    ## 20 21.8             NA

``` r
## Plot -- Depth profiles:
tbl.subset <- data.frame(tbl.subset)
p <- plot_ly(x = ~tbl.subset$depth, y = ~tbl.subset[,sel.var], mode = 'lines+markers', type = 'scatter')
p%>% layout(title='Depth Profile Plot',
         xaxis = list(title = 'Depth'),
         yaxis = list(title= sel.var))
```

<img src="CMAP-depthProfile_files/figure-markdown_github/unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```
