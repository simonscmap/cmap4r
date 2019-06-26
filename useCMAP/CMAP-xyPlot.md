XY Plot
================

``` r
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
# Inpit variable:
# 
table.list <- c('tblSST_AVHRR_OI_NRT', 'tblAltimetry_REP') 
var.list <-  c('sst', 'sla')  
#
range.var <- list()
range.var$lat <- c(25,30)
range.var$lon <- c(-160, -155)
# range.var$depth <- c(0,5)
range.var$time <- c('2016-03-29', '2016-05-29')


## Dataset from table II
selIndex <- 1
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
tbl.subset1 <-  tbl.query %>% collect()  




## Dataset from table II
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
tbl.subset2 <-  tbl.query %>% collect()  




## Merge two columns:
df.plot <-  merge(tbl.subset1,tbl.subset2, agg.var)

## Plot - XY
p <- plot_ly(x = ~df.plot$sst, y = ~df.plot$sla, mode = 'markers', type = 'scatter')
p%>% layout(title='XY Plot',
         xaxis = list(title = var.list[1]),
         yaxis = list(title= var.list[2]))
```

<img src="CMAP-xyPlot_files/figure-markdown_github/unnamed-chunk-1-1.png" width="100%" style="display: block; margin: auto;" />

``` r
dbDisconnect(con)
```
