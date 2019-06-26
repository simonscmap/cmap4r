utils::globalVariables(c("..density.."))


#' Plot variable of interest with varying depth
#'
#' Depth plot object
#'
#' @param tbl.subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel.var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @import magrittr
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point theme xlab ylab aes_string element_text
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' # Inpit variable:
#' table.list <- c('tblArgoMerge_REP', 'tblPisces_NRT', 'tblDarwin_Chl_Climatology')
#' var.list <-  c('argo_merge_chl_adj', 'CHL', 'chl01_darwin_clim')
#' #
#' # selected argo_merge_chl_adj from tblArgoMerge_REP
#' selIndex <- 1
#' table.name <- table.list[selIndex]
#' sel.var <- var.list[selIndex]
#' #
#' range.var <- list()
#' range.var$lat <- c(20,24)
#' range.var$lon <- c( -170, -150)
#' range.var$depth <- c(0, 1500)
#' range.var$time <- c('2016-04-30', '2016-04-30')
#' #
#' #
#' # Subset selection: data retrieval
#' agg.var <- 'depth'
#' tbl.subset <- getAggregatedTableData(con, table.name, sel.var, range.var, agg.var)
#' head(tbl.subset)
#' #
#' #
#' # Plot -- Depth profiles:
#' p <- plot_depth(tbl.subset, 'plotly',sel.var)
#' #
#' #
#' dbDisconnect(con)
#' }
plot_depth = function(tbl.subset, type = c('plotly','ggplot')[1], sel.var){
  tbl.subset <- data.frame(tbl.subset)
  depInd <- match('depth',names(tbl.subset))
  if(is.na(depInd))
    stop('depth variable missing')
  sind <- match(sel.var,names(tbl.subset))

  if(type == 'plotly')
    p <- plotly::plot_ly(x = ~tbl.subset[,depInd], y = ~tbl.subset[,sel.var],
                 mode = 'lines+markers', type = 'scatter')
  p <- p%>% plotly::layout(title='Depth Profile Plot',
                   xaxis = list(title = 'Depth'),
                   yaxis = list(title= sel.var))
  if(type == 'ggplot'){
    p <- ggplot2::ggplot(tbl.subset, ggplot2::aes_string(x = 'depth', y = sel.var) ) +
      ggplot2::geom_point(na.rm = T) +
      ggplot2::ggtitle('Depth Profile Plot') +
      ggplot2::xlab('Depth') +
      ggplot2::ylab(sel.var) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(p)
}





#' Plot variable of interest with varying time
#'
#' times eries plot object
#'
#' @param tbl.subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel.var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point geom_line theme xlab ylab aes_string element_text
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' # Input variable:
#' table.list <- c('tblSST_AVHRR_OI_NRT', 'tblAltimetry_REP', 'tblPisces_NRT')
#' var.list <- c('sst', 'sla', 'NO3')
#' #
#' # selected "sst" from the table "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' table.name <- table.list[selIndex]
#' sel.var <- var.list[selIndex]
#' #
#' # # Example I:
#' range.var <- list()
#' range.var$lat <- c(25,30)
#' range.var$lon <- c(-160,-155)
#' range.var$time <- c('2016-03-29', '2016-05-29')
#' #
#'  # Subset selection: data retrieval
#' agg.var <- 'time'
#' tbl.subset <- getAggregatedTableData(con, table.name, sel.var, range.var, agg.var)
#' head(tbl.subset,20)
#' #
#' # Plot -- Time series:
#' p <- plot_ts(tbl.subset,'plotly',sel.var)
#' p
#' #
#' dbDisconnect(con)
#' }
plot_ts = function(tbl.subset, type = c('plotly','ggplot')[1], sel.var){
  tbl.subset <- data.frame(tbl.subset)
  tind <- match('time',names(tbl.subset))
  if(is.na(tind))
    stop('time variable missing')
  sind <- match(sel.var,names(tbl.subset))
  if(is.na(sind))
    stop('Selected variable missing')

  if(type == 'plotly')
    p <- plotly::plot_ly(x = ~tbl.subset[,tind], y = ~tbl.subset[,sind],
                 mode = 'lines+markers', type = 'scatter')
  p <- p %>% plotly::layout(title='Time Series Plot',
                    xaxis = list(title = 'Time'),
                    yaxis = list(title= sel.var))
  if(type == 'ggplot'){
    p <- ggplot2::ggplot(tbl.subset, ggplot2::aes_string(x = 'time', y = sel.var) ) +
      ggplot2::geom_point() + ggplot2::geom_line()
  }
  return(p)
}





#' Plot histogram of the variable of interest
#'
#' Histogram object of chosen type
#'
#' @param tbl.subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel.var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_histogram geom_density aes_string element_text aes
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' # Inpit variable:
#' table.list <- c('tblSST_AVHRR_OI_NRT', 'tblArgoMerge_REP', 'tblArgoMerge_REP')
#' var.list <-  c('sst', 'argo_merge_temperature_adj', 'argo_merge_salinity_adj')
#' #
#' # variable "sst" selected from "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' table.name <- table.list[selIndex]
#' sel.var <- var.list[selIndex]
#' #
#' #
#'  # Range variable
#' range.var <- list()
#' range.var$lat <- c(20, 24)
#' range.var$lon <- c(-170, 150)
#' range.var$time <- c('2016-04-30', '2016-04-30')
#' #
#' #
#' #
#' # Subset selection:
#' tbl.subset <- getTableData(con, table.name, sel.var, range.var)
#' head(tbl.subset)
#' #
#' #
#' # Plot function available for R User
#' p <- plot_hist(tbl.subset,'ggplot',sel.var)
#' p <- plot_hist(tbl.subset,'plotly',sel.var)
#' p
#' #
#' #
#' dbDisconnect(con)
#' }
plot_hist = function(tbl.subset, type = c('plotly','ggplot')[1], sel.var){
  tbl.subset <- data.frame(tbl.subset)
  sind <- match(sel.var,names(tbl.subset))
  if(is.na(sind))
    stop("Selected variable not in table")

  if(type == 'plotly')
    p <- plotly::plot_ly(x = ~tbl.subset[,sind],
                 type = "histogram",
                 histnorm = "probability")
  p <- p %>% plotly::layout(title='Histogram',
                    xaxis = list(title = sel.var),
                    yaxis = list(title= 'Density'))
  if(type == 'ggplot'){
    p <-  ggplot2::ggplot(tbl.subset, ggplot2::aes_string(x = sel.var) ) +
      ggplot2::geom_histogram(aes(y=..density..) ) +
      ggplot2::geom_density( na.rm = T ) +
      ggplot2::ggtitle('Histogram') +
      ggplot2::xlab(sel.var) +
      ggplot2::ylab('Density') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  }
  return(p)
}



#' XY plot
#'
#' Scatter plot of two variable of interest
#'
#' @param con connection object to the database
#' @param table.list list of  tables
#' @param var.list list of  corresponding variable
#' @param range.var range of time, latitude, longitude, depth
#' @param agg.var aggregate variable
#' @param type choose a type of plot object: 'plotly','ggplot
#' @return
#'    \item{plot}{ plot object of the chosen type}
#'    \item{table.x}{subset of data from  first table }
#'    \item{table.y}{subset of data from  second table }
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_density aes_string element_text
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' # Inpit variable:
#' #
#' table.list <- c('tblSST_AVHRR_OI_NRT', 'tblAltimetry_REP')
#' var.list <-  c('sst', 'sla')
#' #
#' range.var <- list()
#' range.var$lat <- c(25,30)
#' range.var$lon <- c(-160, -155)
#' range.var$time <- c('2016-03-29', '2016-05-29')
#' #
#' agg.var <- 'time'
#'
#' out <- plot_xy(con, table.list,var.list,range.var,agg.var,type = 'plotly')
#' out$plot
#' dbDisconnect(con)
#' }
plot_xy = function(con, table.list,var.list,range.var,agg.var,
                   type = c('plotly','ggplot')[1]){
  selIndex <- 1
  table.name <- table.list[selIndex]                       # Specify table name I
  sel.var <- var.list[selIndex]                            # Variable from table name I
  tbl.subset.x <- getAggregatedTableData(con, table.name, sel.var, range.var, agg.var)

  selIndex <- 2
  table.name <- table.list[selIndex]                       # Specify table name II
  sel.var <- var.list[selIndex]                            # Variable from table name II
  tbl.subset.y <- getAggregatedTableData(con, table.name, sel.var, range.var, agg.var)
  df.plot <-  merge(tbl.subset.x,tbl.subset.y, agg.var)

  if(type == 'plotly'){
    p <- plotly::plot_ly(x = ~df.plot[,var.list[1]], y = ~df.plot[,var.list[2]],
                 mode = 'markers', type = 'scatter')
    p <- p%>% plotly::layout(title='XY Plot',
                     xaxis = list(title = var.list[1]),
                     yaxis = list(title= var.list[2]))
  }
  if(type == 'ggplot'){
    p <- ggplot2::ggplot(df.plot, ggplot2::aes_string(x = var.list[1], y =var.list[2]) ) +
      ggplot2::geom_point() +
      ggplot2::ggtitle('XY Plot') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(list(plot = p, table.x = tbl.subset.x, table.y = tbl.subset.y))
}





#' Regional map
#'
#' Regional map of a variable of interest at varying range of latitude and longitude
#'
#' @param con connection object to the database
#' @param table.name name of table from CMAP
#' @param sel.var selected varable from the table
#' @param range.var range of time, latitude, longitude, depth
#' @param type choose a type of plot object: 'plotly','ggplot
#' @return
#'    \item{plot}{ plot object of the chosen type}
#'    \item{table}{subset of data from  first table }
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_tile aes_string element_text
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' # Inpit variable:
#' table.name = 'tblsst_AVHRR_OI_NRT'
#' sel.var = 'sst'
#' range.var <- list()
#' range.var$lat <- c(10,70)
#' range.var$lon <- c(-180,-80)
#' range.var$time <- c('2016-04-30', '2016-04-30')
#' #
#' ## Plot - regional map
#' # out <- plot_regMap(con, table.name,sel.var,range.var, type = 'ggplot')
#' out <- plot_regMap(con, table.name,sel.var,range.var, type = 'plotly')
#' out$plot
#' }
plot_regMap = function(con, table.name,sel.var,range.var,
                       type = c('plotly','ggplot')[1]){
  tbl.subset <- getTableData(con, table.name, sel.var, range.var,
                             order.var=  c('lat','lon'))

  ## Data prepration for heatmap
  lat.ref <- data.frame(lat=unique(tbl.subset$lat))
  lat.ref$ind <- 1:nrow(lat.ref)
  lon.ref <- data.frame(lon=unique(tbl.subset$lon))
  lon.ref$ind <- 1:nrow(lon.ref)
  ind.lat <- match(tbl.subset$lat,lat.ref$lat)
  ind.lon <- match(tbl.subset$lon,lon.ref$lon)

  sst.mat <- matrix(NA,nrow = nrow(lon.ref), ncol = nrow(lat.ref))
  for (i in 1:length(ind.lon))
    sst.mat[ind.lon[i],ind.lat[i]] <- as.numeric(tbl.subset[i,sel.var])


  if(type == 'plotly'){
    p <- plotly::plot_ly(
      x = lon.ref$lon,
      y = lat.ref$lat,
      z = t(sst.mat),
      type = "heatmap"
    )
    p <- p %>% plotly::layout(title='Regional map',
                      xaxis = list(title = 'Longitude'),
                      yaxis = list(title= 'Latitude'))
  }
  if(type == 'ggplot'){
    plotsub <- data.frame(tbl.subset)
    p <- ggplot2::ggplot(data = plotsub, ggplot2::aes_string(x='lon', y='lat',
                                                             fill=sel.var)) +
      ggplot2::geom_tile()  + ggplot2::ggtitle('Regional map') +
      ggplot2::xlab('Longitude') +
      ggplot2::ylab('Lattitude') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(list(plot = p, table = tbl.subset))
}





