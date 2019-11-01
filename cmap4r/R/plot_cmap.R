

#' Depth profile plot. 
#' 
#' Plot a variable of interest with varying depth after extracting the data (using get_depthprofile) from the Simons CMAP database using the specified space-time constraints (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2). 
#'
#' 
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param type choose a type of plot object: 'plotly','ggplot. Default 'plotly'.
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory. Default FALSE.
#' @return a plot object of the chosen type
#' @export
#' @import magrittr
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point theme xlab ylab aes_string element_text
#' @examples
#' \donttest{
#' #
#' # Inpit variable:
#' tableList <- c('tblArgoMerge_REP', 'tblPisces_NRT', 'tblDarwin_Ecosystem')
#' varList <- c('argo_merge_chl_adj', 'CHL', 'CHL')
#' #
#' # selected argo_merge_chl_adj from tblArgoMerge_REP
#' selIndex <- 1
#' tableName <- tableList[selIndex]
#' varName <- varList[selIndex]
#' #
#' # Range variable [lat,lon,time,depth]
#' lat1 = 20; lat2 = 24
#' lon1 = -170; lon2 = -150
#' dt1 = "2014-04-25"; dt2 = "2014-04-30"
#' depth1 <- 0; depth2 =  1500
#'
#' #
#' # Plot -- Depth profiles:
#' p <- plot_depth(tableName, varName, lat1, lat2, lon1, lon2,
#'                         dt1, dt2,depth1, depth2, "plotly")
#' # p
#' #
#' }
plot_depth <- function(tableName, varName, lat1, lat2,
                       lon1, lon2, dt1, dt2,
                       depth1, depth2,
                       type = c("plotly", "ggplot")[1],
                       export_data = FALSE) {

  tbl_subset <- get_depthprofile(tableName, varName, lat1, lat2,
                                 lon1, lon2, dt1, dt2,
                                 depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)
  depInd <- match("depth", names(tbl_subset))
  if (is.na(depInd)) {
    stop("depth variable missing")
  }

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ tbl_subset[, depInd], y = ~ tbl_subset[, varName],
      mode = "lines+markers", type = "scatter"
    )
  }
  p <- p %>% plotly::layout(
    title = "Depth Profile Plot",
    xaxis = list(title = "Depth"),
    yaxis = list(title = varName)
  )
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = "depth", y = varName)) +
      ggplot2::geom_point(na.rm = T) +
      ggplot2::ggtitle("Depth Profile Plot") +
      ggplot2::xlab("Depth") +
      ggplot2::ylab(varName) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if(export_data) {
    fname <- file.path(getwd(),"depth_profile.csv")
    utils::write.csv(tbl_subset,file = fname)
    cat('Export data location:\n', fname)
    }
  return(p)
}




#' Times eries plot
#' 
#' Plot a variable of interest with varying time after extracting the data (using get_timeseries) from the Simons CMAP database using the specified space-time constraints (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2). 
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param type choose a type of plot object: 'plotly','ggplot. Default 'plotly'.
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory. Default FALSE.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point geom_line theme xlab ylab aes_string element_text
#' @examples
#' \donttest{
#' 
#' #
#' # Input variable:
#' tableList <- c("tblSST_AVHRR_OI_NRT", "tblAltimetry_REP", "tblPisces_NRT")
#' varList <- c("sst", "sla", "NO3")
#' #
#' # selected "sst" from the table "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' tableName <- tableList[selIndex]
#' varName <- varList[selIndex]
#' #
#' # # Example I:
#' # Range variable [lat,lon,time,depth]
#' lat1 = 25; lat2 = 30
#' lon1 = -160; lon2 = -155
#' dt1 = "2016-03-29"; dt2 = "2016-05-29"
#' #
#' # Plot -- Time series:
#' p <- plot_ts(tableName, varName, lat1, lat2, lon1, lon2,
#'              dt1, dt2)
#' p
#' #
#' }
plot_ts <- function(tableName, varName, lat1, lat2,
                    lon1, lon2, dt1, dt2,
                    depth1 = NULL, depth2 = NULL,
                    type = c("plotly", "ggplot")[1],
                    export_data = FALSE) {
  tbl_subset <- get_timeseries(tableName, varName, lat1, lat2,
                               lon1, lon2, dt1, dt2,
                               depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)

  tind <- match("time", names(tbl_subset))
  if (is.na(tind)) {
    stop("time variable missing")
  }
  sind <- match(varName, names(tbl_subset))
  if (is.na(sind)) {
    stop("Selected variable missing")
  }

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ tbl_subset[, tind], y = ~ tbl_subset[, sind],
      mode = "lines+markers", type = "scatter"
    )
  }
  p <- p %>% plotly::layout(
    title = "Time Series Plot",
    xaxis = list(title = "Time"),
    yaxis = list(title = varName)
  )
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = "time", y = varName)) +
      ggplot2::geom_point() + ggplot2::geom_line()
  }
  if(export_data) {
    fname <- file.path(getwd(),"time_series.csv")
    utils::write.csv(tbl_subset,file = fname)
    cat('Export data location:\n', fname)
  }
  return(p)
}






utils::globalVariables(c("..density.."))
#' Histogram plot 
#' 
#' Plot histogram of the variable of interest after extracting the data (using get_spacetime) from the Simons CMAP database using the specified space-time constraints (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2).
#'
#' 
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param type choose a type of plot object: 'plotly','ggplot'. Default 'plotly'.
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory. Default FALSE.
#' @return plot object of the chosen type, i.e., plotly/ggplot
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_histogram geom_density aes_string element_text aes
#' @examples
#' \donttest{
#' 
#' #
#' # Inpit variable:
#' tableList <- c("tblSST_AVHRR_OI_NRT", "tblArgoMerge_REP", "tblArgoMerge_REP")
#' varList <- c("sst", "argo_merge_temperature_adj", "argo_merge_salinity_adj")
#' #
#' # variable "sst" selected from "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' tableName <- tableList[selIndex]
#' varName <- varList[selIndex]
#' #
#' # Range variable [lat,lon,time,depth]
#' lat1 = 20; lat2 = 24
#' lon1 = -170; lon2 = -150
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- NULL; depth2 =  NULL
#'
#'
#' # Plot function available for R User
#' # p <- plot_hist(tableName, varName, lat1, lat2,
#' #                lon1, lon2, dt1, dt2, depth1, depth2, "ggplot")
#'
#' p <- plot_hist(tableName, varName, lat1, lat2, lon1, lon2,
#'                dt1, dt2, depth1, depth2, "plotly")
#' p
#' #
#' }
plot_hist <- function(tableName, varName, lat1, lat2,
                      lon1, lon2, dt1, dt2,
                      depth1 = NULL, depth2 = NULL,
                      type = c("plotly", "ggplot")[1],
                      export_data = FALSE) {

  tbl_subset <- get_spacetime(tableName, varName, lat1, lat2,
                              lon1, lon2, dt1, dt2,
                              depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)

  sind <- match(varName, names(tbl_subset))
  if (is.na(sind)) {
    stop("Selected variable not in table")
  }

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ tbl_subset[, sind],
      type = "histogram",
      histnorm = "probability"
    )
    p <- p %>% plotly::layout(
      title = "Histogram",
      xaxis = list(title = varName),
      yaxis = list(title = "Density")
    )
  }
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = varName)) +
      ggplot2::geom_density(na.rm = T) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..)) +
      ggplot2::ggtitle("Histogram") +
      ggplot2::xlab(varName) +
      ggplot2::ylab("Density") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if(export_data) {
    fname <- file.path(getwd(),"histogram.csv")
    utils::write.csv(tbl_subset,file = fname)
    cat('Export data location:\n', fname)
  }
  return(p)
}



#' XY plot
#'
#' Scatter plot of the two variable of interest.
#'
#' @param tableList table names from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varList specify short name of the corresponding table variables. Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param agg_var aggregate variable
#' @param type choose a type of plot object: 'plotly','ggplot'. Default 'plotly'.
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory. Default FALSE.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_density aes_string element_text
#' @examples
#' \donttest{
#' 
#' # Inpit variable:
#' #
#' tableList <- c("tblSST_AVHRR_OI_NRT", "tblAltimetry_REP")
#' varList <- c("sst", "sla")
#' #
#' # Range variable [lat,lon,time,depth]
#' lat1 = 25; lat2 = 30
#' lon1 = -160; lon2 = -155
#' dt1 =  "2016-03-29"; dt2 = "2016-05-29"
#' depth1 <- NULL; depth2 =  NULL
#'
#' #
#' agg_var <- "time"
#'
#' # xy plot
#' out <- plot_xy(tableList, varList, lat1, lat2, lon1, lon2,
#'                dt1, dt2, depth1, depth2, agg_var)
#' out
#' }
plot_xy <- function(tableList, varList, lat1, lat2,
                    lon1, lon2, dt1, dt2,
                    depth1 = NULL, depth2 = NULL, agg_var,
                    type = c("plotly", "ggplot")[1],
                    export_data = FALSE) {
  if ( agg_var == 'time' ){
    temfun = get_timeseries
  } else if ( agg_var=='depth' ){
    temfun = get_depthprofile
  } else {
    temfun = get_spacetime
  }
  selIndex <- 1
  tableName <- tableList[selIndex] # Specify table name I
  varName <- varList[selIndex] # Variable from table name I
  tbl_subset_x <- temfun(tableName, varName, lat1, lat2,
                         lon1, lon2, dt1, dt2,
                         depth1, depth2)
  tbl_subset_x <- data.frame(tbl_subset_x)


  selIndex <- 2
  tableName <- tableList[selIndex] # Specify table name II
  varName <- varList[selIndex] # Variable from table name II
  tbl_subset_y <- temfun(tableName, varName, lat1, lat2,
                         lon1, lon2, dt1, dt2,
                         depth1, depth2)
  tbl_subset_y <- data.frame(tbl_subset_y)

  ## Suggested:
  # stopifnot(same_resolution(tbl_subset_x, tbl_subset_y))

  df_plot <- merge(tbl_subset_x, tbl_subset_y, agg_var)

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ df_plot[, varList[1]], y = ~ df_plot[, varList[2]],
      mode = "markers", type = "scatter"
    )
    p <- p %>% plotly::layout(
      title = "XY Plot",
      xaxis = list(title = varList[1]),
      yaxis = list(title = varList[2])
    )
  }
  if (type == "ggplot") {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x = varList[1], y = varList[2])) +
      ggplot2::geom_point() +
      ggplot2::ggtitle("XY Plot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if(export_data) {
    fname <- file.path(getwd(),"xy_plot.csv")
    utils::write.csv(df_plot,file = fname)
    cat('Export data location:\n', fname)
  }
  # return(list(plot = p, table_x = tbl_subset_x, table_y = tbl_subset_y))
  return(p)
}






#' Regional map
#'
#' Plot the regional map of a variable of interest in a varying range of latitude and longitude after extracting the data (using get_spacetime) from the Simons CMAP database using the specified space-time constraints (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2). 
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param type choose a type of plot object: 'plotly','ggplot'. Default 'plotly'.
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory. Default FALSE.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_tile aes_string element_text
#' @examples
#' \donttest{
#' 
#' #
#' # Inpit variable:
#' tableName <- "tblsst_AVHRR_OI_NRT"
#' varName <- "sst"
#'
#' # Range variable [lat,lon,time,depth]
#' lat1 = 10; lat2 = 70
#' lon1 = -180; lon2 = -80
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- NULL; depth2 =  NULL
#'
#' #
#' ## Plot - regional map
#' # out <- plot_regmap( tableName,varName, lat1, lat2, lon1, lon2,
#' #                    dt1, dt2, depth1, depth2, type = 'ggplot')
#' out <- plot_regmap(tableName, varName, lat1, lat2, lon1, lon2,
#'                    dt1, dt2, depth1, depth2, type = "plotly")
#' out
#' }
plot_regmap <- function(tableName, varName, lat1, lat2,
                        lon1, lon2, dt1, dt2,
                        depth1 = NULL, depth2 = NULL,
                        type = c("plotly", "ggplot")[1],
                        export_data = FALSE) {
  tbl_subset <- get_spacetime(tableName, varName, lat1, lat2,
                              lon1, lon2, dt1, dt2,
                              depth1, depth2)

  ## Data prepration for heatmap
  lat_ref <- data.frame(lat = unique(tbl_subset$lat))
  lat_ref$ind <- 1:nrow(lat_ref)
  lon_ref <- data.frame(lon = unique(tbl_subset$lon))
  lon_ref$ind <- 1:nrow(lon_ref)
  ind_lat <- match(tbl_subset$lat, lat_ref$lat)
  ind_lon <- match(tbl_subset$lon, lon_ref$lon)

  sst_mat <- matrix(NA, nrow = nrow(lon_ref), ncol = nrow(lat_ref))
  for (i in 1:length(ind_lon)) {
    sst_mat[ind_lon[i], ind_lat[i]] <- as.numeric(tbl_subset[i, varName])
  }


  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = lon_ref$lon,
      y = lat_ref$lat,
      z = t(sst_mat),
      type = "heatmap"
    )
    p <- p %>% plotly::layout(
      title = "Regional map",
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude")
    )
  }
  if (type == "ggplot") {
    plotsub <- data.frame(tbl_subset)
    p <- ggplot2::ggplot(data = plotsub, ggplot2::aes_string(
      x = "lon", y = "lat",
      fill = varName
    )) +
      ggplot2::geom_tile() + ggplot2::ggtitle("Regional map") +
      ggplot2::xlab("Longitude") +
      ggplot2::ylab("Lattitude") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if(export_data) {
    fname <- file.path(getwd(),"regional_map.csv")
    utils::write.csv(tbl_subset,file = fname)
    cat('Export data location:\n', fname)
  }
  # return(list(plot = p, table = tbl_subset))
  return(p)
}


