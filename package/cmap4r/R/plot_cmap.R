



#' Plot variable of interest with varying depth
#'
#' Depth plot object
#'
#' @param table_name name of the table from CMAP
#' @param sel_var select a variable in the table
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory.
#' @return plot object of the chosen type
#' @export
#' @import magrittr
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point theme xlab ylab aes_string element_text
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' # Inpit variable:
#' table_list <- c('tblArgoMerge_REP', 'tblPisces_NRT', 'tblDarwin_Ecosystem')
#' var_list <- c('argo_merge_chl_adj', 'CHL', 'CHL')
#' #
#' # selected argo_merge_chl_adj from tblArgoMerge_REP
#' selIndex <- 1
#' table_name <- table_list[selIndex]
#' sel_var <- var_list[selIndex]
#' #
#' # Range variable [lat,lon,time,depth]
#' lat1 = 20; lat2 = 24
#' lon1 = -170; lon2 = -150
#' dt1 = "2014-04-25"; dt2 = "2014-04-30"
#' depth1 <- 0; depth2 =  1500
#'
#' #
#' # Plot -- Depth profiles:
#' p <- plot_depth(table_name, sel_var, lat1, lat2, lon1, lon2,
#'                         dt1, dt2,depth1, depth2, "plotly")
#' # p
#' #
#' }
plot_depth <- function(table_name, sel_var, lat1, lat2,
                       lon1, lon2, dt1, dt2,
                       depth1, depth2,
                       type = c("plotly", "ggplot")[1],
                       export_data = FALSE) {

  tbl_subset <- get_depthprofile(table_name, sel_var, lat1, lat2,
                                 lon1, lon2, dt1, dt2,
                                 depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)
  depInd <- match("depth", names(tbl_subset))
  if (is.na(depInd)) {
    stop("depth variable missing")
  }

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ tbl_subset[, depInd], y = ~ tbl_subset[, sel_var],
      mode = "lines+markers", type = "scatter"
    )
  }
  p <- p %>% plotly::layout(
    title = "Depth Profile Plot",
    xaxis = list(title = "Depth"),
    yaxis = list(title = sel_var)
  )
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = "depth", y = sel_var)) +
      ggplot2::geom_point(na.rm = T) +
      ggplot2::ggtitle("Depth Profile Plot") +
      ggplot2::xlab("Depth") +
      ggplot2::ylab(sel_var) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if(export_data) {
    fname <- file.path(getwd(),"depth_profile.csv")
    utils::write.csv(tbl_subset,file = fname)
    cat('Export data location:\n', fname)
    }
  return(p)
}





#' Plot variable of interest with varying time.
#'
#' Times eries plot object.
#'
#' @param table_name table with depth and variable of interest
#' @param sel_var select a variable in the table
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point geom_line theme xlab ylab aes_string element_text
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' # Input variable:
#' table_list <- c("tblSST_AVHRR_OI_NRT", "tblAltimetry_REP", "tblPisces_NRT")
#' var_list <- c("sst", "sla", "NO3")
#' #
#' # selected "sst" from the table "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' table_name <- table_list[selIndex]
#' sel_var <- var_list[selIndex]
#' #
#' # # Example I:
#' # Range variable [lat,lon,time,depth]
#' lat1 = 25; lat2 = 30
#' lon1 = -160; lon2 = -155
#' dt1 = "2016-03-29"; dt2 = "2016-05-29"
#' #
#' # Plot -- Time series:
#' p <- plot_ts(table_name, sel_var, lat1, lat2, lon1, lon2,
#'              dt1, dt2)
#' p
#' #
#' }
plot_ts <- function(table_name, sel_var, lat1, lat2,
                    lon1, lon2, dt1, dt2,
                    depth1 = NULL, depth2 = NULL,
                    type = c("plotly", "ggplot")[1],
                    export_data = FALSE) {
  tbl_subset <- get_timeseries(table_name, sel_var, lat1, lat2,
                               lon1, lon2, dt1, dt2,
                               depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)

  tind <- match("time", names(tbl_subset))
  if (is.na(tind)) {
    stop("time variable missing")
  }
  sind <- match(sel_var, names(tbl_subset))
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
    yaxis = list(title = sel_var)
  )
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = "time", y = sel_var)) +
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
#' Plot histogram of the variable of interest
#'
#' Histogram object of chosen type
#'
#' @param table_name name of the table from CMAP
#' @param sel_var select a variable in the table
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_histogram geom_density aes_string element_text aes
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' # Inpit variable:
#' table_list <- c("tblSST_AVHRR_OI_NRT", "tblArgoMerge_REP", "tblArgoMerge_REP")
#' var_list <- c("sst", "argo_merge_temperature_adj", "argo_merge_salinity_adj")
#' #
#' # variable "sst" selected from "tblSST_AVHRR_OI_NRT"
#' selIndex <- 1
#' table_name <- table_list[selIndex]
#' sel_var <- var_list[selIndex]
#' #
#' # Range variable [lat,lon,time,depth]
#' lat1 = 20; lat2 = 24
#' lon1 = -170; lon2 = -150
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- NULL; depth2 =  NULL
#'
#'
#' # Plot function available for R User
#' # p <- plot_hist(table_name, sel_var, lat1, lat2,
#' #                lon1, lon2, dt1, dt2, depth1, depth2, "ggplot")
#'
#' p <- plot_hist(table_name, sel_var, lat1, lat2, lon1, lon2,
#'                dt1, dt2, depth1, depth2, "plotly")
#' p
#' #
#' }
plot_hist <- function(table_name, sel_var, lat1, lat2,
                      lon1, lon2, dt1, dt2,
                      depth1 = NULL, depth2 = NULL,
                      type = c("plotly", "ggplot")[1],
                      export_data = FALSE) {

  tbl_subset <- get_spacetime(table_name, sel_var, lat1, lat2,
                              lon1, lon2, dt1, dt2,
                              depth1, depth2)
  tbl_subset <- data.frame(tbl_subset)

  sind <- match(sel_var, names(tbl_subset))
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
      xaxis = list(title = sel_var),
      yaxis = list(title = "Density")
    )
  }
  if (type == "ggplot") {
    p <- ggplot2::ggplot(tbl_subset, ggplot2::aes_string(x = sel_var)) +
      ggplot2::geom_density(na.rm = T) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..)) +
      ggplot2::ggtitle("Histogram") +
      ggplot2::xlab(sel_var) +
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
#' Scatter plot of two variable of interest
#'
#' @param table_list list of  tables
#' @param var_list list of  corresponding variable
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param agg_var aggregate variable
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_density aes_string element_text
#' @examples
#' \donttest{
#' library(cmap4r)
#' # Inpit variable:
#' #
#' table_list <- c("tblSST_AVHRR_OI_NRT", "tblAltimetry_REP")
#' var_list <- c("sst", "sla")
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
#' out <- plot_xy(table_list, var_list, lat1, lat2, lon1, lon2,
#'                dt1, dt2, depth1, depth2, agg_var)
#' out
#' }
plot_xy <- function(table_list, var_list, lat1, lat2,
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
  table_name <- table_list[selIndex] # Specify table name I
  sel_var <- var_list[selIndex] # Variable from table name I
  tbl_subset_x <- temfun(table_name, sel_var, lat1, lat2,
                         lon1, lon2, dt1, dt2,
                         depth1, depth2)
  tbl_subset_x <- data.frame(tbl_subset_x)


  selIndex <- 2
  table_name <- table_list[selIndex] # Specify table name II
  sel_var <- var_list[selIndex] # Variable from table name II
  tbl_subset_y <- temfun(table_name, sel_var, lat1, lat2,
                         lon1, lon2, dt1, dt2,
                         depth1, depth2)
  tbl_subset_y <- data.frame(tbl_subset_y)

  ## Suggested:
  # stopifnot(same_resolution(tbl_subset_x, tbl_subset_y))

  df_plot <- merge(tbl_subset_x, tbl_subset_y, agg_var)

  if (type == "plotly") {
    p <- plotly::plot_ly(
      x = ~ df_plot[, var_list[1]], y = ~ df_plot[, var_list[2]],
      mode = "markers", type = "scatter"
    )
    p <- p %>% plotly::layout(
      title = "XY Plot",
      xaxis = list(title = var_list[1]),
      yaxis = list(title = var_list[2])
    )
  }
  if (type == "ggplot") {
    p <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x = var_list[1], y = var_list[2])) +
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
#' Regional map of a variable of interest at varying range of latitude and longitude
#'
#' @param table_name name of table from CMAP
#' @param sel_var selected varable from the table
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param export_data boolean variable to export data or not. The data will be saved in the working directory.
#' @return plot object of the chosen type
#' @export
#' @importFrom utils write.csv
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_tile aes_string element_text
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' # Inpit variable:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#' sel_var <- "sst"
#'
#' # Range variable [lat,lon,time,depth]
#' lat1 = 10; lat2 = 70
#' lon1 = -180; lon2 = -80
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- NULL; depth2 =  NULL
#'
#' #
#' ## Plot - regional map
#' # out <- plot_regmap( table_name,sel_var, lat1, lat2, lon1, lon2,
#' #                    dt1, dt2, depth1, depth2, type = 'ggplot')
#' out <- plot_regmap(table_name, sel_var, lat1, lat2, lon1, lon2,
#'                    dt1, dt2, depth1, depth2, type = "plotly")
#' out
#' }
plot_regmap <- function(table_name, sel_var, lat1, lat2,
                        lon1, lon2, dt1, dt2,
                        depth1 = NULL, depth2 = NULL,
                        type = c("plotly", "ggplot")[1],
                        export_data = FALSE) {
  tbl_subset <- get_spacetime(table_name, sel_var, lat1, lat2,
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
    sst_mat[ind_lon[i], ind_lat[i]] <- as.numeric(tbl_subset[i, sel_var])
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
      fill = sel_var
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


