


#' Plot variable of interest with varying depth
#'
#' Depth plot object
#'
#' @param tbl_subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel_var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @import magrittr
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point theme xlab ylab aes_string element_text
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' # Inpit variable:
#' table_list <- c("tblArgoMerge_REP", "tblPisces_NRT", "tblDarwin_Chl_Climatology")
#' var_list <- c("argo_merge_chl_adj", "CHL", "chl01_darwin_clim")
#' #
#' # selected argo_merge_chl_adj from tblArgoMerge_REP
#' selIndex <- 1
#' table_name <- table_list[selIndex]
#' sel_var <- var_list[selIndex]
#' #
#' range_var <- list()
#' range_var$lat <- c(20, 24)
#' range_var$lon <- c(-170, -150)
#' range_var$depth <- c(0, 1500)
#' range_var$time <- c("2016-04-30", "2016-04-30")
#' #
#' #
#' # Subset selection: data retrieval
#' agg_var <- "depth"
#' tbl_subset <- get_aggtable(con, table_name, sel_var, range_var, agg_var)
#' head(tbl_subset)
#' #
#' #
#' # Plot -- Depth profiles:
#' p <- plot_depth(tbl_subset, "plotly", sel_var)
#' #
#' #
#' dbDisconnect(con)
#' }
plot_depth <- function(tbl_subset, type = c("plotly", "ggplot")[1], sel_var) {
  tbl_subset <- data.frame(tbl_subset)
  depInd <- match("depth", names(tbl_subset))
  if (is.na(depInd)) {
    stop("depth variable missing")
  }
  sind <- match(sel_var, names(tbl_subset))

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
  return(p)
}





#' Plot variable of interest with varying time
#'
#' times eries plot object
#'
#' @param tbl_subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel_var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot geom_point geom_line theme xlab ylab aes_string element_text
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
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
#' range_var <- list()
#' range_var$lat <- c(25, 30)
#' range_var$lon <- c(-160, -155)
#' range_var$time <- c("2016-03-29", "2016-05-29")
#' #
#' # Subset selection: data retrieval
#' agg_var <- "time"
#' tbl_subset <- get_aggtable(con, table_name, sel_var, range_var, agg_var)
#' head(tbl_subset, 20)
#' #
#' # Plot -- Time series:
#' p <- plot_ts(tbl_subset, "plotly", sel_var)
#' p
#' #
#' dbDisconnect(con)
#' }
plot_ts <- function(tbl_subset, type = c("plotly", "ggplot")[1], sel_var) {
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
  return(p)
}




utils::globalVariables(c("..density.."))
#' Plot histogram of the variable of interest
#'
#' Histogram object of chosen type
#'
#' @param tbl_subset table with depth and variable of interest
#' @param type choose a type of plot object: 'plotly','ggplot
#' @param sel_var select a variable in the table
#' @return plot object of the chosen type
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_histogram geom_density aes_string element_text aes
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
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
#' #
#' # Range variable
#' range_var <- list()
#' range_var$lat <- c(20, 24)
#' range_var$lon <- c(-170, 150)
#' range_var$time <- c("2016-04-30", "2016-04-30")
#' #
#' #
#' #
#' # Subset selection:
#' tbl_subset <- get_table(con, table_name, sel_var, range_var)
#' head(tbl_subset)
#' #
#' #
#' # Plot function available for R User
#' p <- plot_hist(tbl_subset, "ggplot", sel_var)
#' p <- plot_hist(tbl_subset, "plotly", sel_var)
#' p
#' #
#' #
#' dbDisconnect(con)
#' }
plot_hist <- function(tbl_subset, type = c("plotly", "ggplot")[1], sel_var) {
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
      ggplot2::geom_histogram(aes(y = ..density..)) +
      ggplot2::ggtitle("Histogram") +
      ggplot2::xlab(sel_var) +
      ggplot2::ylab("Density") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(p)
}



#' XY plot
#'
#' Scatter plot of two variable of interest
#'
#' @param con connection object to the database
#' @param table_list list of  tables
#' @param var_list list of  corresponding variable
#' @param range_var range of time, latitude, longitude, depth
#' @param agg_var aggregate variable
#' @param type choose a type of plot object: 'plotly','ggplot
#' @return
#'    \item{plot}{ plot object of the chosen type}
#'    \item{table_x}{subset of data from  first table }
#'    \item{table_y}{subset of data from  second table }
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_density aes_string element_text
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' # Inpit variable:
#' #
#' table_list <- c("tblSST_AVHRR_OI_NRT", "tblAltimetry_REP")
#' var_list <- c("sst", "sla")
#' #
#' range_var <- list()
#' range_var$lat <- c(25, 30)
#' range_var$lon <- c(-160, -155)
#' range_var$time <- c("2016-03-29", "2016-05-29")
#' #
#' agg_var <- "time"
#'
#' out <- plot_xy(con, table_list, var_list, range_var, agg_var, type = "plotly")
#' out$plot
#' dbDisconnect(con)
#' }
plot_xy <- function(con, table_list, var_list, range_var, agg_var,
                    type = c("plotly", "ggplot")[1]) {
  selIndex <- 1
  table_name <- table_list[selIndex] # Specify table name I
  sel_var <- var_list[selIndex] # Variable from table name I
  tbl_subset_x <- get_aggtable(con, table_name, sel_var, range_var, agg_var)

  selIndex <- 2
  table_name <- table_list[selIndex] # Specify table name II
  sel_var <- var_list[selIndex] # Variable from table name II
  tbl_subset_y <- get_aggtable(con, table_name, sel_var, range_var, agg_var)
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
  return(list(plot = p, table_x = tbl_subset_x, table_y = tbl_subset_y))
}





#' Regional map
#'
#' Regional map of a variable of interest at varying range of latitude and longitude
#'
#' @param con connection object to the database
#' @param table_name name of table from CMAP
#' @param sel_var selected varable from the table
#' @param range_var range of time, latitude, longitude, depth
#' @param type choose a type of plot object: 'plotly','ggplot
#' @return
#'    \item{plot}{ plot object of the chosen type}
#'    \item{table}{subset of data from  first table }
#' @export
#' @importFrom plotly plot_ly layout last_plot
#' @importFrom ggplot2 ggtitle ggplot xlab ylab theme geom_point geom_tile aes_string element_text
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' # Inpit variable:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#' sel_var <- "sst"
#' range_var <- list()
#' range_var$lat <- c(10, 70)
#' range_var$lon <- c(-180, -80)
#' range_var$time <- c("2016-04-30", "2016-04-30")
#' #
#' ## Plot - regional map
#' # out <- plot_regmap(con, table_name,sel_var,range_var, type = 'ggplot')
#' out <- plot_regmap(con, table_name, sel_var, range_var, type = "plotly")
#' out$plot
#' }
plot_regmap <- function(con, table_name, sel_var, range_var,
                        type = c("plotly", "ggplot")[1]) {
  tbl_subset <- get_table(con, table_name, sel_var, range_var,
    order_var = c("lat", "lon")
  )

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
  return(list(plot = p, table = tbl_subset))
}
