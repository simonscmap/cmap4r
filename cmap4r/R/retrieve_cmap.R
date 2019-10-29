

#' Retrieve subset of data from a table on CMAP
#'
#' Get subset of a table as data frame
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @param order_var retrieve data with order by variable
#' @return subset of a table as data frame
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange sql desc
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Input: Table name; variable name, space time range information
#' table_name <- "tblsst_AVHRR_OI_NRT" # table name
#' sel_var <- "sst" # choose variable
#' range_var <- list() # Range variable [lat,lon,time]
#' range_var$lat <- c(10, 70)
#' range_var$lon <- c(-180, -80)
#' range_var$time <- c("2016-04-30", "2016-04-30")
#' #
#' ## Subset selection:
#' tbl.subset <- get_table(con, table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
get_table <- function(con, table_name, sel_var, range_var, order_var = NULL) {
  tbl_connect <- dplyr::tbl(con, table_name)
  indrv <- tableVarMatch(con, table_name, names(range_var))
  if (any(is.na(indrv))) {
    stop("Check range variable;")
  }
  indse <- tableVarMatch(con, table_name, sel_var)
  if (any(is.na(indse))) {
    stop("Check selected variable;")
  }
  indsel <- c(indrv, indse)
  filt_queary <- getFilterQuery(range_var)
    print(filt_queary)

  ## Assign some default ordering
  if (is.null(order_var)) {
    tbl_query <- tbl_connect %>%
      dplyr::filter(filt_queary) %>%
      dplyr::select(indsel)
    tbl_subset <- tbl_query %>% dplyr::collect()
    return(tbl_subset)

  ## Else,
  } else {
    indrv <- tableVarMatch(con, table_name, order_var)
    if (any(is.na(indrv))) {
      stop("Check order variable;")
    }
    tempOrd <- paste0(order_var, collapse = ",")
    tbl_query <- tbl_connect %>%
      dplyr::filter(filt_queary) %>%
      dplyr::select(indsel) %>%
      dplyr::arrange(dplyr::desc(dplyr::sql(tempOrd)))
    tbl_subset <- tbl_query %>% dplyr::collect()
    return(tbl_subset)
  }
}








# # Table connect:
# get_aggtablexx = function(con, table.name, sel.var, range.var, agg.var, order.var= NULL){
#   tbl.connect <- tbl(con,table.name)
#   indrv <- tableVarMatch(con, table.name, names(range.var))
#   if(any(is.na(indrv))){
#     stop("Check range variable;")
#   }
#   indse <- tableVarMatch(con, table.name, sel.var)
#   if(any(is.na(indse))){
#     stop("Check selected variable;")
#   }
#   indag <- tableVarMatch(con, table.name, agg.var)
#   if(any(is.na(indag))){
#     stop("Check aggregate variable;")
#   }
#   indsel <- c(indrv, indse)
#   tbl.var <- c(names(range.var),sel.var)
#   summariseIndex <- match(setdiff(indsel,indag),indsel)
#   filt.query <- getFilterQuery(range.var)
#   if(is.null(order.var)){
#     tbl.query <- tbl.connect %>% dplyr::filter(filt.query)   %>%
#       select(indsel) %>% group_by_at(indag) %>%
#       summarise_at( tbl.var[summariseIndex], mean,na.rm=TRUE) %>%
#       arrange(( sql(agg.var)  ))
#
#     tbl.subset <-  tbl.query %>% collect()
#     return(tbl.subset)
#   } else {
#     indrv <- tableVarMatch(con, table.name, order.var)
#     if(any(is.na(indrv))){
#       stop("Check order variable;")
#     }
#     tempOrd <- paste(agg.var,paste0(order.var,collapse = ","),sep = ",")
#     tbl.query <- tbl.connect %>% dplyr::filter(filt.query)   %>%
#       select(indsel) %>% group_by_at(indag) %>%
#       summarise_at( tbl.var[summariseIndex], mean,na.rm=TRUE) %>%
#       arrange(desc(sql(tempOrd))  )
#
#     tbl.subset <-  tbl.query %>% collect()
#     return(tbl.subset)
#   }
#
# }



#' Retrieve aggregated subset of table on CMAP
#'
#' Get aggregated subset of a table
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @param agg_var aggregate variable
#' @param order_var retrieve data with order by variable
#' @return aggregated subset of a table
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at group_by_at sql desc
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Input: Table name; variable name, space time range information
#' table_name <- "tblsst_AVHRR_OI_NRT" # table name
#' sel_var <- "sst" # choose variable
#' range_var <- list() # Range variable [lat,lon,time]
#' range_var$lat <- c(25, 30)
#' range_var$lon <- c(-160, -155)
#' range_var$time <- c("2016-03-29", "2016-05-29")
#' #
#' # Aggregate
#' agg_var <- "time" # Specify aggregate variable
#' tbl_subset <- get_aggtable(con, table_name, sel_var, range_var, agg_var)
#' head(tbl_subset)
#' #
#' dbDisconnect(con)
#' }
get_aggtable <- function(con, table_name, sel_var, range_var,
                                   agg_var, order_var = NULL) {
  tbl_connect <- dplyr::tbl(con, table_name)
  indrv <- tableVarMatch(con, table_name, names(range_var))
  if (any(is.na(indrv))) {
    stop("Check range variable;")
  }
  indse <- tableVarMatch(con, table_name, sel_var)
  if (any(is.na(indse))) {
    stop("Check selected variable;")
  }
  indag <- tableVarMatch(con, table_name, agg_var)
  if (any(is.na(indag))) {
    stop("Check aggregate variable;")
  }
  indsel <- c(indrv, indse)
  tbl_var <- c(names(range_var), sel_var)
  summariseIndex <- match(setdiff(indsel, indag), indsel)
  filt_query <- getFilterQuery(range_var)
  if (is.null(order_var)) {
    tbl_query <- tbl_connect %>%
      dplyr::select(indsel) %>%
      dplyr::filter(filt_query) %>%
      dplyr::group_by_at(sql(agg_var)) %>%
      dplyr::summarise_at(dplyr::sql(sel_var), mean, na.rm = TRUE) %>%
      dplyr::arrange((dplyr::sql(agg_var)))

    tbl_subset <- tbl_query %>% dplyr::collect()
    return(tbl_subset)
  } else {
    indrv <- tableVarMatch(con, table_name, order_var)
    if (any(is.na(indrv))) {
      stop("Check order variable;")
    }
    tempOrd <- paste(agg_var, paste0(order_var, collapse = ","), sep = ",")
    tbl_query <- tbl_connect %>%
      dplyr::select(indsel) %>%
      dplyr::filter(filt_query) %>%
      dplyr::group_by_at(dplyr::sql(agg_var)) %>%
      dplyr::summarise_at(dplyr::sql(sel_var), mean, na.rm = TRUE) %>%
      dplyr::arrange(dplyr::desc(dplyr::sql(tempOrd)))

    tbl_subset <- tbl_query %>% dplyr::collect()
    return(tbl_subset)
  }
}
