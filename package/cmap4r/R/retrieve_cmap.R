

#' Retrieve subset of data from a table on CMAP
#'
#' Get subset of a table as data frame
#'
#' @param con connection object to the database
#' @param table.name table name in the database
#' @param sel.var select a variable in the table
#' @param range.var range of time, latitude, longitude, depth
#' @param order.var retrieve data with order by variable
#' @return subset of a table as data frame
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange sql desc
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' ## Input: Table name; variable name, space time range information
#' table.name = 'tblsst_AVHRR_OI_NRT'    # table name
#' sel.var = 'sst'                         # choose variable
#' range.var <- list()                     # Range variable [lat,lon,time]
#' range.var$lat <- c(10,70)
#' range.var$lon <- c(-180,-80)
#' range.var$time <- c('2016-04-30', '2016-04-30')
#' #
#' ## Subset selection:
#' tbl.subset <- getTableData(con, table.name, sel.var, range.var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
getTableData = function(con, table.name, sel.var, range.var, order.var= NULL){

  tbl.connect <- dplyr::tbl(con,table.name)
  indrv <- tableVarMatch(con, table.name, names(range.var))
  if(any(is.na(indrv))){
    stop("Check range variable;")
  }
  indse <- tableVarMatch(con, table.name, sel.var)
  if(any(is.na(indse))){
    stop("Check selected variable;")
  }
  indsel <- c(indrv, indse)
  filt.query <- getFilterQuery(range.var)
  if(is.null(order.var)){
    tbl.query <- tbl.connect %>% dplyr::filter(filt.query)   %>%
      dplyr::select(indsel)
    tbl.subset <-  tbl.query %>% dplyr::collect()
    return(tbl.subset)
  } else {
    indrv <- tableVarMatch(con, table.name, order.var)
    if(any(is.na(indrv))){
      stop("Check order variable;")
    }
    tempOrd <- paste0(order.var,collapse = ",")
    tbl.query <- tbl.connect %>% dplyr::filter(filt.query)   %>%
      dplyr::select(indsel) %>%
      dplyr::arrange(dplyr::desc(dplyr::sql(tempOrd)))
    tbl.subset <-  tbl.query %>% dplyr::collect()
    return(tbl.subset)
  }

}








# # Table connect:
# getAggregatedTableDataxx = function(con, table.name, sel.var, range.var, agg.var, order.var= NULL){
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
#' @param table.name table name in the database
#' @param sel.var select a variable in the table
#' @param range.var range of time, latitude, longitude, depth
#' @param agg.var aggregate variable
#' @param order.var retrieve data with order by variable
#' @return aggregated subset of a table
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at group_by_at sql desc
#' @examples
#' \dontrun{
#' con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' #
#' ## Input: Table name; variable name, space time range information
#' table.name = 'tblsst_AVHRR_OI_NRT'    # table name
#' sel.var = 'sst'                         # choose variable
#' range.var <- list()                     # Range variable [lat,lon,time]
#' range.var$lat <- c(25,30)
#' range.var$lon <- c(-160,-155)
#' range.var$time <- c('2016-03-29', '2016-05-29')
#' #
#' # Aggregate
#' agg.var <- 'time'                       # Specify aggregate variable
#' tbl.subset <- getAggregatedTableData(con, table.name, sel.var, range.var, agg.var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
getAggregatedTableData = function(con, table.name, sel.var, range.var,
                                  agg.var, order.var= NULL){
  tbl.connect <- dplyr::tbl(con,table.name)
  indrv <- tableVarMatch(con, table.name, names(range.var))
  if(any(is.na(indrv))){
    stop("Check range variable;")
  }
  indse <- tableVarMatch(con, table.name, sel.var)
  if(any(is.na(indse))){
    stop("Check selected variable;")
  }
  indag <- tableVarMatch(con, table.name, agg.var)
  if(any(is.na(indag))){
    stop("Check aggregate variable;")
  }
  indsel <- c(indrv, indse)
  tbl.var <- c(names(range.var),sel.var)
  summariseIndex <- match(setdiff(indsel,indag),indsel)
  filt.query <- getFilterQuery(range.var)
  if(is.null(order.var)){
    tbl.query <- tbl.connect %>% dplyr::select(indsel)   %>%
      dplyr::filter(filt.query) %>%
      dplyr::group_by_at(sql(agg.var)) %>%
      dplyr::summarise_at( dplyr::sql(sel.var) , mean,na.rm=TRUE) %>%
      dplyr::arrange(( dplyr::sql(agg.var)  ))

    tbl.subset <-  tbl.query %>% dplyr::collect()
    return(tbl.subset)
  } else {
    indrv <- tableVarMatch(con, table.name, order.var)
    if(any(is.na(indrv))){
      stop("Check order variable;")
    }
    tempOrd <- paste(agg.var,paste0(order.var,collapse = ","),sep = ",")
    tbl.query <- tbl.connect %>% dplyr::select(indsel)    %>%
      dplyr::filter(filt.query) %>%
      dplyr::group_by_at(dplyr::sql(agg.var)) %>%
      dplyr::summarise_at( dplyr::sql(sel.var), mean,na.rm = TRUE) %>%
      dplyr::arrange(dplyr::desc(dplyr::sql(tempOrd))  )

    tbl.subset <-  tbl.query %>% dplyr::collect()
    return(tbl.subset)
  }

}



