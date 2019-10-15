#' Get the number of observations in the table
#'
#' Number of observations
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @param type method of getting number of observation: a) "approx" - fast retrieval but inaccurate;
#' b) "first-last"- using ID of first and last observation; c) "exact" - use COUNT_BIG to get exact count
#' @param idvar ID variable used in "first-last" method
#' @return number of observation in the table
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at group_by_at
#' @importFrom DBI dbGetQuery dbClearResult
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Choose table:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#'
#' # Number of observations
#' nObs <- tbl_nobs(con, table_name)
#' nObs
#' #
#' dbDisconnect(con)
#' }
tbl_nobs <- function(con, table_name,
                     type = c("approx", "first-last", "exact")[1],
                     idvar = "ID") {
  # require(dbplyr,dplyr,DBI)
  id <- match(type, c("approx", "first-last", "exact"))
  if (id == 1) {
    query <- paste("SELECT CONVERT(bigint, rows) FROM sysindexes WHERE id = OBJECT_ID('", table_name,
      "') AND indid < 2",
      sep = ""
    )
    ab <- DBI::dbGetQuery(con, query)
    return(as.numeric(ab[1, ]))
  } else if (id == 2) {
    query0 <- paste("SELECT TOP 1 ", idvar, " FROM ", table_name, " ORDER BY ", idvar, sep = "")
    query <- paste(query0, "ASC")
    ab <- DBI::dbGetQuery(con, query)
    fobs <- as.numeric(ab$ID)

    query <- paste(query0, "DESC")
    ab <- DBI::dbGetQuery(con, query)

    return(as.numeric(ab$ID) - fobs + 1)
  } else {
    query <- paste("SELECT COUNT_BIG(*) as nObservation FROM ", table_name, sep = "")
    ab <- DBI::dbGetQuery(con, query)
    nobs <- as.numeric(ab$nObservation)
    DBI::dbClearResult(ab)
    return(nobs)
  }
}







#' Get space time range of a  table
#'
#' Space time range of a table
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @return table with range of space time variable
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Choose table:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#' #
#' #
#' # Space/time information of the table
#' tbl.spaceTimeInfo <- tbl_spacetime_range(con, table_name)
#' print(tbl.spaceTimeInfo)
#' #
#' dbDisconnect(con)
#' }
tbl_spacetime_range <- function(con, table_name) {
  tblxx <- tbl_sample(con, table_name, n = 5)
  tbl.fields <- names(tblxx) # dbListFields(con,table_name)
  range.var <- c("time", "lat", "lon", "depth")
  index <- match(range.var, tbl.fields)
  range.var2 <- range.var[!is.na(index)]
  ab <- dplyr::tbl(con, table_name)
  am <- ab %>%
    dplyr::select_at(range.var2) %>%
    dplyr::summarise_at(range.var2, list(min = min, max = max), na.rm = TRUE) %>%
    dplyr::collect()

  return(am)


  ## ## Temporary
  ## range.var <- c("time")
  ## index <- match(range.var, tbl.fields)
  ## range.var2 <- range.var[!is.na(index)]
  ## ab <- dplyr::tbl(con, table_name)
  ## am <- ab %>% dplyr::select_at(range.var2) %>%
  ## browser()
  ## am <- ab %>%
  ##   dplyr::select_at(range.var2) %>% summarise_at(range.var2, class) %>% collect()

  ##   dplyr::summarise_at(range.var2, list(min = min, max = max), na.rm = TRUE) %>%
  ##   dplyr::collect()


  ## ## An important question: what is the time zone when time-of-day information
  ## ## is missing? Should we infer it from the latitude and longitude?
  ## ## End of temporary

}




#' Get range of all the numerical variables
#'
#' Range of numeric variables
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @return table with range of numeric variables
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' # Choose table:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#' #
#' # Numeric variable range:
#' tbl.rangeNumVar <- tbl_numvar_range(con, table_name)
#' print(tbl.rangeNumVar)
#' #
#' dbDisconnect(con)
#' }
tbl_numvar_range <- function(con, table_name) {
  tbl.colClass <- tbl_vartype(con, table_name)
  var.name <- as.character(tbl.colClass$Variable[tbl.colClass$Type == "numeric"])

  ab <- dplyr::tbl(con, table_name)
  am <- ab %>%
    dplyr::select_at(var.name) %>%
    dplyr::summarise_at(var.name, list(min = min, max = max), na.rm = TRUE) %>%
    dplyr::collect()

  p <- length(var.name)
  outp <- data.frame(
    Variable = var.name,
    min = as.numeric(am[1, 1:p]),
    max = as.numeric(am[1, -(1:p)])
  )
  return(outp)
}






# ## Range of the numerical variable
# tbl_numvar_range1 = function(con,table_name,var.name){
#   query.range <- vector('list',length = length(var.name))
#   var.range <- NULL
#   for(i in 1:length(var.name)){
#     query.range[[i]] <- paste('SELECT MAX(',var.name[i],') as Max,  MIN(',
#                               var.name[i], ') as Min  FROM ',table_name,sep = '')
#     var.range <- rbind(var.range,dbGetQuery(con, query.range[[i]]))
#   }
#   var.range$Variable <- var.name
#   return(var.range)
# }





#' Get the sample of size n from a table
#'
#' Retrieve data sample of size n
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @param n size of sample to retrieve
#' @return output data frame
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Choose table:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#'
#' ## collect sample data
#' tbl.fields <- tbl_sample(con, table_name, n = 10)
#' print(tbl.fields)
#'
#' dbDisconnect(con)
#' }
tbl_sample <- function(con, table_name, n) {
  query <- paste("select * from", table_name)
  rs <- DBI::dbSendQuery(con, query)
  tbl_sample <- DBI::dbFetch(rs, n)
  DBI::dbClearResult(rs)
  tbl_sample
}







#' Get class of columns in the table
#'
#' Class of columns
#'
#' @param con connection object to the database
#' @param table_name table name in the database
#' @return column name with its class
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Choose table:
#' table_name <- "tblsst_AVHRR_OI_NRT"
#' #
#' # Class of each column in the table
#' tbl.colClass <- tbl_vartype(con, table_name)
#' print(tbl.colClass)
#' #
#' dbDisconnect(con)
#' }
tbl_vartype <- function(con, table_name) {
  query <- paste("select * from", table_name)
  rs <- DBI::dbSendQuery(con, query)
  tbl_sample <- DBI::dbFetch(rs, 10)
  colClass <- NULL
  for (i in 1:ncol(tbl_sample)) {
    colClass <- rbind(
      colClass,
      c(
        names(tbl_sample)[i],
        class(tbl_sample[, i])
      )
    )
  }
  colnames(colClass) <- c("Variable", "Type")
  return(data.frame(colClass))
}
