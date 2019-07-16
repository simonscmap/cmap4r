
# ------------------- function for subset of data

## Range of the numerical variable of subset of data
# getSubsetRangeNumVar = function(con,table.name,range.var,var.name){
#   query <-  paste("SELECT COUNT_BIG(*) as nObservartion FROM dbo.", table.name,sep = '')
#   condQuery <- paste("where (time BETWEEN '", range.var$date[1] ,"' AND '", range.var$date[2],  "') AND ",
#                      " (lat BETWEEN ", range.var$lat[1] ," AND ", range.var$lat[2],  ") AND ",
#                      " (lon BETWEEN ", range.var$lon[1] ," AND ", range.var$lon[2],  ")", sep = "")
#   query <-  paste(query,condQuery)
#
#   query.range <- vector('list',length = length(var.name))
#   var.range <- NULL
#   for(i in 1:length(var.name)){
#     query.range[[i]] <- paste('SELECT MAX(',var.name[i],') as Max,  MIN(',
#                               var.name[i], ') as Min FROM ',table.name,sep = '')
#     query.range[[i]] <-  paste(query.range[[i]],condQuery)
#     var.range <- rbind(var.range,dbGetQuery(con, query.range[[i]]))
#   }
#   var.range$Variable <- var.name
#   var.range <- var.range[,c(3,1:2)]
#   return(var.range)
# }




#' Get range of all the numerical variables of a subset
#'
#' Range of numeric variables  in a subset of table
#'
#' @param con connection object to the database
#' @param table.name table name in the database
#' @param range.var range of time, latitude, longitude, depth
#' @return table with range of numeric variables
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_if select_at funs
#' @importFrom stats sd
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Input: Table name; variable name, space time range information
#' table.name <- "tblsst_AVHRR_OI_NRT" # table name
#' sel.var <- "sst" # choose variable
#' range.var <- list() # Range variable information
#' range.var$time <- c("2016-04-30", "2016-04-30")
#' range.var$lat <- c(10, 70)
#' range.var$lon <- c(-180, -80)
#' #
#' # Summary of the data:
#' tbl.subsetSummary <- susetsummary_numvar(con, table.name, range.var)
#' print(tbl.subsetSummary)
#' #
#' dbDisconnect(con)
#' }
susetsummary_numvar <- function(con, table.name, range.var) {
  tbl.connect <- dplyr::tbl(con, table.name)
  indrv <- tableVarMatch(con, table.name, names(range.var))
  if (any(is.na(indrv))) {
    stop("Check range variable;")
  }
  filt.query <- getFilterQuery(range.var)
  tbl.query <- tbl.connect %>%
    dplyr::filter(filt.query) %>%
    dplyr::summarise_if(is.numeric, funs(min, max, sd), na.rm = T)
  tbl.subset <- tbl.query %>% dplyr::collect()

  mnx <- grep("min", names(tbl.subset))
  mny <- grep("max", names(tbl.subset))
  msd <- grep("sd", names(tbl.subset))
  varname <- gsub("_min", "", names(tbl.subset)[mnx])

  outp <- data.frame(
    Variable = varname,
    min = as.numeric(tbl.subset[1, mnx]),
    max = as.numeric(tbl.subset[1, mny]),
    sd = as.numeric(tbl.subset[1, msd])
  )
  outp <- outp[outp$Variable != "ID", ]
  return(outp)
}






#' Get space time range of subset of a  table
#'
#' Space time range
#'
#' @param con connection object to the database
#' @param table.name table name in the database
#' @param range.var range of time, latitude, longitude, depth
#' @return table with range of space time variable
#' @export
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Input: Table name; variable name, space time range information
#' table.name <- "tblsst_AVHRR_OI_NRT" # table name
#' sel.var <- "sst" # choose variable
#' range.var <- list() # Range variable information
#' range.var$time <- c("2016-04-30", "2016-04-30")
#' range.var$lat <- c(10, 70)
#' range.var$lon <- c(-180, -80)
#' #
#' # Summary of the data:
#' tbl.subsetSummary <- subsetrange_spacetime(con, table.name, range.var)
#' print(tbl.subsetSummary)
#' #
#' dbDisconnect(con)
#' }
subsetrange_spacetime <- function(con, table.name, range.var) {
  tblxx <- tbl_sample(con, table.name, n = 5)
  tbl.fields <- names(tblxx) # dbListFields(con,table.name)
  range.varxx <- c("time", "lat", "lon", "depth")
  index <- match(range.varxx, tbl.fields)
  range.var2 <- range.varxx[!is.na(index)]
  ab <- dplyr::tbl(con, table.name)
  filt.query <- getFilterQuery(range.var)
  tbl.subsetSpaceTimeSummary <- ab %>%
    dplyr::filter(filt.query) %>%
    dplyr::select_at(range.var2) %>%
    dplyr::summarise_at(range.var2, list(min = min, max = max),
      na.rm = TRUE
    ) %>%
    dplyr::collect()

  return(tbl.subsetSpaceTimeSummary)
}
