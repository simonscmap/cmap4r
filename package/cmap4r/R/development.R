#' Set authorization/API key for accessing the CMAP database.
#'
#' @param reset boolean variable for set authorization key for API access to the database
#' @import magrittr
#' @importFrom keyring key_delete key_set
#' @examples
#' \dontrun{
#' library(cmap4r)
#' # To set the authorization key 
#' set_authorization()
#' # To reset the authorization key 
#' set_authorization(reset = TRUE)
#' }
set_authorization = function(reset = FALSE){
  if (reset){
    keyring::key_delete("cmap_api")
    stop("API authorization key has been deteted.")
  } else {
    print("Enter API authorization key:")
    keyring::key_set(
      service = "cmap_api"
    )
  }
}




#' Check query execution
#'
#' @param query_out output after a query
check_authorization = function(query_out){
  if(rawToChar(query_out$content) == "Unauthorized"){
    print("Please reset API authorization key")
    return(FALSE)
  } else return(TRUE)
}




#' Check range variable format
#'
#' @param range_var output after a query
check_rangevar <- function(range_var) {
  if( !any(names(range_var) %in% "lat"))
    stop("Latitude range is missing.")
  if( !any(names(range_var) %in% "lon"))
    stop("Longitude range is missing.")
  if( !any(names(range_var) %in% "time")){
    print("Time range is missing. Using default date range.")
    range_var$time <- c("2000-01-01", Sys.Date())
  }
  if( !any(names(range_var) %in% "depth")){
    print("Depth range is missing. Using default date range.")
    range_var$depth <- c(0, 1000)
  }
  return(range_var)
}


#' Initialize CMAP database access credentials.
#'
#' @param base_url base url for the database
#' @param route data retrieval route of the database
#' @import magrittr
#' @importFrom keyring key_get
#' @examples
#' \dontrun{
#' library(cmap4r)
#' # Output list with CMAP database access credentials
#' inti_cmap()
#' }
inti_cmap = function(base_url=NULL, route = NULL){
  if(is.null(base_url))
    base_url = 'https://simonscmap.com'
  if(is.null(route)){
    # route = '/dataretrieval/sp?'
    route <- "/api/data/sp?"
  }
  
  tem1 <- try(api_key <-  
                paste("Api-Key",keyring::key_get("cmap_api"),sep = " "),
              silent = T)
  if(!is.null(attr(tem1,"class"))){
    print("API key not available.")
    print("Setting up API authorization key.")
    set_authorization()
    api_key <-  paste("Api-Key",keyring::key_get("cmap_api"),sep = " ")
  }
  return(list(base_url = base_url,route = route, api_key = api_key))
}





#' Retrieve subset of data from a table on CMAP
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @return subset of a table as data frame
#' @export
#' @import jsonlite fromJSON
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' library(cmap4r)
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
#' tbl.subset <- get_table(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
get_table_old <- function(table_name, sel_var, range_var) {
  payload <- inti_cmap()
  query_var <- list()
  query_var$tableName <- table_name
  query_var$fields <- sel_var
  range_var <- check_rangevar(range_var)
  query_var <- modifyList(query_var,range_var)
  names(query_var)[names(query_var) == "time"] <- "dt"
  query_var$spName <- "uspSpaceTime"
  ## Payload and query
  tem1 <- unlist(query_var)
  api_filetr <- paste(paste(names(tem1),tem1,sep="="),collapse = "&")
  api_query <- paste(payload$base_url,payload$route,api_filetr,sep = "")
  get_data <- httr::GET(url=api_query, add_headers(Authorization = payload$api_key))
  raw.content <- rawToChar(get_data$content)
  tem1 <- gsub('\n',',',raw.content)
  tem1 <- paste('[',substring(tem1,first = 1,nchar(tem1)-1),"]",sep = "")
  this.content <- fromJSON(tem1)
  this.content
}





#' Retrieve subset of data from a table on CMAP
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @return subset of a table as data frame
#' @export
#' @import jsonlite fromJSON
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' library(cmap4r)
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
#' tbl.subset <- get_table(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
get_spacetime <- function(table_name, sel_var, range_var) {
  range_var <- check_rangevar(range_var)
  spName <- "uspSpaceTime"
  dt1 <- range_var$time[1]
  dt2 <- range_var$time[2]
  lat1 <- range_var$lat[1]
  lat2 <- range_var$lat[2]
  lon1 <- range_var$lon[1]
  lon2 <- range_var$lon[2]
  depth1 <- range_var$depth[1]
  depth2 <- range_var$depth[2]
  payload <- inti_cmap()
  apiKey <- payload$api_key
  apiKey <- strsplit(apiKey," ")[[1]][2]
  out <- subset_m(spName, table_name, sel_var, dt1, dt2, lat1, lat2, lon1,
                     lon2, depth1, depth2, apiKey)
  return(out)
}



subset_m <- function(spName, table_name, sel_var, dt1, dt2, lat1, lat2, lon1,
                   lon2, depth1, depth2, apiKey){
  query = sprintf('EXEC %s ?, ?, ?, ?, ?, ?, ?, ?, ?, ?', spName)
  args = list(table_name, sel_var, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
  return(stored_proc_m(query, args, apiKey))
}

##' Near-identical function in python. Executes a strored-procedure and returns
##' the results in form of a dataframe. Similar to query()
stored_proc_m <- function(query, args, apiKey){
  payload =list(tableName = args[1],
                fields = args[2],
                dt1 = args[3],
                dt2 = args[4],
                lat1 = args[5],
                lat2 = args[6],
                lon1 = args[7],
                lon2 = args[8],
                depth1 = args[9],
                depth2 = args[10],
                spName = strsplit(toString(query), " ")[[1]][2])
  
  ## ## Special handling of dates (temporary)
  ## payload$dt1 = paste0(payload$dt1, " 00:00:00")
  ## payload$dt2 = paste0(payload$dt2, " 00:00:00")
  # assert_that(validate_sp_args(args[1], args[2], args[3], args[4], args[5],
  #                              args[6], args[7], args[8], args[9], args[10]))
  request(payload, route= "/dataretrieval/query?", apiKey)
}


uspInterval_indiacaror = function(interval){
  if(is.null(interval)){
    return('uspTimeSeries')
  } else if (interval %in% c('w', 'week', 'weekly') ) {
    return( 'uspWeekly')
  } else if (interval %in% c('m', 'month', 'monthly') ){
    return('uspMonthly')
  } else if (interval %in% c( 'q', 's', 'season', 'seasonal', 'seasonality', 'quarterly' ) ){
    return('uspQuarterly')
  } else if (interval %in% c( 'y', 'a', 'year', 'yearly', 'annual' ) ){
    return( 'uspAnnual')
  } else {
    return(stop('Invalid interval'))
  }
}



get_timeseries <- function(table_name, sel_var, range_var, interval=NULL) {
  range_var <- check_rangevar(range_var)
  spName <- uspInterval_indiacaror(interval)
  payload <- inti_cmap()
  apiKey <- payload$api_key
  apiKey <- strsplit(apiKey," ")[[1]][2]
  out <- subset_m(spName, table_name, sel_var, 
                  dt1 = range_var$time[1],
                  dt2 = range_var$time[2],
                  lat1 = range_var$lat[1],
                  lat2 = range_var$lat[2],
                  lon1 = range_var$lon[1],
                  lon2 = range_var$lon[2],
                  depth1 = range_var$depth[1],
                  depth2 = range_var$depth[2],
                  apiKey)
  return(out)
}


get_depthprofile = function(table_name, sel_var, range_var) {
  range_var = check_rangevar(range_var)
  spName = "uspDepthProfile"
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out = subset_m(spName, table_name, sel_var, 
                  dt1 = range_var$time[1],
                  dt2 = range_var$time[2],
                  lat1 = range_var$lat[1],
                  lat2 = range_var$lat[2],
                  lon1 = range_var$lon[1],
                  lon2 = range_var$lon[2],
                  depth1 = range_var$depth[1],
                  depth2 = range_var$depth[2],
                  apiKey)
  return(out)
}




get_section = function(table_name, sel_var, range_var) {
  range_var = check_rangevar(range_var)
  spName = "uspSectionMap"
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out = subset_m(spName, table_name, sel_var, 
                  dt1 = range_var$time[1],
                  dt2 = range_var$time[2],
                  lat1 = range_var$lat[1],
                  lat2 = range_var$lat[2],
                  lon1 = range_var$lon[1],
                  lon2 = range_var$lon[2],
                  depth1 = range_var$depth[1],
                  depth2 = range_var$depth[2],
                  apiKey)
  return(out)
}




# one line function:

# Returns top records of a data set.
tbl_head = function(tableName, rows=5){
  exequery = sprintf('select TOP(%s) * FROM %s', rows, tableName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  return(query(exequery,apiKey))
}


# Returns the list of columns of a data set.
tbl_columns = function(tableName){
  exequery = sprintf("SELECT COLUMN_NAME [Columns] FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'%s'",tableName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}



get_var = function(tableName, varName){
  # Returns a single-row dataframe from tblVariables containing info associated with varName.
  # This method is mean to be used internally and will not be exposed at documentations.
  exequery = sprintf(
    "SELECT * FROM tblVariables WHERE Table_Name='%s' AND Short_Name='%s'",tableName,varName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}










## get_section, get_timeseries, get_depthprofile, get_xxxx

library(httr) 
library(rjson)   
library(jsonlite)

table_name <- "tblsst_AVHRR_OI_NRT" # table name
sel_var <- "sst" # choose variable
range_var <- list() # Range variable [lat,lon,time]
range_var$lat <- c(10, 70)
range_var$lon <- c(-180, -80)
range_var$time <- c("2016-04-30", "2016-04-30")
#
## Subset selection:
# tbl.subset <- get_table(table_name, sel_var, range_var)
# head(tbl.subset)

tbl.subset <- get_spacetime(table_name, sel_var, range_var)
head(tbl.subset)





api_query <-  "https://simonscmap.com/dataretrieval/query?query=EXEC+uspCatalog"

get_catalog = function(){
  payload <- inti_cmap(route = )
  query_var <- list()
  query_var$spName <- "uspCatalog"
  tem1 <- unlist(query_var)
  api_filetr <- paste(paste(names(tem1),tem1,sep="="),collapse = "&")
  # api_query <- paste(base_url,api_filetr,sep = "")
  api_query <- paste(payload$base_url,payload$route,api_filetr,sep = "")
  get_data <- httr::GET(url=api_query, add_headers(Authorization = payload$api_key))
  raw.content <- rawToChar(get_data$content)
  tem1 <- gsub('\n',',',raw.content)
  tem1 <- paste('[',substring(tem1,first = 1,nchar(tem1)-1),"]",sep = "")
  this.content <- fromJSON(tem1)
  this.content
}
  
return self.query('EXEC uspCatalog')





















## --------------------------------

x
get_var_catalog = function(tableName, varName){
  # Returns a single-row dataframe from catalog (udfCatalog) containing all of the variable's info at catalog.
  exequery = sprintf(
    "SELECT * FROM [dbo].udfCatalog() WHERE Table_Name='%s' AND Variable='%s'",tableName,varName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}


get_var_long_name = function(tableName, varName){
  # """Returns the long name of a given variable."""
  return(get_var(tableName, varName)[1,'Long_Name'])
}



get_unit = function(tableName, varName){
  # """Returns the unit of a given variable."""
  return(get_var(tableName, varName)[1,'Unit'])
}


get_var_resolution = function(tableName, varName){
  # Returns a single-row dataframe from catalog (udfCatalog) containing the variable's spatial and temporal resolutions.
  return(get_var(tableName, varName)[1,c('Temporal_Resolution', 'Spatial_Resolution')])
}


get_var_coverage = function(tableName, varName){
  # Returns a single-row dataframe from catalog (udfCatalog) containing the variable's spatial and temporal coverage.
  return(get_var(tableName, varName)[1,c('Time_Min', 'Time_Max', 'Lat_Min', 'Lat_Max', 'Lon_Min', 'Lon_Max', 'Depth_Min', 'Depth_Max')])
}


get_var_stat = function(tableName, varName){
  # Returns a single-row dataframe from catalog (udfCatalog) containing the variable's summary statistics.
  return(get_var(tableName, varName)[1,c( 'Variable_Min', 'Variable_Max', 'Variable_Mean', 'Variable_Std', 'Variable_Count', 'Variable_25th', 'Variable_50th', 'Variable_75th' )])
}



##' Helper to get response to tibble.
process_response_to_tibble <- function(getdata){
  this.raw.content <- rawToChar(getdata$content)
  a <- gsub('\n',',',this.raw.content)
  b <- paste('[',substring(a,first = 1,nchar(a)-1),"]",sep = "")
  this.content <- fromJSON(b)
  return(this.content)
}
  



has_field  = function (tableName, varName){
  # tableName = 'tblAltimetry_REP'; varName = 'sla'
  # Returns a boolean confirming whether a field (varName) exists in a table (data set).
  exequery = sprintf("SELECT COL_LENGTH('%s', '%s') AS RESULT ",tableName,varName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  df <- out$RESULT
  hasField = FALSE
  if (length(df)>0) hasField = TRUE
  return(hasField)
}




# is_grid  = function (tableName, varName){
#   # Returns a boolean indicating whether the variable is a gridded product or has irregular spatial resolution.
#   grid = TRUE
#   exequery = "SELECT Spatial_Res_ID, RTRIM(LTRIM(Spatial_Resolution)) AS Spatial_Resolution FROM tblVariables "
#   exequery = paste(exequery, "JOIN tblSpatial_Resolutions ON [tblVariables].Spatial_Res_ID=[tblSpatial_Resolutions].ID ", sep="")
#   exequery = paste(exequery,sprintf("WHERE Table_Name='%s' AND Short_Name='%s' ",tableName,varName), sep="")
#   payload = inti_cmap()
#   apiKey = payload$api_key
#   apiKey = strsplit(apiKey," ")[[1]][2]
#   df <- query(exequery,apiKey)
#   if (length(df)<1) return(NULL)
#   ////if df.Spatial_Resolution[0].lower().find('irregular') != -1:
#     ////  grid = False
#   return(grid)
# }


