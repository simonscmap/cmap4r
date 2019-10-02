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
    range_var$depth <- c(0, 0)
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








##' Perform subset operation ona  given table based on lat, lon, time and depth range.
subset_m <- function(spName, table_name, sel_var, dt1, dt2, lat1, lat2,
                     lon1, lon2, depth1, depth2, apiKey){
  query = sprintf('EXEC %s ?, ?, ?, ?, ?, ?, ?, ?, ?, ?', spName)
  args = list(table_name, sel_var, dt1, dt2, lat1, lat2,
              lon1, lon2, depth1, depth2)
  return(stored_proc_m(query, args, apiKey))
}





#' Assign relevant input to the structure calling stored procedure in the database.
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
  # assert_that(validate_sp_args(args[1], args[2], args[3],
  #                              args[4], args[5],
  #                              args[6], args[7], args[8],
  #                              args[9], args[10]))

  # route = '/dataretrieval/sp?'     # JSON format, deprecated
  route = '/api/data/sp?'     # CSV format
  request(payload, route, apiKey)
}




#' retrieve stored procedure for extracting timeseries data aggregated at weekely, monthly, quaterly,  annual or none level.
uspInterval_indiacaror = function(interval){
  if(is.null(interval)){
    return('uspTimeSeries')
  } else if (interval %in% c('w', 'week', 'weekly') ) {
    return( 'uspWeekly')
  } else if (interval %in% c('m', 'month', 'monthly') ){
    return('uspMonthly')
  } else if (interval %in% c( 'q', 's', 'season', 'seasonal',
                              'seasonality', 'quarterly' ) ){
    return('uspQuarterly')
  } else if (interval %in% c( 'y', 'a', 'year', 'yearly', 'annual' ) ){
    return( 'uspAnnual')
  } else {
    return(stop('Invalid interval'))
  }
}


##' From the data extracted through the api interface to the data frame:
##' @import jsonlite fromJSON
process_response_to_tibble1 <- function(getdata){
  this.raw.content <- rawToChar(getdata$content)
  a <- gsub('\n',',',this.raw.content)
  b <- paste('[',substring(a,first = 1,nchar(a)-1),"]",sep = "")
  this.content <- jsonlite::fromJSON(b)
  return(this.content)
}





# ----------------------------------data retrieval module ----

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
#' tbl.subset <- get_table_old(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
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
  get_data <- httr::GET(url=api_query, httr::add_headers(Authorization = payload$api_key))
  return(process_response_to_tibble(get_data))
}







#' Retrieve subset of a table from CMAP using space/time range suggested
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @return subset of a table as data frame
#' @export
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
#' tbl.subset <- get_spacetime(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' }
get_spacetime <- function(table_name, sel_var, range_var) {
  range_var <- check_rangevar(range_var)
  spName <- "uspSpaceTime"
  payload <- inti_cmap()
  apiKey <- payload$api_key
  apiKey <- strsplit(apiKey," ")[[1]][2]
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




#' Retrieve subset of a table from CMAP using space time range parameters
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @return subset of a table as data frame
#' @export
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
#' tbl.subset <- get_section(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' }
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







#' Retrieve timeseries data from  CMAP using space time range parameters
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @param interval choose time series data aggregation at [weekly, montly, quaterly, annual, none]
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name, space time range information
#' table_name <- "tblHOT_Bottle" # table name
#' sel_var <- "SiO4_bottle_hot" # choose variable
#' range_var <- list() # Range variable [lat,lon,time]
#' range_var$lat <- c(22, 23)
#' range_var$lon <- c(-159, -157)
#' range_var$time <- c("1988-12-01", "2016-10-15")
#'
#' # Subset selection:
#' tbl.subset <- get_timeseries(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' }
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



#' Retrieve depth profile data from  CMAP using space time range parameters
#'
#' Get subset of a table as data frame
#'
#' @param table_name table name in the database
#' @param sel_var select a variable in the table
#' @param range_var range of time, latitude, longitude, depth
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name, space time range information
#' table_name <- "tblArgoMerge_REP" # table name
#' sel_var <- "argo_merge_chl_adj" # choose variable
#' range_var <- list() # Range variable [lat,lon,time]
#' range_var$lat <- c(20, 24)
#' range_var$lon <- c(-170, -150)
#' range_var$time <- c("2016-04-30", "2016-04-30")
#' range_var$depth <- c(0, 1500)
#' #
#' ## Subset selection:
#' tbl.subset <- get_depthprofile(table_name, sel_var, range_var)
#' head(tbl.subset)
#' #
#' }
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









# ----------------table query module function -------------------

#' Returns top n rows records from a table on CMAP
#'
#' data frame of top n rows
#'
#' @param table_name table name in the database
#' @param nrows number of rows
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name;
#' table_name <- "tblArgoMerge_REP" # table name
#' #
#' ## Subset selection:
#' tbl.subset <- tbl_head(table_name, nrows=10)
#' head(tbl.subset)
#' #
#' }
tbl_head = function(tableName, nrows=5){
  exequery = sprintf('select TOP(%s) * FROM %s', nrows, tableName)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  return(query(exequery,apiKey))
}



#' Returns the list of columns of a data set.
#'
#'
#' @param table_name table name in the database
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name;
#' table_name <- "tblArgoMerge_REP" # table name
#' #
#' ## Subset selection:
#' tbl.columns <- tbl_columns(table_name)
#' tbl.columns
#' #
#' }
tbl_columns = function(table_name){
  exequery = sprintf("SELECT COLUMN_NAME [Columns] FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'%s'",table_name)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}




#' Returns catalog of the CMAP data base.
#'
#'
#' @param apiKey API access key of the CMAP
#' @return table of catalog
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: apiKey
#' #
#' ## Variable attribute:
#' cmap.catalog <- get_catalog()
#' cmap.catalog
#' #
#' }
get_catalog <- function(apiKey = NULL){
  myquery = 'EXEC uspCatalog'
  if(is.null(apiKey)){
    payload = inti_cmap()
    apiKey = payload$api_key
    apiKey = strsplit(apiKey," ")[[1]][2]
    df = query(myquery, apiKey)
  } else {
    df = query(myquery, apiKey)
  }
  return(df)
}





#' Returns a single-row dataframe from tblVariables containing info associated with var_name.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' tbl.var <- get_var_attr(table_name, var_name)
#' tbl.var
#' #
#' }
get_var_attr = function(table_name, var_name){
  # This method is mean to be used internally and will not be exposed at documentations.
  exequery = sprintf(
    "SELECT * FROM tblVariables WHERE Table_Name='%s' AND Short_Name='%s'",table_name,var_name)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}



#' Returns a single-row dataframe from catalog (udfCatalog) containing all of the variable's info at catalog.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' tbl.catlog.var <- get_var_catalog(table_name, var_name)
#' tbl.catlog.var
#' #
#' }
get_var_catalog = function(table_name, var_name){
  #
  exequery = sprintf(
    "SELECT * FROM [dbo].udfCatalog() WHERE Table_Name='%s' AND Variable='%s'",table_name,var_name)
  payload = inti_cmap()
  apiKey = payload$api_key
  apiKey = strsplit(apiKey," ")[[1]][2]
  out <- query(exequery,apiKey)
  return(out)
}




#' Returns long name of the variable.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varLonName <- get_var_long_name(table_name, var_name)
#' varLonName
#' #
#' }
get_var_long_name = function(table_name, var_name){
  # """Returns the long name of a given variable."""
  return(get_var_catalog(table_name, var_name)[1,'Long_Name'])
}


#' Returns unit of the variable.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' unitName <- get_var_unit(table_name, var_name)
#' unitName
#' #
#' }
get_var_unit = function(table_name, var_name){
  # """Returns the unit of a given variable."""
  return(get_var_catalog(table_name, var_name)[1,'Unit'])
}



#' Returns resolution of the variable.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varResolution <- get_var_resolution(table_name, var_name)
#' varResolution
#' #
#' }
get_var_resolution = function(table_name, var_name){
  return(get_var_catalog(table_name, var_name)[1,c('Temporal_Resolution', 'Spatial_Resolution')])
}




#' Returns coverage of the variable.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varCoverage <- get_var_coverage(table_name, var_name)
#' varCoverage
#' #
#' }
get_var_coverage = function(table_name, var_name){
  return(get_var_catalog(table_name, var_name)[1,c('Time_Min', 'Time_Max', 'Lat_Min', 'Lat_Max', 'Lon_Min', 'Lon_Max', 'Depth_Min', 'Depth_Max')])
}



#' Returns summary statistics of the variable.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varStats <- get_var_stat(table_name, var_name)
#' varStats
#' #
#' }
get_var_stat = function(table_name, var_name){
  return(get_var_catalog(table_name, var_name)[1,c( 'Variable_Min', 'Variable_Max', 'Variable_Mean', 'Variable_Std', 'Variable_Count', 'Variable_25th', 'Variable_50th', 'Variable_75th' )])
}




#' Returns boolean output checking if the variable exist in the table.
#'
#'
#' @param table_name table name in the database
#' @param var_name name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' var_exist <- has_field(table_name, var_name)
#' var_exist
#' #
#' }
has_field  = function (table_name, var_name){
  # table_name = 'tblAltimetry_REP'; var_name = 'slas'
  exequery = sprintf("SELECT COL_LENGTH('%s', '%s') AS RESULT ",
                     table_name,var_name)
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


