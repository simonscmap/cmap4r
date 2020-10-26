#####################################################################
###  All functions here are "CMAP data retrieval" functions       ###
#####################################################################



#' Retrieve the subset of a table from the Simons CMAP databse using the space-time range inputs (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2).
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Pass the input "*" to retrieve all fields in the table.  Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor. Defaults to 0 if not provided.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor. Defaults to 0 if not provided.
#' @return required subset of the table is ordered by time, lat, lon, and depth (if exists).
#' @export
#' @examples
#' \donttest{
#' ## Input: Table name; variable name, space time range information
#' tableName <- "tblsst_AVHRR_OI_NRT" # table name
#' varName <- "sst" # choose variable
#' # Range variable [lat,lon,time]
#' lat1 = 10; lat2 = 70
#' lon1 = -180; lon2 = -80
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' #
#' ## Subset selection:
#' tbl.subset <- get_spacetime(tableName, varName, lat1, lat2,
#'                                lon1, lon2, dt1, dt2)
#' head(tbl.subset)
#' #
#' }
get_spacetime <- function(tableName, varName, lat1, lat2,
                          lon1, lon2, dt1, dt2,
                          depth1 = NULL, depth2 = NULL) {
  range_var <- list()
  range_var$lat <- c(lat1, lat2)
  range_var$lon <- c(lon1, lon2)
  range_var$time <- c(dt1, dt2)
  range_var$depth <- c(depth1, depth2)
  range_var <- check_rangevar(range_var)
  spName <- "uspSpaceTime"
  apiKey = get_api_key()
  out = subset(spName, tableName, varName,
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




#' Retrieve section of a table from the Simons CMAP databse using spatio-temporal range parameters.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. Pass the input "*" to retrieve all fields in the table.  Use "get_catalog()" to retrieve list of table variables on the database. 
#' @param dt1 start date or datetime (lower bound of temporal cut). Example values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90° to 90°.
#' @param lon1 start longitude [degree E]  of the zonal cut; ranges from  -180° to 180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from  -180° to 180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical cut. Note that depth  is 0 at surface and grows towards ocean floor.
#' @return required subset of the table is ordered by time, lat, lon, and depth (if exists).
#' @export
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name, space time range information
#' tableName <- "tblPisces_NRT" # table name
#' varName <- "NO3" # choose variable
#' # Range variable [lat,lon,time]
#' lat1 = 10; lat2 = 60
#' lon1 = -160; lon2 = -158
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- 0; depth2 =  5000
#' #
#' ## Subset selection:
#' tbl.subset <- get_section(tableName, varName, lat1, lat2, lon1, lon2,
#'                    dt1, dt2, depth1, depth2)
#' head(tbl.subset)
#' #
#' }
get_section <- function(tableName, varName, lat1, lat2,
                        lon1, lon2, dt1, dt2,
                        depth1 = NULL, depth2 = NULL) {
  range_var <- list()
  range_var$lat <- c(lat1, lat2)
  range_var$lon <- c(lon1, lon2)
  range_var$time <- c(dt1, dt2)
  range_var$depth <- c(depth1, depth2)
  range_var = check_rangevar(range_var)
  spName = "uspSectionMap"
  apiKey = get_api_key()
  out = subset(spName, tableName, varName,
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



#' Retrieve the subset of a table from Simons CMAP using the space-time range
#' inputs (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2). The output is
#' aggregated by time, i.e., we compute the mean and standard deviation of a
#' variable of interest in the specified range of inputs. Also, the function is
#' capable of binning the timeseries data weekly, monthly, quarterly, or
#' annually.
#'
#' @param tableName table name from the Simons CMAP database. Use
#'   "get_catalog()" to retrieve list of tables on the database.
#' @param varName specify short name of a variable in the table.  Use
#'   "get_catalog()" to retrieve list of table variables on the database.
#' @param dt1 start date or datetime (lower bound of temporal cut). Example
#'   values: '2016-05-25' or '2017-12-10 17:25:00'
#' @param dt2 end date or datetime (upper bound of temporal cut). Example
#'   values: '2016-04-30' or '2016-04-30 17:25:00'
#' @param lat1 start latitude [degree N] of the meridional cut; ranges from -90°
#'   to 90°.
#' @param lat2 end latitude [degree N] of the meridional cut; ranges from -90°
#'   to 90°.
#' @param lon1 start longitude [degree E] of the zonal cut; ranges from -180° to
#'   180°.
#' @param lon2 end longitude [degree E] of the zonal cut; ranges from -180° to
#'   180°.
#' @param depth1 positive value specifying the start depth [m] of the vertical
#'   cut. Note that depth is 0 at surface and grows towards ocean floor.
#' @param depth2 positive value specifying the end depth [m]of the vertical
#'   cut. Note that depth is 0 at surface and grows towards ocean floor.
#' @param interval choose time series data aggregation at [weekly, montly,
#'   quarterly, annual, none]. Default none.
#' @return required aggregated output is ordered by time, lat, lon, and depth
#'   (if exists).
#' @export
#' @examples
#' \dontrun{
#' ## Input: Table name; variable name, space time range information
#' tableName <- "tblHOT_Bottle" # table name
#' varName <- "SiO4_bottle_hot" # choose variable
#' # Range variable [lat,lon,time]
#' lat1 = 22; lat2 = 23
#' lon1 = -159; lon2 = -157
#' dt1 = "1988-12-01"; dt2 = "2016-10-15"
#'
#' # Subset selection:
#' tbl.subset <- get_timeseries(tableName, varName, lat1, lat2,
#'                                 lon1, lon2, dt1, dt2)
#' head(tbl.subset)
#' #
#' }
get_timeseries <- function(tableName, varName, lat1, lat2,
                           lon1, lon2, dt1, dt2,
                           depth1 = NULL, depth2 = NULL,
                           interval=NULL) {
  range_var <- list()
  range_var$lat <- c(lat1, lat2)
  range_var$lon <- c(lon1, lon2)
  range_var$time <- c(dt1, dt2)
  range_var$depth <- c(depth1, depth2)
  range_var <- check_rangevar(range_var)
  spName <- uspInterval_indicator(interval)
  apiKey = get_api_key()
  out <- subset(spName, tableName, varName,
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




#' Retrieve the subset of a table from the Simons CMAP database using the
#' space-time range inputs (dt1, dt2, lat1, lat2, lon1, lon2, depth1,
#' depth2). The output is aggregated by depth, i.e., we compute the mean and
#' standard deviation of a variable of interest in the specified range of
#' inputs. Also, the function is capable of binning the timeseries data weekly,
#' monthly, quarterly, or annually.
#' 
#' Retrieve subset of a table aggregated by depth from the Simons CMAP databse
#' using the space-time range inputs.
#' 
#' Compute mean and standard deviation at each depth level based on the range of
#' lattitude, longitude and time inputs.
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
#' @return required aggregated output is ordered by depth. 
#' @export
#' @examples
#' \dontrun{
#' 
#' #
#' ## Input: Table name; variable name, space time range information
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # choose variable
#' # Range variable [lat,lon,time]
#' lat1 = 20; lat2 = 24
#' lon1 = -170; lon2 = -150
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' depth1 <- 0; depth2 =  1500
#' #
#' ## Subset selection:
#' tbl.subset <- get_depthprofile(tableName, varName, lat1, lat2, lon1, lon2,
#'                                dt1, dt2, depth1, depth2)
#' head(tbl.subset)
#' #
#' }
get_depthprofile <- function(tableName, varName, lat1, lat2,
                            lon1, lon2, dt1, dt2,
                            depth1, depth2) {
  range_var <- list()
  range_var$lat <- c(lat1, lat2)
  range_var$lon <- c(lon1, lon2)
  range_var$time <- c(dt1, dt2)
  range_var$depth <- c(depth1, depth2)
  range_var = check_rangevar(range_var)
  spName = "uspDepthProfile"
  apiKey = get_api_key()
  out = subset(spName, tableName, varName,
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








#' Retrieve data using the user designed SQL query.
#'
#' @param manual_query own query of the user.
#' @return output is returned as dataframe object.
#' @export
#' @examples
#' \donttest{
#' #
#' manual_query = "SELECT [time], lat, lon, depth, Fe FROM tblPisces_NRT
#'   WHERE
#' [time] BETWEEN '2017-06-03' AND '2017-06-03' AND
#' lat BETWEEN 10 AND 55 AND
#' lon BETWEEN -180 AND 100 AND
#' depth BETWEEN 0 AND 0.5
#' ORDER BY [time], lat, lon, depth"
#' #
#' ## Subset selection:
#' tbl.subset <- exec_manualquery(manual_query)
#' head(tbl.subset)
#' #
#' }
exec_manualquery <- function(manual_query) {
  apiKey = get_api_key()
  out <- query(manual_query,apiKey)

  return(out)
}






# space_time <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
#   apiKey = get_api_key()
#   subset('uspSpaceTime', table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, apiKey)
# }
#
#



#' #' Returns a subset of data according to space-time constraints.  The results
#' #' are ordered by time, lat, lon, and depth.
#' #'
#' #' {
#' #' 
#' #' table <- "tblsst_AVHRR_OI_NRT"
#' #' variable <- "sst"
#' #' lat1 = 10
#' #' lat2 = 70
#' #' lon1 = -180
#' #' lon2 = -80
#' #' dt1 = "2016-04-30"
#' #' dt2 = "2016-04-30"
#' #' depth1 = 0
#' #' depth2 = 30
#' #' section(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
#' #' }
#' section <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
#'   apiKey = get_api_key()
#'   return(subset('uspSectionMap', table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, apiKey))
#' }





#' Retrieve timeseries data from  CMAP using space time range parameters
#'
#' Get subset of a table as data frame
#'
#'  interval choose time series data aggregation at [weekly, montly, quaterly, annual, none]
#'  subset of a table as data frame
#' {
#' 
#' #
#' table <- "tblHOT_Bottle" # table name
#' variable <- "SiO4_bottle_hot" # choose variable
#' lat1 = 22
#' lat2 = 23
#' lon1 = -159
#' lon2 = -157
#' dt1 = 1988-12-01
#' dt2 = 2016-10-15
#' time_series(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2) ## not complete
#' #
#' }
#' # time_series <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, interval=NULL){#   #   spName <- uspInterval_indicator(interval)#   apiKey = get_api_key()#   out <- subset(spName, tableName, varName, dt1, dt2, lat1, lat2, lon1, lon2,#                 depth1, depth2, apiKey)#   return(out)#





#' #' Retrieve depth profile data from  CMAP using space time range parameters
#' #'
#' #' Get subset of a table as data frame
#' #'
#' #' @param tableName table name in the database
#' #' @param varName select a variable in the table
#' #' @param range_var range of time, latitude, longitude, depth
#' #' @return subset of a table as data frame
#' #'
#' #'
#' #' {
#' #' 
#' #' #
#' #' ## Input: Table name; variable name, space time range information
#' #' table = "tblArgoMerge_REP"
#' #' variable = "argo_merge_chl_adj"
#' #' lat1 = 20
#' #' lat2 = 24
#' #' lon1 = -170
#' #' lon2 = -159
#' #' dt1 = "2016-04-30"
#' #' dt2 = "2016-04-30"
#' #' depth1 = 0
#' #' depth2 = 1500
#' #' depthprofile(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
#' #' #
#' #' }
#' depth_profile <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
#'   apiKey = get_api_key()
#'   out = subset("uspDepthProfile", tableName, varName, dt1, dt2, lat1, lat2, lon1, lon2,
#'                depth1, depth2, apiKey)
#'   return(out)
#' }
