#############################################################################
### All functions here are "interface" functions i.e. the user has access ###
#############################################################################

#' Returns a boolean confirming whether a field (varName) exists in a table (data set).
#' @param tableName Table name.
#' @param varName Variable name.
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
has_field<- function(tableName, varName){

  apiKey = get_api_key()
  myquery = sprintf("SELECT COL_LENGTH('%s', '%s') AS RESULT ", tableName, varName)
  return(length(query(myquery, apiKey)[1, 'RESULT'])>0)
  ## TODO: test the above line.
}



#' Returns a dataframe containing a list of all of the hosted cruise names.
#' @export
cruises <- function(){
  apiKey = get_api_key()
  myquery = "EXEC uspCruises"
  df = query(myquery, apiKey)
  return(df)
}


#' Returns a dataframe containing all registered variables (at Simons CMAP) during a cruise.
#' @export
cruise_variables = function(cruiseName){
  df = cruise_by_name(cruiseName)
  exequery <- sprintf('SELECT * FROM dbo.udfCruiseVariables(%d) ', df$ID[1])
  apiKey = get_api_key()
  df <- query(exequery,apiKey)
  return(df)
}


#' Returns catalog of the CMAP data base.
#'
#'
#' @return table of catalog
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
#' #
#' #
#' ## Variable attribute:
#' cmap.catalog <- get_catalog()
#' cmap.catalog
#' #
#' }
get_catalog <- function(){
  apiKey = get_api_key()
  myquery = 'EXEC uspCatalog'
  df = query(myquery, apiKey)
}



#' Returns a dataframe containing cruise info using cruise name.
#' @param cruisename Name of cruise.
#' @export
cruise_by_name <- function(cruisename){

  ## Form EXEC statement.
  myquery = sprintf("EXEC uspCruiseByName '%s' ", cruisename)

  ## Issue query
  apiKey = get_api_key()
  df = query(myquery, apiKey)

  ## Check results
  if(nrow(df)<1){
    stop(sprintf('Invalid cruise name: %s' , cruiseName))
  }
  if(nrow(df)>1){
    df$Keywords <- NULL
    print(df)
    message('More than one cruise found. Please provide a more specific cruise name: ')
  }
  return(df)
}

#' Returns a dataframe containing cruise boundaries in space and time.
#' @param cruisename Name of cruise.
#' @export
cruise_bounds <- function(cruisename){
  apiKey = get_api_key()
  df = cruise_by_name(cruisename)
  myquery = sprintf("EXEC uspCruiseBounds %d", unlist(df[['ID']][1]))
  df = query(myquery, apiKey)
  return(df)
}

#' Returns a dataframe containing the cruise trajectory (Identical function in python)
#' @export
cruise_trajectory <- function(cruisename){

  apiKey = get_api_key()
  df = cruise_by_name(cruisename)
  myquery = sprintf("EXEC uspCruiseTrajectory %d", unlist(df[['ID']][1]))
  df = query(myquery, apiKey)

  ## ## Temporary, until Mohammad fixes the query issue
  ## df[["time"]] = sapply(df[["time"]], substr, 1, 19)

  return(df)
}


#' Returns a subset of data according to
#' space-time constraints.  The results are ordered by time, lat, lon, and
#' depth (if exists).
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
space_time <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
  apiKey = get_api_key()
  subset('uspSpaceTime', table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, apiKey)
}


#' Returns a subset of data according to space-time constraints.  The results
#' are ordered by time, lat, lon, and depth.
#' @examples
#' \dontrun{
#' library(cmap4r)
#' table <- "tblsst_AVHRR_OI_NRT"
#' variable <- "sst"
#' lat1 = 10
#' lat2 = 70
#' lon1 = -180
#' lon2 = -80
#' dt1 = "2016-04-30"
#' dt2 = "2016-04-30"
#' depth1 = 0
#' depth2 = 30
#' section(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
#' }
#'
section <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
  apiKey = get_api_key()
  return(subset('uspSectionMap', table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, apiKey))
}



#' Retrieve timeseries data from  CMAP using space time range parameters
#'
#' Get subset of a table as data frame
#'
#' @param interval choose time series data aggregation at [weekly, montly, quaterly, annual, none]
#' @return subset of a table as data frame
#' @export
#' @examples
#' \dontrun{
#' library(cmap4r)
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
time_series <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2, interval=NULL){

  spName <- uspInterval_indicator(interval)
  apiKey = get_api_key()
  out <- subset(spName, table_name, sel_var, dt1, dt2, lat1, lat2, lon1, lon2,
                depth1, depth2, apiKey)
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
#' table = "tblArgoMerge_REP"
#' variable = "argo_merge_chl_adj"
#' lat1 = 20
#' lat2 = 24
#' lon1 = -170
#' lon2 = -159
#' dt1 = "2016-04-30"
#' dt2 = "2016-04-30"
#' depth1 = 0
#' depth2 = 1500
#' depthprofile(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
#' #
#' }
depth_profile <- function(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2){
  apiKey = get_api_key()
  out = subset("uspDepthProfile", table_name, sel_var, dt1, dt2, lat1, lat2, lon1, lon2,
               depth1, depth2, apiKey)
  return(out)
}



#' Returns a dataframe containing the associated metadata of a variable (such
#' as data source, distributor, references, and etc..). The inputs can be
#' string literals (if only one table, and variable is passed) or a list of
#' string literals.
#' @param table Table name.
#' @param variable Variable name.
#' @return Metadata, in the form of a data frame.
get_metadata_noref <- function(table, variable){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM dbo.udfMetaData_NoRef('%s', '%s')", variable, table)
  return(query(myquery, apiKey))
}


#' Returns a dataframe containing the associated metadata. The inputs can be
#' strings (if only one table, and variable is passed) or a list of
#' string literals.
#' @param tables Vector of table names.
#' @param variables Vector of variable names in the tables.
#' @export
#' @examples
#' \dontrun{
#' table <- c("tblArgoMerge_REP") # table name
#' variable <- c("argo_merge_chl_adj") # variable name
#' get_metadata(table, variable)
#' }
get_metadata <- function(tables, variables){

  metadata = data.frame()
  for(i in 1:length(table)){
    df <- get_metadata_noref(table[i], variable[i])
    datasetID = df$ Dataset_ID[1]
    refs = get_references(datasetID)
    df <- cbind(df,Reference = t(refs$Reference))
    if(i ==1){
      metadata <- df
    } else {
      metadata <- rbind(metadata,df)
    }
  }
  return(metadata)
}


#' Returns a dataframe containing references associated with a data set.
#' @param datasetID ID of dataset.
#' @export
get_references <- function(datasetID){
  apiKey = get_api_key()
  myquery = sprintf("SELECT Reference FROM dbo.udfDatasetReferences(%d)", datasetID)
  return(query(myquery, apiKey))
}



#' Returns the list of columns of a data set.
#' @param tableName Table name.
#' @export
#' @examples
#' \dontrun{
#' columns('tblAMT13_Chisholm')
#' }
columns <- function(tableName){
  apiKey = get_api_key()
  return(query(sprintf("SELECT COLUMN_NAME [Columns] FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'%s'", tableName), apiKey))
}


#' Returns top records of a data set.
#' @param tableName Name of table.
#' @param rows Number of rows to retrieve.
#' @export
#' @examples
#' \dontrun{
#' tbl_head("tblArgoMerge_REP", rows=10)
#' }
head <- function(tableName, rows = 5, apiKey){
  apiKey = get_api_key()
  return(query(sprintf('select TOP(%d) * FROM %s' , rows, tableName), apiKey))
}




#' Returns a single-row dataframe from tblVariables containing info associated with varName.
#' This method is mean to be used internally and will not be exposed at documentations.
#'
#' @param tableName Table name.
#' @param varName Variable name.
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
#' tbl.var <- get_var(table_name, var_name)
#' }
get_var <- function(tableName, varName, apiKey){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM tblVariables WHERE Table_Name='%s' AND Short_Name='%s'", tableName, varName)
  return(query(myquery, apiKey))
}


#' Returns a single-row dataframe from catalog (udfCatalog) containing all of
#' the variable's info at catalog.
#' @param tableName Table name.
#' @param varName Variable name.
#' @return Single-row data frame.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_var_catalog(tableName, varName)
#' }
get_var_catalog <- function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM [dbo].udfCatalog() WHERE Table_Name='%s' AND Variable='%s'" ,
                tableName, varName)
  return(query(myquery, apiKey))
}

#' Returns the long name of a given variable.
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_var_long_name(tableName, varName)
#' }
get_var_long_name<- function(tableName, varName){
  apiKey = get_api_key()
  df = get_var(tableName, varName, apiKey)
  return(df[1, 'Long_Name'])
}


#' Returns the unit for a given variable.
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_unit(tableName, varName)
#' }
get_unit<- function(tableName, varName){
  paste("[", get_var(tableName, varName)[1,,"Unit"], "]")
}

#' Returns a single-row dataframe from catalog (udfCatalog) containing the
#' variable's spatial and temporal resolutions.
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_var_resolution(tableName, varName)
#' }
get_var_resolution<- function(tableName, varName){
  return(get_var_catalog(tableName, varName)[,c('Temporal_Resolution',
                                                        'Spatial_Resolution')])
}

#' Returns a single-row dataframe from catalog (udfCatalog) containing the
#' variable's spatial and temporal coverage.
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_var_coverage(tableName, varName)
#' }
get_var_coverage<- function(tableName, varName){
  mynames = c('Time_Min', 'Time_Max', 'Lat_Min', 'Lat_Max', 'Lon_Min', 'Lon_Max', 'Depth_Min', 'Depth_Max')
  return(get_var_catalog(tableName, varName)[, mynames])
}

#' Returns a single-row dataframe from catalog (udfCatalog) containing the variable's summary statistics.
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' get_var_stat(tableName, varName)
#' }
get_var_stat<- function(tableName, varName){
  mynames = c('Variable_Min', 'Variable_Max', 'Variable_Mean', 'Variable_Std', 'Variable_Count', 'Variable_25th', 'Variable_50th', 'Variable_75th')
  return(get_var_catalog(tableName, varName)[, mynames])
}



#' Returns a boolean indicating whether the variable is a gridded product or has
#' irregular spatial resolution.
#'
#' @param tableName Table name.
#' @param varName Variable name.
#' @export
#' @examples
#' \dontrun{
#' }
is_grid  = function(table_name, var_name){
  apiKey = get_api_key()
  grid = TRUE
  myquery = "SELECT Spatial_Res_ID, RTRIM(LTRIM(Spatial_Resolution)) AS Spatial_Resolution FROM tblVariables "
  myquery = paste(exequery, "JOIN tblSpatial_Resolutions ON [tblVariables].Spatial_Res_ID=[tblSpatial_Resolutions].ID ", sep = "")
  myquery = paste(exequery,sprintf("WHERE Table_Name='%s' AND Short_Name='%s' ",table_name,var_name), sep = "")
  df <- query(exequery,apiKey)
  if (nrow(df)<1) return(NULL)
  if (tolower(df$Spatial_Resolution[1])=='irregular'){
    grid = FALSE
  }
  return(grid)
}



#' Returns True if the table represents a climatological data set.  Currently,
#' the logic is based on the table name.  Ultimately, it should query the DB to
#' determine if it's a climatological data set.
#' @param tableName Table name.
#' @export
#' @examples
#' \dontrun{ }
is_climatology <- function(tableName){
  return(length(grep('_Climatology', tableName))!=0)
}
