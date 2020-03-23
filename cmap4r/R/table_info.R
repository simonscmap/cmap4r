#########################################################################
### All functions here are provides table informations functions      ###
#########################################################################

#' Returns a boolean outcome checking if a field (varName) exists in a table (data set).
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @export
#' @return boolean outcome
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' var_exist <- has_field(tableName, varName)
#' var_exist
#' #
#' }
has_field = function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT COL_LENGTH('%s', '%s') AS RESULT ",
                    tableName, varName)
  return(length(query(myquery, apiKey)[1, 'RESULT']) > 0)
}




#' Returns top  n records  from a table on the Simons CMAP.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param nrows number of rows to retrieve.
#' @return return table  as dataframe
#' @export
#' @examples
#' \donttest{
#' 
#' ## Input: Table name;
#' tableName <- "tblArgoMerge_REP" # table name
#' #
#' ## Top n rows:
#' tbl.subset <- get_head(tableName, nrows=10)
#' tbl.subset
#' }
get_head <- function(tableName, nrows = 5){
  apiKey = get_api_key()
  return(query(sprintf('select TOP(%d) * FROM %s' , nrows, tableName), apiKey))
}




#' Returns the list of column variables in a table.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @return column variables name of a table as dataframe
#' @export
#' @examples
#' \donttest{
#' #
#' ## Input: Table name;
#' tableName <- "tblAMT13_Chisholm" # table name
#' #
#' ## Subset selection:
#' tbl.columns <- get_columns(tableName)
#' tbl.columns
#' #
#' }
get_columns <- function(tableName){
  apiKey = get_api_key()
  return(query(sprintf("SELECT COLUMN_NAME [Columns] FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'%s'", tableName), apiKey))
}







#' Returns a catalog of the Simons CMAP database.
#'
#'
#' @return Simons CMAP catalog as dataframe object
#' @export
#' @examples
#' \donttest{
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
  return(df)
}










#' Returns a single-row dataframe containing the attribute of a variable associated with a table on the Simons CMAP database.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @export
#' @return attributes of variable  as dataframe 
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' tbl.var <- get_var(tableName, varName)
#' tbl.var
#' }
get_var <- function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM tblVariables WHERE Table_Name='%s' AND Short_Name='%s'", tableName, varName)
  return(query(myquery, apiKey))
}



#' Return a single-row dataframe about a table variable from the catalog of the Simons CMAP database. 
#' 
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return attributes of variable on the catalog as dataframe.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' tbl.catlog.var <- get_var_catalog(tableName, varName)
#' tbl.catlog.var
#' #
#' }
get_var_catalog <- function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM [dbo].udfCatalog() WHERE Table_Name='%s' AND Variable='%s'" ,tableName, varName)
  return(query(myquery, apiKey))
}





#' Returns the long name of a given variable.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return long name of a table variable
#' @export
#' @examples
#' \donttest{
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varLonName <- get_var_long_name(tableName, varName)
#' varLonName
#' #
#' }
get_var_long_name <- function(tableName, varName){
  df = get_var(tableName, varName)
  return(df[1, 'Long_Name'])
}



#' Returns the unit of a table variable on the Simons CMAP database.
#'
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return measuring unit of a table variable as dataframe
#' @export
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' unitName <- get_var_unit(tableName, varName)
#' unitName
#' #
#' }
get_var_unit = function(tableName, varName){
  return(get_var_catalog(tableName, varName)[1,'Unit'])
}



#' Returns a single-row dataframe from the database  catalog  containing the
#' variable's spatial and temporal resolutions.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return resolution of a table variable as dataframe
#' @export
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varResolution <- get_var_resolution(tableName, varName)
#' varResolution
#' #
#' }
get_var_resolution = function(tableName, varName){
  return(get_var_catalog(tableName, varName)[,c('Temporal_Resolution',
                                                'Spatial_Resolution')])
}



#' Returns a single-row dataframe from the database  catalog containing the
#' variable's spatial and temporal coverage.
#'
#'
#' @param tableName table name.
#' @param varName variable name.
#' @export
#' @return spatio-temporal range information of a table variable as dataframe
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varCoverage <- get_var_coverage(tableName, varName)
#' varCoverage
#' #
#' }
get_var_coverage <- function(tableName, varName){
  mynames = c('Time_Min', 'Time_Max', 'Lat_Min', 'Lat_Max', 'Lon_Min', 'Lon_Max', 'Depth_Min', 'Depth_Max')
  return(get_var_catalog(tableName, varName)[, mynames])
}



#' Returns a single-row dataframe from the database catalog containing the variable's summary statistics.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return numerical attribute of a table variable as dataframe
#' @export
#' @examples
#' \donttest{
#' #
#' ## Input: Table name; variable name
#' tableName <- "tblArgoMerge_REP" # table name
#' varName <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' varStats <- get_var_stat(tableName, varName)
#' varStats
#' #
#' }
get_var_stat <- function(tableName, varName){
  mynames = c('Variable_Min', 'Variable_Max', 'Variable_Mean', 'Variable_Std', 'Variable_Count', 'Variable_25th', 'Variable_50th', 'Variable_75th')
  return(get_var_catalog(tableName, varName)[, mynames])
}




#' Returns a boolean indicating whether the variable is a gridded product or has irregular spatial resolution.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param varName specify short name of a variable in the table. 
#' @return boolean
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Input:
#' table <- c("tblArgoMerge_REP") # table name
#' variable <- c("argo_merge_chl_adj") # variable name
#' #
#' is_grid(table, variable)
#'
#' #
#' }
is_grid  = function(tableName, varName){
  apiKey = get_api_key()
  grid = TRUE
  myquery = "SELECT Spatial_Res_ID, RTRIM(LTRIM(Spatial_Resolution)) AS Spatial_Resolution FROM tblVariables "
  myquery = paste(myquery, "JOIN tblSpatial_Resolutions ON [tblVariables].Spatial_Res_ID=[tblSpatial_Resolutions].ID ", sep = "")
  myquery = paste(myquery,sprintf("WHERE Table_Name='%s' AND Short_Name='%s' ",tableName,varName), sep = "")
  df <- query(myquery,apiKey)
  if (nrow(df)<1) return(NULL)
  if (tolower(df$Spatial_Resolution[1])=='irregular'){
    grid = FALSE
  }
  return(grid)
}




#' Returns True if the table represents a climatological data set.  
#' Currently, the logic is based on the table name.  
#' Ultimately, it should query the DB to determine if it's a climatological data set.
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @export
#' @return boolean
#' @examples
#' \donttest{
#' #
#' ## Input:
#' table <- "tblDarwin_Plankton_Climatology" # table name
#' #
#' is_climatology(table)
#'
#' #
#' }
is_climatology <- function(tableName){
  return(length(grep('_Climatology', tableName)) != 0)
}






#' Returns a dataframe containing the associated metadata. The inputs can be strings (if only one table, and variable is passed) or a list of string literals.
#'
#' @param tables vector of table names from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
#' @param variables specify short name of the corresponding table variables.
#' @return metadata associated with all the table variables as dataframe.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Input:
#' tables <- c('tblsst_AVHRR_OI_NRT', 'tblArgoMerge_REP') # table name
#' variables <- c('sst', 'argo_merge_salinity_adj') # variable name
#'
#' metadata <- get_metadata(tables, variables)
#' metadata
#' }
get_metadata <- function(tables, variables){
  append_df = function(df, a){
    out <- data.frame(matrix(NaN, a, ncol(df)))
    names(out) <- names(df)
    out[1,] <- data.frame(df)[1,]
    out
  }
  metadata = data.frame()
  for(i in 1:length(tables)){
    df <- get_metadata_noref(tables[i], variables[i])
    datasetID = df$Dataset_ID[1]
    refs = get_references(datasetID)
    df <- append_df(df,length(refs$Reference))
    df$Reference = refs$Reference
    if(i == 1){
      metadata <- df
    } else {
      metadata <- rbind(metadata,df)
    }
  }
  return(metadata)
}



# romCatalog boolean variable to obtain number of observation in a table from the simons CMAP catalog 



#' Retrieve the number of observations in the subset of a table from the Simons CMAP databse using the space-time range inputs (dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2).
#'
#' @param tableName table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database. 
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
#' # Range variable [lat,lon,time]
#' lat1 = 10; lat2 = 70
#' lon1 = -180; lon2 = -80
#' dt1 = "2016-04-30"; dt2 = "2016-04-30"
#' #
#' ## Subset selection:
#' ncount <- get_count(tableName, lat1, lat2, lon1, lon2, dt1, dt2)
#' ncount
#' #
#' }
get_count = function(tableName, lat1 = NULL, lat2 = NULL, 
                     lon1 = NULL, lon2 = NULL, 
                     dt1 = NULL, dt2 = NULL,
                     depth1 = NULL, depth2 = NULL){
  range_var <- list()
  range_var$time <- c(dt1, dt2)
  range_var$lat <- c(lat1, lat2)
  range_var$lon <- c(lon1, lon2)
  range_var$depth <- c(depth1, depth2)
  
  # in case if only table names are provided 
  if (length(range_var) == 0) {
    ab <- get_catalog()
    index <- tolower(ab$Table_Name) == tolower(tableName)
    range_var$time <- c(ab$Time_Min[index], ab$Time_Max[index])
    range_var$lat <- c(ab$Lat_Min[index], ab$Lat_Max[index])
    range_var$lon <- c(ab$Lon_Min[index], ab$Lon_Max[index])
    range_var$depth <- c(ab$Depth_Min[index], ab$Depth_Max[index])
    range_var <- lapply(range_var, function(x){
      if (any(is.na(x))) x = NULL
      x
    })
    range_var[sapply(range_var, is.null)] <- NULL
  }
  # if (!fromCatalog) {
  tout <- NULL
  for (tmp in names(range_var)) {
    if (length(range_var[[tmp]]) == 1)
      range_var[[tmp]] <- rep(range_var[[tmp]],2)
    if (tmp == 'time')
      range_var[[tmp]] <- paste("\n",range_var[[tmp]],"\n", sep = '')
    tout <- c( tout, paste(tmp, 'between',range_var[[tmp]][1],'and',range_var[[tmp]][2]))
  }
  filt_query <- paste0(tout, collapse = ' and ')
  sub_query <- sprintf("select count(*) from %s where",tableName )
  full_query <- paste(sub_query, filt_query)
  full_query <- gsub('\n',"'",full_query)
  tmp <- exec_manualquery(full_query)
  ncount <- as.numeric(names(tmp))
  # }
  # else {
  #   ab <- get_catalog()
  #   index <- tolower(ab$Table_Name) == tolower(tableName)
  #   ncount <- max(ab$Variable_Count[index],na.rm = T)
  # }
  return(ncount)
}




