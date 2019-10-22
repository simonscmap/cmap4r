#########################################################################
### All functions here are provides table informations functions      ###
#########################################################################

#' Returns a boolean confirming whether a field (varName) exists in a table (data set).
#'
#' @param tableName table name in the database
#' @param varName variable name to check
#' @export
#' @examples
#' \donttest{
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
  myquery = sprintf("SELECT COL_LENGTH('%s', '%s') AS RESULT ",
                    tableName, varName)
  return(length(query(myquery, apiKey)[1, 'RESULT'])>0)
}




#' Returns top  n records  from a table on CMAP
#'
#' @param tableName name of table.
#' @param nrows number of rows to retrieve.
#' @return subset of a table as data frame
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
#'
#' ## Input: Table name;
#' table_name <- "tblArgoMerge_REP" # table name
#' #
#' ## Top n rows:
#' tbl.subset <- get_head(table_name, nrows=10)
#' tbl.subset
#' }
get_head <- function(tableName, nrows = 5){
  apiKey = get_api_key()
  return(query(sprintf('select TOP(%d) * FROM %s' , nrows, tableName), apiKey))
}




#' Returns the list of columns of a data set.
#'
#' @param tableName table name.
#' @return columns in a table
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' ## Input: Table name;
#' table_name <- "tblAMT13_Chisholm" # table name
#' #
#' ## Subset selection:
#' tbl.columns <- get_columns(table_name)
#' tbl.columns
#' #
#' }
get_columns <- function(tableName){
  apiKey = get_api_key()
  return(query(sprintf("SELECT COLUMN_NAME [Columns] FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'%s'", tableName), apiKey))
}






#' Returns catalog of the CMAP data base.
#'
#'
#' @return table of catalog
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
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










#' Returns a single-row dataframe from tblVariables containing info associated with varName.
#'
#' This method is mean to be used internally and will not be exposed at documentations.
#'
#' @param tableName table name.
#' @param varName variable name.
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' ## Input: Table name; variable name
#' table_name <- "tblArgoMerge_REP" # table name
#' var_name <- "argo_merge_chl_adj" # variable name
#' #
#' ## Variable attribute:
#' tbl.var <- get_var(table_name, var_name)
#' tbl.var
#' }
get_var <- function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM tblVariables WHERE Table_Name='%s' AND Short_Name='%s'", tableName, varName)
  return(query(myquery, apiKey))
}



#' Returns a single-row dataframe from catalog (udfCatalog) containing all of
#' the variable's info at catalog.
#' @param tableName table name.
#' @param varName variable name.
#' @return Single-row data frame.
#' @export
#' @examples
#' \donttest{
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
get_var_catalog <- function(tableName, varName){
  apiKey = get_api_key()
  myquery = sprintf("SELECT * FROM [dbo].udfCatalog() WHERE Table_Name='%s' AND Variable='%s'" ,tableName, varName)
  return(query(myquery, apiKey))
}





#' Returns the long name of a given variable.
#'
#' @param tableName table name.
#' @param varName variable name.
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



#' Returns unit of the variable.
#'
#'
#' @param tableName table name in the database
#' @param varName name of variable
#' @return subset of a table as data frame
#' @export
#' @examples
#' \donttest{
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
get_var_unit = function(tableName, varName){
  return(get_var_catalog(tableName, varName)[1,'Unit'])
}



#' Returns a single-row dataframe from catalog (udfCatalog) containing the
#' variable's spatial and temporal resolutions.
#'
#' @param tableName table name.
#' @param varName variable name.
#' @export
#' @examples
#' \donttest{
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
get_var_resolution = function(tableName, varName){
  return(get_var_catalog(tableName, varName)[,c('Temporal_Resolution',
                                                'Spatial_Resolution')])
}



#' Returns a single-row dataframe from catalog (udfCatalog) containing the
#' variable's spatial and temporal coverage.
#'
#'
#' @param tableName table name.
#' @param varName variable name.
#' @export
#' @examples
#' \donttest{
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
get_var_coverage<- function(tableName, varName){
  mynames = c('Time_Min', 'Time_Max', 'Lat_Min', 'Lat_Max', 'Lon_Min', 'Lon_Max', 'Depth_Min', 'Depth_Max')
  return(get_var_catalog(tableName, varName)[, mynames])
}



#' Returns a single-row dataframe from catalog (udfCatalog) containing the variable's summary statistics.
#'
#' @param tableName table name.
#' @param varName variable name.
#' @export
#' @examples
#' \donttest{
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
get_var_stat<- function(tableName, varName){
  mynames = c('Variable_Min', 'Variable_Max', 'Variable_Mean', 'Variable_Std', 'Variable_Count', 'Variable_25th', 'Variable_50th', 'Variable_75th')
  return(get_var_catalog(tableName, varName)[, mynames])
}




#' Returns a boolean indicating whether the variable is a gridded product or has irregular spatial resolution.
#'
#' @param table_name table name.
#' @param var_name variable name.
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
#' #
#' ## Input:
#' table <- c("tblArgoMerge_REP") # table name
#' variable <- c("argo_merge_chl_adj") # variable name
#' #
#' is_grid(table, variable)
#'
#' #
#' }
is_grid  = function(table_name, var_name){
  apiKey = get_api_key()
  grid = TRUE
  myquery = "SELECT Spatial_Res_ID, RTRIM(LTRIM(Spatial_Resolution)) AS Spatial_Resolution FROM tblVariables "
  myquery = paste(myquery, "JOIN tblSpatial_Resolutions ON [tblVariables].Spatial_Res_ID=[tblSpatial_Resolutions].ID ", sep = "")
  myquery = paste(myquery,sprintf("WHERE Table_Name='%s' AND Short_Name='%s' ",table_name,var_name), sep = "")
  df <- query(myquery,apiKey)
  if (nrow(df)<1) return(NULL)
  if (tolower(df$Spatial_Resolution[1])=='irregular'){
    grid = FALSE
  }
  return(grid)
}




#' Returns True if the table represents a climatological data set.  Currently, the logic is based on the table name.  Ultimately, it should query the DB to determine if it's a climatological data set.
#'
#' @param tableName table name.
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
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
#' @param tables vector of table names.
#' @param variables vector of variable names in the tables.
#' @export
#' @examples
#' \donttest{
#' library(cmap4r)
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













