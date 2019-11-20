# ---------------- cruise related function  -------------------

#' Returns a dataframe containing a list of all of the hosted cruise names.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Variable attribute:
#' cruises_info <- get_cruises()
#' #
#' }
get_cruises <- function(){
  apiKey = get_api_key()
  myquery = "EXEC uspCruises"
  df = query(myquery, apiKey)
  return(df)
}







#' Returns a dataframe containing all registered variables (at Simons CMAP) during a cruise.
#'
#' @param cruiseName name of cruise.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Variable attribute:
#' cruiseVar <- get_cruise_variables('SCOPE_Falkor1')
#' #
#' }
get_cruise_variables <- function(cruiseName){
  df = cruise_by_name(cruiseName)
  exequery <- sprintf('SELECT * FROM dbo.udfCruiseVariables(%d) ',
                      df$ID[1])
  apiKey = get_api_key()
  df <- query(exequery,apiKey)
  return(df)
}







#' Returns a dataframe containing cruise info using cruise name.
#'
#' @param cruisename name of cruise.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Variable attribute:
#' cruiseByName <- get_cruise_by_name('KOK1606')
#' #
#' }
get_cruise_by_name <- function(cruisename){
  
  ## Form EXEC statement.
  myquery = sprintf("EXEC uspCruiseByName '%s' ", cruisename)
  
  ## Issue query
  apiKey = get_api_key()
  df = query(myquery, apiKey)
  
  ## Check results
  if(nrow(df)<1){
    stop(sprintf('Invalid cruise name: %s' , cruisename))
  }
  if(nrow(df)>1){
    df$Keywords <- NULL
    message(df)
    message('More than one cruise found. Please provide a more specific cruise name: ')
  }
  return(df)
}





#' Returns a dataframe containing cruise boundaries in space and time.
#'
#' @param cruisename Name of cruise.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Variable attribute:
#' cruiseBounds <- get_cruise_bounds('KOK1606')
#' #
#' }
get_cruise_bounds <- function(cruisename){
  apiKey = get_api_key()
  df = get_cruise_by_name(cruisename)
  myquery = sprintf("EXEC uspCruiseBounds %d", unlist(df[['ID']][1]))
  suppressMessages({
    df = query(myquery, apiKey)
  })
  return(df)
}






#' Returns a dataframe containing the cruise trajectory (Identical function in python)
#'
#' @param cruisename name of cruise.
#' @export
#' @examples
#' \donttest{
#' 
#' #
#' ## Variable attribute:
#' cruiseTrajectory  <- get_cruise_trajectory('KM1513')
#' #
#' }
get_cruise_trajectory <- function(cruisename){
  apiKey = get_api_key()
  df = cruise_by_name(cruisename)
  myquery = sprintf("EXEC uspCruiseTrajectory %d", unlist(df[['ID']][1]))
  df = query(myquery, apiKey)
  
  ## ## Temporary, until Mohammad fixes the query issue
  ## df[["time"]] = sapply(df[["time"]], substr, 1, 19)
  
  return(df)
}








