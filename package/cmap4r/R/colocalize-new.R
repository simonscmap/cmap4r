

##' Colocalizes the source variable (from source table) with a single target
##' variable (from target table).  The tolerance parameters set the matching
##' boundaries between the source and target data sets.  Returns a dataframe
##' containing the source variable joined with the target variable.
##' @param spname stored procedure name that executes the matching logic.
##' @param sourceTable table name of the source data set.
##' @param sourceVariable the source variable. The target variables are matched
##'   (colocalized) with this variable.
##' @param targetTables table names of the target data sets to be matched with
##'   the source data.
##' @param targetVariables variable names to be matched with the source
##'   variable.
##' @param dt1 start date or datetime.
##' @param dt2 end date or datetime.
##' @param lat1 start latitude [degree N].
##' @param lat2 end latitude [degree N].
##' @param lon1 start longitude [degree E].
##' @param lon2 end longitude [degree E].
##' @param depth1 start depth [m].
##' @param depth2 end depth [m].
##' @param timeTolerance float list of temporal tolerance values between pairs
##'   of source and target datasets. The size and order of values in this list
##'   should match those of targetTables. If only a single integer value is
##'   given, that would be applied to all target datasets. This parameter is in
##'   day units except when the target variable represents monthly climatology
##'   data in which case it is in month units. Notice fractional values are not
##'   supported in the current version.
##' @param latTolerance float list of spatial tolerance values in meridional
##'   direction [deg] between pairs of source and target data sets. If only one
##'   value is given, that would be applied to all target data sets.
##' @param lonTolerance float list of spatial tolerance values in zonal
##'   direction [deg] between pairs of source and target data sets. If only one
##'   value is given, that would be applied to all target data sets.
##' @param depthTolerance float list of spatial tolerance values in vertical
##'   direction [m] between pairs of source and target data sets. If only one
##'   value is given, that would be applied to all target data sets.
atomic_match <- function(spName, sourceTable, sourceVar, targetTable, targetVar,
                         dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2,
                         temporalTolerance, latTolerance, lonTolerance,
                         depthTolerance, apiKey){

  ## Stringify all arguments and form Query
  args = list(spName, sourceTable, sourceVar, targetTable, targetVar, dt1,
              dt2, lat1, lat2, lon1, lon2, depth1, depth2, temporalTolerance,
              latTolerance, lonTolerance, depthTolerance)

  ## A helper function. islist is a boolean that says whether the
  ## argument should be treated as a list
  custom_toString <- function(x, islist){
    if(length(x)>1 | islist){
      x = sapply(x, function(b)paste0("'", b, "'"))
      x = paste(x, collapse=",+")
      x = paste0('[', x, ']')
    } else {
      x = toString(x)
    }
    return(x)
  }


  ## Define the ones that are list objects
  type_args = rep(FALSE, 17)
  ## type_args[c(3,4,14,15,16,17)] = TRUE ## Commented out for now, but most
  ## likely needed!


  ## Form EXEC statement (new);
  args = Map(custom_toString, args, type_args)
  myquery = sprintf("EXEC %s '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s'",
                    args[1], args[2], args[3], args[4], args[5], args[6],
                    args[7], args[8], args[9], args[10], args[11], args[12],
                    args[13], args[14], args[15], args[16], args[17])

  ## Issue query
  return(query(myquery, apiKey)) ## query is API().query in Python
}

##' Takes a custom query, issues a request to the API, and returns the results
##' in form of a dataframe.
##' @param myquery An "EXEC ..." string.
##' @param apiKey The API Key.
##' @param ... Rest of arguments to \code{query_url()}.
query <- function(myquery, apiKey){

  ## Form query
  payload = list(query = myquery)
  request(payload, route="/api/data/query?", apiKey)
}
