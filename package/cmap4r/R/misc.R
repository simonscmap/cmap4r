#' Retrieve stored procedure for extracting timeseries data aggregated at
#' weekely, monthly, quaterly, annual or none level.
uspInterval_indicator <- function(interval){
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


#' Helper to check error (very basic for now)
check_error <- function(response){
  return(response$status_code==200)
  ## TODO: return the actual status corresponding to the status code, IF not
  ## 200.
}


#' Helper to get response to tibble.
process_response_to_tibble <- function(response, route){

  if(route == "/api/data/query?"){
    ## Handling CSV responses.
    a = invisible(httr::content(response, "raw"))
    a = suppressMessages(readr::read_csv(a))

  } else if (route == "/dataretrieval/query?"){
    ## Handling JSON responses.
    a = invisible(httr::content(response, "text"))
    a = unlist(strsplit(a, "\n", fixed=TRUE))
    a = lapply(a, jsonlite::fromJSON)
    a = lapply(a, function(aa){as.data.frame(rbind(aa))})
    ## TODO: fix the rest of this. Basically, any subsetting of any element
    ## now becomes a list. Just make it a proper list.
    a = do.call(rbind,a)
    rownames(a) = NULL

    ## ## This works when getting cruise bounds
    ## a = invisible(httr::content(response, "parsed"))
    ## a[which(sapply(a, is.null))]=NA
    ## a = tibble::as_tibble(a)
  } else {
    stop("Check your |route| variable!")
  }
  invisible(return(a))
}


#' Validating the format of sp arguments.
#' @param table (string) table name (each data set is stored in one or more
#'   than one table).
#' @param variable (string) variable short name which directly corresponds to a
#'   field name in the table.
#' @param dt1 (string of format "YYYY-MM-DDTHH:MM:SS" start date (or datetime
#'   simply as YYYY-MM-DD). Caution!! This function doesn't check for the exact
#'   format of the input, and only checks for whether it is a string. Same for
#'   \code{dt2}.
#' @param dt2 (string of format "YYYY-MM-DDTHH:MM:SS" end date (or datetime,
#'   simply as YYYY-MM_DD).
#' @param lat1 (Numeric) start latitude [degree N].
#' @param lat2 (Numeric) end latitude [degree N].
#' @param lon1 (Numeric) start longitude [degree E].
#' @param lon2 (Numeric) end longitude [degree E].
#' @param depth1 (Numeric) start depth [m].
#' @param depth2 (Numeric) end depth [m].
validate_sp_args <- function(table,
                             variable, dt1, dt2, lat1, lat2, lon1, lon2,
                             depth1, depth2){
  assertthat::assert_that(assertthat::is.string(table))
  assertthat::assert_that(assertthat::is.string(variable))
  assertthat::assert_that(assertthat::is.string(dt1))
  assertthat::assert_that(assertthat::is.string(dt2))
  assertthat::assert_that(is.numeric(lat1))
  assertthat::assert_that(is.numeric(lat2))
  assertthat::assert_that(is.numeric(lon1))
  assertthat::assert_that(is.numeric(lon2))
  assertthat::assert_that(is.numeric(depth1))
  assertthat::assert_that(is.numeric(depth2))
}


#' Takes a custom query, issues a request to the API, and returns the results
#' in form of a dataframe.
#' @param myquery An "EXEC ..." string.
#' @param apiKey The API Key.
query <- function(myquery, apiKey){
  ## Form query
  payload = list(query = myquery)
  # route = '/dataretrieval/query?'     # JSON format, deprecated
  route = '/api/data/query?'     # CSV format
  request(payload, route, apiKey)
}




#' Returns a subset of data according to space-time constraints. (Identical function in python.)
subset <- function(spName, table, variable, dt1, dt2, lat1, lat2, lon1,
                   lon2, depth1, depth2, apiKey){
        query = sprintf('EXEC %s ?, ?, ?, ?, ?, ?, ?, ?, ?, ?', spName)
        args = list(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
        return(stored_proc(query, args, apiKey))
}

#' Executes a stored procedure. (Near-identical function in python.) Executes a
#' stored-procedure and returns the results in form of a dataframe. Similar to
#' \code{query()}.
stored_proc <- function(query, args, apiKey){
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

        assert_that(validate_sp_args(args[[1]], args[[2]], args[[3]],
                                     args[[4]], args[[5]],
                                     args[[6]], args[[7]],
                                     args[[8]], args[[9]], args[[10]]))
        request(payload, route= "/api/data/query?", apiKey)}

#' Send API request. Going from (payload -> response -> df). Near-identical
#' function in Python.
request <- function(payload, route, apiKey){

  ## Hard coded
  baseURL = 'https://simonscmap.com'

  ## Form URL query and send + retrieve
  url_safe_query = urlencode_python(payload)
  url = paste0(baseURL,
               route,
               url_safe_query)
  # print(url)
  header = httr::add_headers(Authorization = paste0("Api-Key ", apiKey))
  response = httr::GET(url, header)

  ## TODO: Consider using RETRY("GET", "http://invalidhostname/"), since the
  ## initial request sometimes fails.

  if(!check_error(response))browser() ## temporary
  stopifnot(check_error(response))
  return(process_response_to_tibble(response,
                                    route)) ## The second argument is until
                                            ## Mohammad fixes this issue.
}

#' Helper, UNTESTED!! Only works for match so far.
urlencode_python <- function(payload){
  ## assert_that(length(payload)==1)
  all_items = Map(function(item, itemname){
    payload = paste0(itemname, "=", item)
    payload = utils::URLencode(payload, reserved=TRUE)
    payload = gsub("%3D", "=", payload)
  }, payload, names(payload))
  payload = paste(all_items, collapse="&")
  ## One step too far; the spaces " " are converted to "+"
  payload = gsub("%20", "+", payload)
  return(payload)
}




#' Helper to check variable name or table name with the catalog.
check_with_catalog <- function(thing, type=c("varName", "tableName"), apiKey){
  type = match.arg(type)
  catalog = get_catalog()
  if(type=="tableName"){
    return(thing %in% unlist(unique(catalog[,"Table_Name"])))
  }
  if(type=="varName"){
    return(thing %in% unlist(unique(catalog[,"Variable"])))
  }
}
