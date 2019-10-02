##' Helper to check error (very basic for now)
check_error <- function(response){
  return(response$status_code==200)
  ## TODO: return the actual status corresponding to the status code, IF not
  ## 200.
}

##' Helper to get response to tibble.
process_response_to_tibble <- function(response){
  a = httr::content(response, "raw")
  a = suppressMessages(readr::read_csv(a))
  return(a)
}

##' Validating the format of sp arguments.
validate_sp_args <- function(table,
                             variable, dt1, dt2, lat1, lat2, lon1, lon2,
                             depth1, depth2){
  ## NOT WRITTEN YET; TODO: mimic what the python does.
  return(TRUE)
}

##' Identical function in python.
cruises <- function(apiKey){
  myquery = "EXEC uspCruises"
  df = query(myquery, apiKey)
  return(df)
}

##' Identical function in python.
get_catalog <- function(apikey){
  myquery = 'EXEC uspCatalog'
  df = query(myquery, apiKey)
}



##' Identical function in python.
cruise_by_name <- function(cruisename, apiKey){

  ## Form EXEC statement.
  myquery = sprintf("EXEC uspCruiseByName '%s' ", cruisename)

  ## Issue query
  df = query(myquery, apiKey)
  return(df)
  ## TODO: check the response for multiple cruises or invalid names.
}

##' Identical function in python.
cruise_bounds <- function(cruisename, apiKey){

  df = cruise_by_name(cruisename, apiKey)
  myquery = sprintf("EXEC uspCruiseBounds %d", df[['ID']][1])
  df = query(myquery, apiKey)
  return(df)
}

##' Identical function in python. (Returns a subset of data according to space-time constraints.)
subset <- function(spName, table, variable, dt1, dt2, lat1, lat2, lon1,
                   lon2, depth1, depth2, apiKey){
        query = sprintf('EXEC %s ?, ?, ?, ?, ?, ?, ?, ?, ?, ?', spName)
        args = list(table, variable, dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2)
        return(stored_proc(query, args, apiKey))
}

##' Near-identical function in python. Executes a strored-procedure and returns
##' the results in form of a dataframe. Similar to query()
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

        assert_that(validate_sp_args(args[1], args[2], args[3], args[4], args[5],
                                     args[6], args[7], args[8], args[9], args[10]))
        request(payload, route= "/api/data/sp?", apiKey)
}

##' Going from (payload -> response -> df). Near-identical function in Python.
request <- function(payload, route, apiKey){

  ## Hard coded
  baseURL = 'https://simonscmap.com'

  ## Form URL query and send + retrieve
  url_safe_query = urlencode_python(payload)
  url = paste0(baseURL,
               route,
               url_safe_query)
  response = httr::GET(url, httr::add_headers(Authorization = paste0("Api-Key ", apiKey)))
  stopifnot(check_error(response))
  return(process_response_to_tibble(response))
}



##' Helper, UNTESTED!! Only works for match so far.
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


