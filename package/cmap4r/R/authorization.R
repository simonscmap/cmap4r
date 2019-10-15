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
##TODO: change to initialize
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

#' Gets an API key. To be used internally.
get_api_key <- function(){
  payload = inti_cmap()
  apiKey = payload$api_key
  strsplit(apiKey," ")[[1]][2]
}
