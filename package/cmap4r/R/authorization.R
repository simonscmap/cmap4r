#' Set authorization/API key for accessing the CMAP database.
#'
#' @param reset boolean variable for set authorization key for API access to the database
#' @import magrittr
#' @importFrom keyring key_delete key_set
#' @examples
#' \dontrun{
#' library(cmap4r)
#'
#' # To set the API authorization key
#' set_authorization()
#'
#' # To reset the authorization key
#' set_authorization(reset = TRUE)
#' }
set_authorization <- function(reset = FALSE){
  if (reset){
    keyring::key_delete("cmap_api")
    message("CMAP API authorization key has been deteted.")
  } else {
    message("Enter CMAP API authorization key:")
    keyring::key_set(
      service = "cmap_api"
    )
  }
}



#' Initialize CMAP database access credentials.
#'
#'
#' @param base_url base url for the database. Default: 'https://simonscmap.com'
#' @param route data retrieval route of the database.  Default: "/api/data/sp?"
#' @import magrittr
#' @importFrom keyring key_get
#' @examples
#' \dontrun{
#' library(cmap4r)
#' # Output list with CMAP database access credentials
#' initialize_cmap()
#' }
initialize_cmap <- function(base_url=NULL, route = NULL){

  ## Form some common arguments
  if(is.null(base_url))
    base_url = 'https://simonscmap.com'
  if(is.null(route)){
    # route = '/dataretrieval/sp?'
    route <- "/api/data/sp?"
  }


  ## Try fetching key once
  tem1 <- try(api_key <-
                paste("Api-Key",keyring::key_get("cmap_api"),sep = " "),
              silent = T)

  ## If an error occurs, reset the authorization
  if(!is.null(attr(tem1,"class"))){
    message("API key incorrect/not available.")
    message("Setting up API authorization key.")
    set_authorization(reset=TRUE)
    set_authorization()
    api_key <-  paste("Api-Key",keyring::key_get("cmap_api"),sep = " ")
  }
  return(list(base_url = base_url,route = route, api_key = api_key))
}

