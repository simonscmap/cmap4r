#' Set/save the user API key for accessing the CMAP database.
#'
#' @param reset boolean variable for set authorization key for API access to the database
#' @param cmap_key api key of the cmap database
#' @import magrittr
#' @export
#' @importFrom keyring key_delete key_set key_set_with_value
#' @examples
#' \dontrun{
#'
#' # To set the API authorization key
#' set_authorization()
#'
#' # To reset the authorization key
#' set_authorization(reset = TRUE)
#' }
set_authorization <- function(reset = FALSE, cmap_key = NULL){
  if (reset){
    keyring::key_delete("cmap_api")
    ## message("CMAP API authorization key has been detected.")
  } else {
    if (is.null(cmap_key)) stop("CMAP authorization key not provided.")
    # message("Enter CMAP API authorization key:")
    keyring::key_set_with_value(
      service = "cmap_api", password = cmap_key
    )
  }
}



#' Initialize the CMAP database access credentials.
#'
#'
#' @param base_url base url for the database. Default: 'https://simonscmap.com'
#' @param route data retrieval route of the database.  Default: "/api/data/sp?"
#' @param cmap_key api key of the cmap database
#' @import magrittr
#' @importFrom keyring key_get
#' @export
#' @examples
#' \dontrun{
#' # Output list with CMAP database access credentials
#' initialize_cmap()
#' }
initialize_cmap <- function(base_url=NULL, route = NULL, cmap_key = NULL){

  ## Form some common arguments
  if(is.null(base_url))
    base_url = 'https://simonscmap.com'
  if(is.null(route)){
    # route = '/dataretrieval/sp?'
    route <- "/api/data/sp?"
  }

  if (is.null(cmap_key)){
    ## Try fetching key once
    tem1 <- suppressWarnings(try(api_key <-
                  paste("Api-Key",keyring::key_get("cmap_api"),sep = " "),
                silent = T))
    if (!is.null(attr(tem1,"class"))) {
      message("API key incorrect/not available.")
      stop("Set up API authorization key using set_authorization() function.")
      }
  } else {

    set_authorization(cmap_key = cmap_key)
    api_key <-  paste("Api-Key",keyring::key_get("cmap_api"),sep = " ")
  }
  
  ## If an error occurs, reset the authorization
  # if(!is.null(attr(tem1,"class"))){
  #   message("API key incorrect/not available.")
  #   message("Setting up API authorization key.")
  #   set_authorization(reset = TRUE)
  # }
  return(list(base_url = base_url,route = route, api_key = api_key))
}
