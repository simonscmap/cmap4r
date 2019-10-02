##' (incomplete) Takes the response.
check_response <- function(response){

  ## Throw error if /anything/ is off.
  if(erroneous(response)) stop("")

}


##' (incomplete) Transforms a sound response to a data frame. Assumes response is sound.
response_to_dataframe <- function(response){

  ## Optional check
  stopifnot(check_response)

  ## Rest of code goes here
}

