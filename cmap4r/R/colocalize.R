#' Colocalizes the source variable (from source table) with a single target
#' long variable (from target table).  The tolerance parameters set the
#' matching boundaries between the source and target data sets.  Returns a
#' dataframe containing the source variable joined with the target variable.
#'
#' @param spname stored procedure name that executes the matching logic.
#' @param sourceTable table name of the source data set.
#' @param sourceVariable the source variable. The target variables are matched
#'   (colocalized) with this variable.
#' @param targetTables table names of the target data sets to be matched with the source data.
#' @param targetVariables variable names to be matched with the source variable.
#' @param dt1 start date or datetime.
#' @param dt2 end date or datetime.
#' @param lat1 start latitude [degree N].
#' @param lat2 end latitude [degree N].
#' @param lon1 start longitude [degree E].
#' @param lon2 end longitude [degree E].
#' @param depth1 start depth [m].
#' @param depth2 end depth [m].
#' @param timeTolerance float list of temporal tolerance values between pairs
#'   of source and target datasets. The size and order of values in this list
#'   should match those of targetTables. If only a single integer value is
#'   given, that would be applied to all target datasets. This parameter is in
#'   day units except when the target variable represents monthly climatology
#'   data in which case it is in month units. Notice fractional values are not
#'   supported in the current version.
#' @param latTolerance float list of spatial tolerance values in meridional
#'   direction [deg] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.
#' @param lonTolerance float list of spatial tolerance values in zonal
#'   direction [deg] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.
#' @param depthTolerance float list of spatial tolerance values in vertical
#'   direction [m] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.
#'
#' @export
atomic_match <- function(spName, sourceTable, sourceVar, targetTable, targetVar,
                         dt1, dt2, lat1, lat2, lon1, lon2, depth1, depth2,
                         temporalTolerance, latTolerance, lonTolerance,
                         depthTolerance){

  apiKey = get_api_key()

  ## Stringify all arguments and form Query
  args = list(spName, sourceTable, sourceVar, targetTable, targetVar, dt1,
              dt2, lat1, lat2, lon1, lon2, depth1, depth2, temporalTolerance,
              latTolerance, lonTolerance, depthTolerance)

  ## A helper function. islist is a boolean that says whether the
  ## argument should be treated as a list
  custom_toString <- function(x, islist){
    if(length(x)>1 | islist){
      x = sapply(x, function(b)paste0("'", b, "'"))
      x = paste(x, collapse = ",+")
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







#' Match one dataset with another on hand.  Loops through the target data sets
#' and match them with the source data set according to the the accoiated
#' tolerance parameters.  Returns a compiled dataframe of the source and matched
#' target data sets.
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
compile <- function(sourceTable,
                    sourceVar,
                    targetTables,
                    targetVars,
                    dt1,
                    dt2,
                    lat1,
                    lat2,
                    lon1,
                    lon2,
                    depth1,
                    depth2,
                    temporalTolerance,
                    latTolerance,
                    lonTolerance,
                    depthTolerance){

  apiKey = get_api_key()

  ## Helper to add and subtract date
  shift_dt <- function(dt, timeMargin){

    ## Basic checks
    ## assert_that("Date" %in% class(dt))
    ## Commented out until Mohammad fixes the query issue

    ## Shift time
    dt = as.POSIXlt(dt) + (+1) * 60 * 60 * 24 * timeMargin
    dt = format(dt,'%Y-%m-%d %H:%M:%S')
    ## dt3 = as.Date(dt2, format='%Y:%m:%d %H:%M:%S')
    ## browser()
    return(dt)
  }

  ## ## Testing shift_dt
  ## test_that("Time shift is done properly", {
  ##   dt = "2019-03-03"
  ##   dt = as.Date(dt)
  ##   dt_after = shift_dt(dt, 1)
  ##   expect_equal(dt_after, as.Date("2019-03-04"))
  ## })

  ## Helper to check size (and latitude and longitude match).
  size_ok <- function(data1, data2){
    ok1 = all(nrow(data1) == nrow(data2))
    ok2 = all.equal(data1[["lat"]], data2[["lat"]])
    ok3 = all.equal(data1[["lon"]], data2[["lon"]])
    return(all(ok1, ok2, ok3))
  }

  ## If a single tolerance value is provided although multiple variable&table
  ## pairs are queried for, handle it!

  ## 1. Check that all tolerances have the same length
  lens = c(length(temporalTolerance),
           length(latTolerance),
           length(lonTolerance),
           length(depthTolerance))
  stopifnot(length(unique(lens)) == 1)
  len = unique(lens)


  ## 2. If length is 1, and there are more than one variable & table pairs to
  ## manage, do so.
  if(len == 1){
    ntables = length(targetTables)
    if(ntables > 1){
      temporalTolerance = rep(temporalTolerance, ntables)
      latTolerance = rep(latTolerance, ntables)
      lonTolerance = rep(lonTolerance, ntables)
      depthTolerance = rep(depthTolerance, ntables)
    }
  }

  pb = utils::txtProgressBar(min = 0, max = length(targetTables), style = 3) ## This isn't doing much; get rid of this soon.
  spName = "uspMatch"
  datalist = list()
  for(ii in 1:length(targetTables)){

    ## Get data using atomic_match
    ## utils::setTxtProgressBar(pb, ii)
    data = atomic_match(spName,
                        sourceTable,
                        sourceVar,
                        targetTables[ii],
                        targetVars[ii],
                        shift_dt(dt1, -temporalTolerance[ii]),
                        shift_dt(dt2, temporalTolerance[ii]),
                        lat1 - latTolerance[ii],
                        lat2 + latTolerance[ii],
                        lon1 - latTolerance[ii],
                        lon2 + latTolerance[ii],
                        depth1 - depthTolerance[ii],
                        depth2 + depthTolerance[ii],
                        temporalTolerance[ii],
                        latTolerance[ii],
                        lonTolerance[ii],
                        depthTolerance[ii])
    if(length(data) < 1){ warning(sprintf('No matching entry associated with %s.', targetVars[ii]))}
    if(ii > 1){
      if(!size_ok(data, datalist[[ii-1]])){
        stop(sprintf('The matched data frame associated with %s does not have the same size as the first target variable. Please change the tolerance for variable.',
                     targetVars[ii]))
      }
    }
    datalist[[ii]] = data
  }
  df = datalist %>% purrr::reduce(dplyr::full_join, by = c("time", "lat", "lon"))


  ## browser()

  ## datalist %>% length()

  ##   ## If all goes well, insert the colocalized data into the list (later turned
  ##   ## into a data frame)
  ##   if(ii == 1){
  ##     df[[ii]] = data
  ##   } else if(size_ok(data, df)){
  ##     df[[targetVars[ii]]] = cbind(data[[targetVars[ii]]])
  ##     df[[paste0(targetVars[ii],'_std')]] = cbind(data[[paste0(targetVars[ii],'_std')]])
  ##   } else {
  ##     stop(sprintf('The matched data frame associated with %s does not have the same size as the first target variable. Please change the tolerance variable.',
  ##                     targetVars[ii]))
  ##   }
  ## df %>% .[[1]] %>% summary()
  ## df %>% .[[2]] %>% summary()
  ## df %>% .[[3]] %>% summary()

  cat(fill=TRUE)

  ## ## Format into data frame
  ## df = as.data.frame(do.call(rbind, df))
  return(df)
}









#' Colocalize along a cruise track.  Takes a cruise name and colocalizes
#'   the cruise track with the specified variable(s). THIS IS THE MAIN
#'   COLOCALIZATION function for a cruise.
#'
#' @param cruise String of the name of cruise. You can see the list of cruises
#'   using \code{cruises()}.
#' @param targetTables table names of the target data sets to be matched with
#'   the source data.
#' @param targetVars variable names to be matched with the source variable.
#' @param depth1 LOWER limit for depth along the cruise track (lat/lon
#'   trajectory) the user is interested in.
#' @param depth2 UPPER limit for depth along the cruise track (lat/lon
#'   trajectory) the user is interested in.
#' @param temporalTolerance numeric vector of temporal tolerance values between
#'   pairs of source and target datasets. The size and order of values in this
#'   list should match those of targetTables. If only a single integer value is
#'   given, that would be applied to all target datasets. This parameter is in
#'   day units except when the target variable represents monthly climatology
#'   data in which case it is in month units. Notice fractional values are not
#'   supported in the current version.
#' @param latTolerance numeric vector of spatial tolerance values in meridional
#'   direction [deg] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.  A "safe"
#'   value for this parameter can be slightly larger than the half of the target
#'   variable’s spatial resolution.
#' @param lonTolerance numeric vector of spatial tolerance values in zonal
#'   direction [deg] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.  A "safe"
#'   value for this parameter can be slightly larger than the half of the target
#'   variable’s spatial resolution.
#' @param depthTolerance numeric vector of spatial tolerance values in vertical
#'   direction [m] between pairs of source and target data sets. If only one
#'   value is given, that would be applied to all target data sets.
#' @return A dataset colocalized with the cruise.
#' @export
#' @examples 
#' \donttest{ 
#'
#' cruise='diel'
#' targetTables=c('tblSeaFlow')
#' targetVars=c('synecho_abundance')
#' depth1=0
#' depth2=5
#' temporalTolerance=c(0)
#' latTolerance=c(0)
#' lonTolerance=c(0)
#' depthTolerance=c(5)
#' dat = along_track(cruise,
#'                   targetTables,
#'                   targetVars,
#'                   depth1,
#'                   depth2,
#'                   temporalTolerance,
#'                   latTolerance,
#'                   lonTolerance,
#'                   depthTolerance)
#'
#' }
along_track <- function(cruise, targetTables, targetVars, depth1, depth2,
                        temporalTolerance, latTolerance, lonTolerance,
                        depthTolerance){

  ## Basic checks
  stopifnot(length(targetTables) == length(targetVars))

  ## Change target tab

  ## Perform the colocalization
  apiKey = get_api_key()
  df = get_cruise_bounds(cruise)
  dat = compile(sourceTable='tblCruise_Trajectory',
                sourceVar=toString(df[,'ID']),
                targetTables=targetTables,
                targetVars=targetVars,
                dt1=df[['dt1']][1],
                dt2=df[['dt2']][1],
                lat1=as.numeric(df[1,'lat1']),
                lat2=as.numeric(df[1,'lat2']),
                lon1=as.numeric(df[1,'lon1']),
                lon2=as.numeric(df[1,'lon2']),
                depth1=depth1,
                depth2=depth2,
                temporalTolerance=temporalTolerance,
                latTolerance=latTolerance,
                lonTolerance=lonTolerance,
                depthTolerance=depthTolerance)
}


