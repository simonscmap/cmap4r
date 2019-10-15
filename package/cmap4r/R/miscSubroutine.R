#'  Convert range object to list object: It will be used latter for creating query.
#'
#' @param range_var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
range2list <- function(range_var) {
  range_varx <- data.frame(range_var)
  if (any(names(range_var) == "time")) {
    range_varx$time <- as.character(range_varx$time)
  }
  if (any(names(range_var) == "date")) {
    range_varx$date <- as.character(range_varx$date)
  }
  varname <- names(range_var)
  out <- vector("list", 2)
  out[[2]] <- out[[1]] <- vector("list", length = length(varname))
  names(out[[1]]) <- names(out[[2]]) <- varname
  for (i in 1:2) {
    for (j in 1:length(varname)) {
      out[[i]][[j]] <- range_varx[i, j]
    }
  }
  return(out)
}





#'  Create filter query with equality condition
#'
#' @param range_var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom purrr map imap reduce
#' @importFrom rlang sym expr
getEqualQuery <- function(range_var) {
  out <- range2list(range_var)[1]
  ab <- vector("list", 1)
  ab[1] <- out[1] %>%
    # iterate through each row
    purrr::map(~ {
      # iterate through each pair ie: gear == 4
      purrr::imap(.x, ~ rlang::expr(!!rlang::sym(.y) == !!.x)) %>%
        # creates one call in the row by
        # "collapsing" all pairs, separating them with &
        purrr::reduce(function(x, y) rlang::expr((!!x & !!y)))
    })
  return(ab[[1]])
}



#'  Create filter query using specified range variable
#'
#' @param range_var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom purrr map imap reduce
#' @importFrom rlang sym expr
getFilterQuery <- function(range_var) {
  out <- range2list(range_var)

  ab <- vector("list", 2)
  ab[1] <- out[1] %>%
    # iterate through each row
    purrr::map(~ {
      # iterate through each pair ie: gear == 4
      purrr::imap(.x, ~ rlang::expr(!!rlang::sym(.y) >= !!.x)) %>%
        # creates one call in the row by
        # "collapsing" all pairs, separating them with &
        purrr::reduce(function(x, y) rlang::expr((!!x & !!y)))
    })

  ab[2] <- out[2] %>%
    # iterate through each row
    purrr::map(~ {
      # iterate through each pair ie: gear == 4
      purrr::imap(.x, ~ rlang::expr(!!rlang::sym(.y) <= !!.x)) %>%
        # creates one call in the row by
        # "collapsing" all pairs, separating them with &
        purrr::reduce(function(x, y) rlang::expr((!!x & !!y)))
    })

  xb <- ab %>% purrr::reduce(function(x, y) rlang::expr((!!x & !!y)))
  return(xb)
}

# filter option for query creation
# ab <- imap(range.var, ~expr(between(!!sym(.y), !!.x[1], !!.x[2]))) %>%
#   reduce(function(x, y) expr((!!x & !!y)))


##' Takes in date (can be a string) and returns TRUE of only date exists, and no
##' time information.
##' @param mydate Date
##' @return \code{TRUE} if the time-of-day information does not exist.
only_has_date <- function(mydate){
  mydate2 = lubridate::as_datetime(mydate, tz = "UTC")
  mydate2 = hms::as.hms(mydate2, tz="UTC")
  return(toString(mydate2) == "00:00:00")
}


##' Subsets a subtble from an OFFLINE table (3 columns called "lat" "lon" and
##' "time").
##' @param tbl 3 column matrix.
##' @param range_var Data frame containing information about the "rectangle" of
##'   interest.
##' @return 3 column matrix with only a subset of the table.
subset_from_tbl <- function(tbl, range_var) {

  ## Check if time-of-day information exists. If not, discard time altogether
  ## from ranges prior to comparison.
  one_random_time = sample(tbl$time, 1)
  if(only_has_date(one_random_time)){
    range_var$time = lubridate::as_date(range_var$time)
  }

  ## Handle Lat/lon
  lat_ind <- which(tbl[, "lat"] >= range_var$lat[1] &
    tbl[, "lat"] <= range_var$lat[2])
  lon_ind <- which(tbl[, "lon"] >= range_var$lon[1] &
    tbl[, "lon"] <= range_var$lon[2])

  ## Handle time
  times <- tbl[["time"]]
  times <- as.POSIXct(times)
  dt_range <- as.POSIXct(sub("T", " ", range_var$time))
  time_ind <- which(times >= dt_range[1] &
                    times <= dt_range[2])

  ## Intersect and return
  joint_ind <- Reduce(intersect, list(lat_ind, lon_ind, time_ind))
  return(tbl[joint_ind, ])
}


##' Helper for matchSource() that takes in many lat/lon/date triplets and the
##' margins of error for query, and returns a "range" variable for all triplets
##' (with margin).
##' @param source_table a matrix with 3 columns which each are: lat (center
##'   latitude), lon (center longitude) dt (center date).
##' @param latMargin margin for latitude, around the center.
##' @param lonMargin margin for longitude, around the center.
##' @param timeMargin margin for time, around the center (\code{dt}).
##' @param depthRange Range of depth of interest
##' @return A list containing the desired lat/lon/time ranges.
return_range_from_all_triplets <- function(source_table, latMargin, lonMargin,
                                           timeMargin, depthRange) {

  ## Subset lat lon and tiem
  lat_all <- source_table[, "lat"]
  lon_all <- source_table[, "lon"]
  dt_all <- source_table[, "time"]

  ## Grab ranges of Everything.
  lat <- range(lat_all, na.rm=TRUE)
  lon <- range(lon_all, na.rm=TRUE)
  range_dat <- function(dats) {
    ## For now, assume that the dates are sorted
    return(c(toString(dats[1]), toString(dats[length(dats)])))
  }
  dt <- range_dat(dt_all) ## Assume time is sorted

  ## Add and subtract date
  dt <- sub("T", " ", dt)
  dt_min <- as.POSIXlt(dt[1]) + (-1) * 60 * 60 * 24 * timeMargin
  dt_max <- as.POSIXlt(dt[2]) + (+1) * 60 * 60 * 24 * timeMargin
  dt_range <- c(dt_min, dt_max)
  dt_range <- sub(" ", "T", dt_range)

  ## Create the range of times and latitude and longitudes (a box)
  lat_min <- lat[1] + c(-1) * latMargin
  lat_max <- lat[2] + c(+1) * latMargin

  lon_min <- lon[1] + c(-1) * lonMargin
  lon_max <- lon[2] + c(+1) * lonMargin

  range_var <- list()
  range_var$lat <- c(lat_min, lat_max)
  range_var$lon <- c(lon_min, lon_max)
  range_var$time <- dt_range

  if (!is.null(depthRange)) range_var$depth <- depthRange # recently added
  return(range_var)
}



##' Helper for matchSource() that takes in a lat/lon/date triplet and the
##' margins of error for query, and returns a "range" variable.
##' @param lat center latitude.
##' @param lon center longitude.
##' @param dt center date.
##' @param latMargin margin for latitude, around the center.
##' @param lonMargin margin for longitude, around the center.
##' @param timeMargin margin for time, around the center (\code{dt}).
##' @param depthRange Range of depth of interest
##' @return A list containing the desired lat/lon/time ranges.
return_range <- function(lat, lon, dt, latMargin, lonMargin, timeMargin,
                         depthRange) {

  ## Add and subtract date
  dt <- sub("T", " ", dt)
  dt_range <- as.POSIXlt(dt) + c(-1, +1) * 60 * 60 * 24 * timeMargin
  dt_range <- sub(" ", "T", dt_range)


  ## Create the range of times and latitude and longitudes (a box)
  range_var <- list()
  range_var$lat <- lat + c(-1, +1) * latMargin ## lat_range##c(25,30) ## lat.range
  range_var$lon <- lon + c(-1, +1) * lonMargin ## c(-160,-155) ## lon.range
  range_var$time <- dt_range
  if (!is.null(depthRange)) range_var$depth <- depthRange # recently added
  return(range_var)
}


##' Helper to take the tibbles containing min/maxs of the variables, and
##' transforms it into a set of 2 by #(variables) data frames.
##' @param myrange Result of \code{return_range()}, containing user-desired
##'   ranges of variables.
##' @return 2 by #(variables) data frame.
range_tibble_to_table <- function(myrange) {
  varnames <- sapply(names(myrange), function(myname) {
    loc <- as.numeric(gregexpr("_", myname))
    substring(myname, 1, loc - 1)
  })
  mins <- myrange[which(sapply(names(myrange), function(myname) {
    grepl("min", myname)
  }))]
  maxs <- myrange[which(sapply(names(myrange), function(myname) {
    grepl("max", myname)
  }))]
  names(mins) <- names(maxs) <- rep("", 3)
  tab <- data.frame(rbind(mins, maxs))
  colnames(tab) <- varnames[1:(length(varnames) / 2)]
  rownames(tab) <- c("min", "max")
  return(tab)
}


##' Matches range of tables.
##' @param range_tables tables containing ranges.
##' @param source csv file name.
##' @return names of the tables whose lat/lon/time ranges have /any/ match with
##'   the cruise trajectory in \code{source}.
match_ranges <- function(range_tables, source) {

  ## For each entry in the source, get the range
  source_tab <- read.csv(source, colClasses = c("POSIXct", "numeric", "numeric"))
  stopifnot(all(names(source) == c("time", "lat", "lon")))
  source_range <- lapply(1:ncol(source_tab), function(icol) {
    var <- source_tab[, icol]
    var <- var[which(!is.na(var))]
    range(var)
  })
  source_range <- data.frame(
    time = source_range[[1]],
    lat = source_range[[2]],
    lon = source_range[[3]]
  )

  ## Check overlap
  ## overlaps = sapply(range.tables, function(table.range){
  overlaps <- sapply(1:length(range_tables), function(ii) {
    table_range <- range_tables[[ii]]
    verdict <- any_overlap(table_range, source_range)
    return(verdict)
  })
  return(names(range_tables)[overlaps])
}

##' Helper to identify overlap between one range (table_range) and the reference
##' (source_range).
##' @param table_range Lat/lon/time range of target table.
##' @param source_range Lat/lon/time range of source.
##' @return \code{TRUE} if any overlap exists in lat/lon/time.
any_overlap <- function(table_range, source_range) {

  ## Manually check column names, so they are time/lat/lon, in that order.
  table_range <- table_range[, which(names(table_range) %in% c("time", "lat", "lon"))]
  stopifnot(all(names(table_range) == names(source_range)))
  stopifnot(all(names(table_range) == c("time", "lat", "lon")))

  ## Make the comparison
  combined_range <- rbind(table_range, source_range)
  overlaps <- sapply(combined_range, function(myvar) {
    one_overlap(myvar[1], myvar[2], myvar[3], myvar[4])
  })
  return(all(overlaps))
}

##' Helper that checks: (StartA <= EndB) and (EndA >= StartB), in 1d.
##' @param min1 minimum of first range.
##' @param max1 maximum of first range.
##' @param min2 minimum of second range.
##' @param max2 maximum of second range.
##' @return \code{TRUE} if there is overlap.
one_overlap <- function(min1, max1, min2, max2) {
  return((min1 <= max2) & (max1 >= min2))
}

