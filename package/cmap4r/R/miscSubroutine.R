#' Check if the specified range variable is present in the table or not
#'
#' @param con connection object to the database
#' @param table.name table name in the database
#' @param range.var range variable
#' @return 0 or index of range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
rangeVarCheck <- function(con, table.name, range.var) {
  # tbl.connect <- dplyr::tbl(con,table.name)
  rvarName <- names(range.var)
  tblxx <- tbl_sample(con, table.name, n = 5)
  inde <- match(rvarName, names(tblxx))
  if (any(is.na(inde))) {
    print("Check speccified range variable:")
    return(0)
  } else {
    return(inde)
  }
}


#'  Check if the specified variables are present in the table or not
#'
#' @param con connection object to the database
#' @param table.name table name in the database
#' @param varname names of variables
#' @return 0 or index of range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
tableVarMatch <- function(con, table.name, varname) {
  # tbl.connect <- dplyr::tbl(con,table.name)
  tblxx <- tbl_sample(con, table.name, n = 5)

  inde <- match(varname, names(tblxx))
  if (any(is.na(inde))) {
    print("Check specified  variable:")
    return(NA)
  } else {
    return(inde)
  }
}









# getSubsetSpaceTimeRange11  = function(con,table.name,range.var){
#   tbl.connect <- tbl(con,table.name)
#   tbl.subsetSpaceTimeSummary <- tbl.connect %>%
#     dplyr::filter(between(time, range.var$time[1], range.var$time[2]),
#            between(lat,range.var$lat[1],range.var$lat[2]),
#            between(lon,range.var$lon[1],range.var$lon[2])) %>%
#     summarise(nObs = n(), MaxLat = max(lat, na.rm = TRUE),
#               MinLat = min(lat, na.rm = TRUE),
#               MaxLon = max(lon, na.rm = TRUE),
#               MinLon = min(lon, na.rm = TRUE),
#               MaxTime = max(time, na.rm = TRUE),
#               MinTime = min(time, na.rm = TRUE)) %>%
#     collect()
#   return(tbl.subsetSpaceTimeSummary)
# }




#'  Convert range object to list object: It will be used latter for creating query.
#'
#' @param range.var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
range2list <- function(range.var) {
  range.varx <- data.frame(range.var)
  if (any(names(range.var) == "time")) {
    range.varx$time <- as.character(range.varx$time)
  }
  if (any(names(range.var) == "date")) {
    range.varx$date <- as.character(range.varx$date)
  }
  varname <- names(range.var)
  out <- vector("list", 2)
  out[[2]] <- out[[1]] <- vector("list", length = length(varname))
  names(out[[1]]) <- names(out[[2]]) <- varname
  for (i in 1:2) {
    for (j in 1:length(varname)) {
      out[[i]][[j]] <- range.varx[i, j]
    }
  }
  return(out)
}





#'  Create filter query with equality condition
#'
#' @param range.var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom purrr map imap reduce
#' @importFrom rlang sym expr
getEqualQuery <- function(range.var) {
  out <- range2list(range.var)[1]
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
#' @param range.var range variable
#' @return list object from range variable
#' @import magrittr
#' @importFrom dplyr tbl collect filter select arrange summarise_at select_at
#' @importFrom purrr map imap reduce
#' @importFrom rlang sym expr
getFilterQuery <- function(range.var) {
  out <- range2list(range.var)

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


##' Subsets a subtble from an OFFLINE table (3 columns called "lat" "lon" and
##' "time").
##' @param tbl 3 column matrix.
##' @param range.var Data frame containing information about the "rectangle" of
##'   interest.
##' @return 3 column matrix with only a subset of the table.
subset_from_tbl <- function(tbl, range.var) {

  ## Handle Lat/lon
  lat.ind <- which(tbl[, "lat"] >= range.var$lat[1] &
    tbl[, "lat"] <= range.var$lat[2])
  lon.ind <- which(tbl[, "lon"] >= range.var$lon[1] &
    tbl[, "lon"] <= range.var$lon[2])

  ## Handle time
  times <- tbl[["time"]]
  times <- as.POSIXct(times)
  dt.range <- as.POSIXct(sub("T", " ", range.var$time))
  time.ind <- which(times >= dt.range[1] &
    times <= dt.range[2])

  ## Intersect and return
  joint.ind <- Reduce(intersect, list(lat.ind, lon.ind, time.ind))
  return(tbl[joint.ind, ])
}


##' Helper for matchSource() that takes in many lat/lon/date triplets and the
##' margins of error for query, and returns a "range" variable for all triplets
##' (with margin).
##' @param source.table a matrix with 3 columns which each are: lat (center
##'   latitude), lon (center longitude) dt (center date).
##' @param latMargin margin for latitude, around the center.
##' @param lonMargin margin for longitude, around the center.
##' @param timeMargin margin for time, around the center (\code{dt}).
##' @param depthRange Range of depth of interest
##' @return A list containing the desired lat/lon/time ranges.
return_range_from_all_triplets <- function(source.table, latMargin, lonMargin,
                                           timeMargin, depthRange) {

  ## Subset lat lon and tiem
  lat.all <- source.table[, "lat"]
  lon.all <- source.table[, "lon"]
  dt.all <- source.table[, "time"]

  ## Grab ranges of Everything.
  lat <- range(lat.all)
  lon <- range(lon.all)
  range_dat <- function(dats) {
    ## For now, assume that the dates are sorted
    return(c(toString(dats[1]), toString(dats[length(dats)])))
  }
  dt <- range_dat(dt.all) ## Assume time is sorted

  ## Add and subtract date
  dt <- sub("T", " ", dt)
  dt.min <- as.POSIXlt(dt[1]) + (-1) * 60 * 60 * 24 * timeMargin
  dt.max <- as.POSIXlt(dt[2]) + (+1) * 60 * 60 * 24 * timeMargin
  dt.range <- c(dt.min, dt.max)
  dt.range <- sub(" ", "T", dt.range)

  ## Create the range of times and latitude and longitudes (a box)
  lat.min <- lat[1] + c(-1) * latMargin
  lat.max <- lat[2] + c(+1) * latMargin

  lon.min <- lon[1] + c(-1) * lonMargin
  lon.max <- lon[2] + c(+1) * lonMargin

  range.var <- list()
  range.var$lat <- c(lat.min, lat.max)
  range.var$lon <- c(lon.min, lon.max)
  range.var$time <- dt.range

  if (!is.null(depthRange)) range.var$depth <- depthRange # recently added
  return(range.var)
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
  dt.range <- as.POSIXlt(dt) + c(-1, +1) * 60 * 60 * 24 * timeMargin
  dt.range <- sub(" ", "T", dt.range)


  ## Create the range of times and latitude and longitudes (a box)
  range.var <- list()
  range.var$lat <- lat + c(-1, +1) * latMargin ## lat.range##c(25,30) ## lat.range
  range.var$lon <- lon + c(-1, +1) * lonMargin ## c(-160,-155) ## lon.range
  range.var$time <- dt.range
  if (!is.null(depthRange)) range.var$depth <- depthRange # recently added
  return(range.var)
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
##' @param range.tables tables containing ranges of each ta.
##' @param source csv file name.
##' @return names of the tables whose lat/lon/time ranges have /any/ match with
##'   the cruise trajectory in \code{source}.
match.ranges <- function(range.tables, source) {

  ## For each entry in the source, get the range
  source.tab <- read.csv(source, colClasses = c("POSIXct", "numeric", "numeric"))
  stopifnot(all(names(source) == c("time", "lat", "lon")))
  source.range <- lapply(1:ncol(source.tab), function(icol) {
    var <- source.tab[, icol]
    var <- var[which(!is.na(var))]
    range(var)
  })
  source.range <- data.frame(
    time = source.range[[1]],
    lat = source.range[[2]],
    lon = source.range[[3]]
  )

  ## Check overlap
  ## overlaps = sapply(range.tables, function(table.range){
  overlaps <- sapply(1:length(range.tables), function(ii) {
    table.range <- range.tables[[ii]]
    verdict <- any.overlap(table.range, source.range)
    return(verdict)
  })
  return(names(range.tables)[overlaps])
}

##' Helper to identify overlap between one range (table.range) and the reference
##' (source.range).
##' @param table.range Lat/lon/time range of target table.
##' @param source.range Lat/lon/time range of source.
##' @return \code{TRUE} if any overlap exists in lat/lon/time.
any.overlap <- function(table.range, source.range) {

  ## Manually check column names, so they are time/lat/lon, in that order.
  table.range <- table.range[, which(names(table.range) %in% c("time", "lat", "lon"))]
  stopifnot(all(names(table.range) == names(source.range)))
  stopifnot(all(names(table.range) == c("time", "lat", "lon")))

  ## Make the comparison
  combined.range <- rbind(table.range, source.range)
  overlaps <- sapply(combined.range, function(myvar) {
    one.overlap(myvar[1], myvar[2], myvar[3], myvar[4])
  })
  return(all(overlaps))
}

##' Helper that checks: (StartA <= EndB) and (EndA >= StartB), in 1d.
##' @param min1 minimum of first range.
##' @param max1 maximum of first range.
##' @param min2 minimum of second range.
##' @param max2 maximum of second range.
##' @return \code{TRUE} if there is overlap.
one.overlap <- function(min1, max1, min2, max2) {
  return((min1 <= max2) & (max1 >= min2))
}





# # Plot - Regional map
# lat.ref <- data.frame(lat=unique(tbl.subset$lat))
# lat.ref$ind <- 1:nrow(lat.ref)
# lon.ref <- data.frame(lon=unique(tbl.subset$lon))
# lon.ref$ind <- 1:nrow(lon.ref)
# ind.lat <- match(tbl.subset$lat,lat.ref$lat)
# ind.lon <- match(tbl.subset$lon,lon.ref$lon)
# Fe.mat <- matrix(NA,nrow = nrow(lon.ref), ncol = nrow(lat.ref))
# for (i in 1:length(ind.lon)) {
#   Fe.mat[ind.lon[i],ind.lat[i]] <- tbl.subset$Fe[i]
# }
# p <- plot_ly(
#   z = t(Fe.mat),
#   x = lon.ref$lon,
#   y = lat.ref$lat,
#   type = "heatmap"
# )
# p %>%
#   layout(title='Regional map',
#          xaxis = list(title = 'Longitude'),
#          yaxis = list(title= 'Latitude'))



# between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
# between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
# between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]),
# between(sql(rvarName[4]), range.var[[rvarName[4]]][1], range.var[[rvarName[4]]][2])


#
# tbl.query <- tbl.connect %>%
#   select(indsel) %>%
#   dplyr::filter(between(sql(rvarName[1]), range.var[[rvarName[1]]][1], range.var[[rvarName[1]]][2]),
#          between(sql(rvarName[2]), range.var[[rvarName[2]]][1], range.var[[rvarName[2]]][2]),
#          between(sql(rvarName[3]), range.var[[rvarName[3]]][1], range.var[[rvarName[3]]][2]))
