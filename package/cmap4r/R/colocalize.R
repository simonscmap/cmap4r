#' Colocalization of a dataset using lattitude, longitude, depth and time information
#'
#' Takes all the lat/lon/time triplets in the comma separated \code{source}
#' file, collects and aggregates (mean and standard deviation) each variable's
#' data in a rectangle around each triplet. The size of the rectangle is
#' determined by a user-input margin in lat, lon and time.
#'
#'
#' @param con  connection object to the database
#' @param table_name table name.
#' @param sel_vars variables in this table that are to be queried.
#' @param latMargin latitude margin.
#' @param lonMargin longitude margin.
#' @param timeMargin time margin.
#' @param depthRange range of depth
#' @param source The file name of the csv source file that contains the
#'   lat/lon/date triplets.
#' @param orderby orderby variable in the selected table
#' @param N description to add later
#' @importFrom utils read.csv
#' @export
#' @return Data frame containing the colocalized data. Each row is an original
#'   time/lat/lon.
#' @examples
#' \dontrun{
#' library(cmap4r)
#' ## Setup
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' latMargin <- 0.3
#' lonMargin <- 0.3
#' timeMargin <- 1
#' source <- "somefile.csv"
#' table_name <- "tblSST_AVHRR_OI_NRT"
#' sel_var <- "sst"
#' res <- match_table(con, source, table_name, sel_var, latMargin, lonMargin, timeMargin)
#' print(head(res))
#' }
match_table <- function(con, source, table_name, sel_vars,
                                latMargin, lonMargin, timeMargin, depthRange = NULL,
                                orderby = NULL, N = 1E5 ## TODO: Find a good number.
) {

  ## Read in source table
  source_table <- read.csv(source)
  ntriplets <- nrow(source_table)

  ## Create data frame to fill in
  DF <- data.frame(matrix(nrow = ntriplets)[, -1])
  DF[["lat"]] <- source_table[, "lat"]
  DF[["lon"]] <- source_table[, "lon"]
  DF[["time"]] <- source_table[, "time"]

  ## TODO Remember to add depth query
  ## if not 'depth' in srcDF.columns:
  ##     srcDF['depth'] = 0

  ## For each variable in "variables",
  for (sel_var in sel_vars) {
    means <- stds <- rep(NA, ntriplets)

    ## Download the entire table first
    range_var_all <- return_range_from_all_triplets(
      source_table, latMargin,
      lonMargin, timeMargin,
      depthRange
    )
    ## n = getObservationCount(con, table.name, type="exact")
    ## if(n > N) stop("Query is too large!")
    tbl <- get_table(con, table_name, sel_var, range_var_all, orderby)

    for (itriplet in 1:ntriplets) {
      ## printprogress(itriplet, ntriplets)

      lat <- source_table[itriplet, "lat"]
      lon <- source_table[itriplet, "lon"]
      dt <- source_table[itriplet, "time"]

      ## Query that subtable:
      range_var <- return_range(lat, lon, dt, latMargin, lonMargin, timeMargin, depthRange)

      ## Get a subset from /that/ table.
      tbl_subset <- subset_from_tbl(tbl, range_var)

      ## Aggregate it (for now, only calculating means)
      if (nrow(tbl_subset) == 0) {
        means[itriplet] <- NA
      } else {
        values <- tbl_subset[[sel_var]]
        means[itriplet] <- mean(values, na.rm = TRUE) ## Removing the NAs
        stds[itriplet] <- sd(values, na.rm = TRUE) ## Removing the NAs
      }
    }
    DF[[sel_var]] <- means
    DF[[paste0(sel_var, "-std")]] <- stds
    cat(fill = TRUE)
  }
  return(DF)
}
