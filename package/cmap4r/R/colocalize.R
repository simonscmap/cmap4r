#' Colocalization of a dataset using lattitude, longitude, depth and time information
#'
#' Takes all the lat/lon/time triplets in the comma separated \code{source}
#' file, collects and aggregates (mean and standard deviation) each variable's
#' data in a rectangle around each triplet. The size of the rectangle is
#' determined by a user-input margin in lat, lon and time.
#'
#'
#' @param con  connection object to the database
#' @param table.name table name.
#' @param sel.vars variables in this table that are to be queried.
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
#' ## Setup
#' con = DBI::dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
#' latMargin = 0.3
#' lonMargin = 0.3
#' timeMargin = 1
#' source = 'somefile.csv'
#' table.name = "tblSST_AVHRR_OI_NRT"
#' sel.var = "sst"
#' res = matchSourceOneTable(con, source, table.name, sel.var, latMargin, lonMargin, timeMargin)
#' print(head(res))
#' }
matchSourceOnetable <- function(con, source, table.name, sel.vars,
                        latMargin, lonMargin, timeMargin, depthRange=NULL,
                        orderby=NULL, N=1E5 ## TODO: Find a good number.
                        ){

  ## Read in source table
  source.table = read.csv(source)
  ntriplets = nrow(source.table)

  ## Create data frame to fill in
  DF = data.frame(matrix(nrow=ntriplets)[,-1])
  DF[["lat"]] = source.table[,"lat"]
  DF[["lon"]] = source.table[,"lon"]
  DF[["time"]] = source.table[,"time"]

  ## TODO Remember to add depth query
  ## if not 'depth' in srcDF.columns:
  ##     srcDF['depth'] = 0

  ## For each variable in "variables",
  for(sel.var in sel.vars){
    means = stds = rep(NA, ntriplets)

    ## Download the entire table first
    range.var.all = return_range_from_all_triplets(source.table, latMargin,
                                                   lonMargin, timeMargin,
                                                   depthRange)
    ## n = getObservationCount(con, table.name, type="exact")
    ## if(n > N) stop("Query is too large!")
    tbl = getTableData(con, table.name, sel.var, range.var.all, orderby)

    for(itriplet in 1:ntriplets){
      ## printprogress(itriplet, ntriplets)

      lat = source.table[itriplet,"lat"]
      lon = source.table[itriplet,"lon"]
      dt = source.table[itriplet, "time"]

      ## Query that subtable:
      range.var = return_range(lat, lon, dt, latMargin, lonMargin, timeMargin, depthRange)

      ## Get a subset from /that/ table.
      tbl.subset = subset_from_tbl(tbl, range.var)

      ## Aggregate it (for now, only calculating means)
      if(nrow(tbl.subset)==0){
        means[itriplet] = NA
      } else {
        values = tbl.subset[[sel.var]]
        means[itriplet] = mean(values, na.rm=TRUE) ## Removing the NAs
        stds[itriplet] = sd(values, na.rm=TRUE) ## Removing the NAs
      }
    }
    DF[[sel.var]] = means
    DF[[paste0(sel.var, "-std")]] = stds
    cat(fill=TRUE)
  }
  return(DF)
}
