#' Connect to the CMAP database
#'
#' User needs to provide CMAP databse login credentials once to connect to the database. Thereafter, call "connect2cmap" with driver inofrmation to connect to the database.
#'
#' @param Driver name of the database driver to be used to connect to the database
#' @param reset TRUE if key_ring credentials needs to be deleted
#' @return connection object for the database
#' @export
#' @import magrittr
#' @importFrom keyring key_list key_set key_get key_delete
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @examples
#' \dontrun{
#' library(cmap4r)
#' con <- connect_cmap(Driver = "libtdsodbc.so")
#' #
#' ## Input: Table name; variable name, space time range information
#' table.name <- "tblsst_AVHRR_OI_NRT" # table name
#' sel.var <- "sst" # choose variable
#' range.var <- list() # Range variable [lat,lon,time]
#' range.var$lat <- c(10, 70)
#' range.var$lon <- c(-180, -80)
#' range.var$time <- c("2016-04-30", "2016-04-30")
#' #
#' ## Subset selection:
#' tbl.subset <- get_table(con, table.name, sel.var, range.var)
#' head(tbl.subset)
#' #
#' dbDisconnect(con)
#' }
connect_cmap <- function(Driver = "libtdsodbc.so", reset = FALSE) {
  if (reset){
    print("Database credential deteted")
    keyring::key_delete("cmap_con")
  }
  if (nrow(keyring::key_list("cmap_con")) == 0) {
    print("Enter database credential ..")
    UID <- readline("Enter user name:")
    keyring::key_set(
      service = "cmap_con",
      username = UID
    )
  }
  print("Getting database credential...")
  con <- DBI::dbConnect(odbc::odbc(),
    Driver = "libtdsodbc.so",
    Server = "128.208.239.15",
    # Database = "Opedia",
    UID = keyring::key_list("cmap_con")[1, 2],
    PWD = keyring::key_get("cmap_con"),
    Port = 1433
  )
  con
}




