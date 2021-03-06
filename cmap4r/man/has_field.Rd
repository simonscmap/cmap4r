% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_info.R
\name{has_field}
\alias{has_field}
\title{Returns a boolean outcome checking if a field (varName) exists in a table (data set).}
\usage{
has_field(tableName, varName)
}
\arguments{
\item{tableName}{table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database.}

\item{varName}{specify short name of a variable in the table.}
}
\value{
boolean outcome
}
\description{
Returns a boolean outcome checking if a field (varName) exists in a table (data set).
}
\examples{
\donttest{
#
## Input: Table name; variable name
tableName <- "tblArgoMerge_REP" # table name
varName <- "argo_merge_chl_adj" # variable name
#
## Variable attribute:
var_exist <- has_field(tableName, varName)
var_exist
#
}
}
