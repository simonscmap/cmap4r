% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_info.R
\name{get_var_unit}
\alias{get_var_unit}
\title{Returns the unit of a table variable on the Simons CMAP database.}
\usage{
get_var_unit(tableName, varName)
}
\arguments{
\item{tableName}{table name from the Simons CMAP database. Use "get_catalog()" to retrieve list of tables on the database.}

\item{varName}{specify short name of a variable in the table.}
}
\value{
measuring unit of a table variable as dataframe
}
\description{
Returns the unit of a table variable on the Simons CMAP database.
}
\examples{
\donttest{
#
## Input: Table name; variable name
tableName <- "tblArgoMerge_REP" # table name
varName <- "argo_merge_chl_adj" # variable name
#
## Variable attribute:
unitName <- get_var_unit(tableName, varName)
unitName
#
}
}
