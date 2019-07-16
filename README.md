# CMAP for R users

[Simons CMAP](https://cmap.readthedocs.io/en/latest/index.html) is an initiative from [CBIOMES](https://cbiomes.org/) collaboration to provide open source database service for to unify, visualize, and analyze ocean data sets such as field campaign observations, remote sensing satellite data, and model outputs. The project is supported by the Simons Foundation .

Currently, [CMAP](https://cmap.readthedocs.io/en/latest/index.html) host examples and function to analyze and process data for Python users. In order to make R users familiar to the data base, we provide examples in R to connect, process, analyze and visualize the data.


## Getting Started

CMAP hosts data on a Microsoft SQL Server. 

### Prerequisites

For  Mac operating system, a user needs to install the unixODBC library and database drivers. We suggest using SQL Server ODBC drivers (Free TDS). Using Homebrew, run the following commands to install the suggested module.


-   brew install unixodbc
-   brew install freetds

In case of Linux operating system, first, install [Anaconda
distribition](https://www.anaconda.com/distribution/#linux), and then
run the following commands to install suggested module.

-   conda install -c anaconda unixodbc
-   conda install -c anaconda freetds

Please follow the [link](https://db.rstudio.com/best-practices/drivers/)
to see other drivers available for installation.


### R user
An R user will need the following package to connect to the database: a) “DBI”, for database interface; b) “odbc” for connecting to the database using **DBI** interface.

In addition, user may require some additional R package for downloading, processing and visualizing the data. Run the following commands to install some of the essential packages.


```
## Package "DBI" provide interface to the database
install.packages("DBI")

## Driver for the database
install.packages("odbc")

## Package for data processing:
install.packages("dbplyr")  
install.packages("plyr")

## Package for visualization:
install.packages("ggplot2")
install.packages("plotly")

```
## R package

Functions in the *cmap4r* package facilitates downloading, preprocessing and visualizing the data from the CMAP database. 

### Connecting to the database

*connect_cmap* function the *cmap4r* package returns connection object to the database. 

```
library(cmap4r)
## connection object to the database
con <- connect_cmap(Driver = "libtdsodbc.so")

dbDisconnect(con)
```


### Example:

Use *get_table* function to download data from a table on CMAP database in given range of space and time.

```
library(cmap4r)
con <- connect_cmap(Driver = "libtdsodbc.so")
#
## Input: Table name; variable name, space time range information
table.name <- "tblsst_AVHRR_OI_NRT" # table name
sel.var <- "sst" # choose variable
range.var <- list() # Range variable [lat,lon,time]
range.var$lat <- c(10, 70)
range.var$lon <- c(-180, -80)
range.var$time <- c("2016-04-30", "2016-04-30")
#
## Subset selection:
tbl.subset <- get_table(con, table.name, sel.var, range.var)
head(tbl.subset)
#
dbDisconnect(con)
```

