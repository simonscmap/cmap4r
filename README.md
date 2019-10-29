# CMAP for R users

[Simons CMAP](https://cmap.readthedocs.io/en/latest/index.html) is an initiative from [CBIOMES](https://cbiomes.org/) collaboration that provides open source database service to unify, visualize, and analyze ocean data sets from the  field campaign observations, remote sensing satellite, and model outputs. The project is supported by the [Simons Foundation](https://www.simonsfoundation.org/).

To make R users familiar to the database, the package "cmap4r" allows a user to download, visualize, and process data from the CMAP database.



## Getting Started

CMAP hosts data on a Microsoft SQL Server. To retrieve the data from the database, *cmap4r* requires an API key. For this, the user needs to register on the [SimonsCMAP](https://simonscmap.com/register) website and follow the APIKeys tab on the webpage to obtain the API key. The package allows the user to save the key as a **keyring variable** which will be used for data retrieval whenever required.



### Installation
Currently, we host the latest version of the package on [GitHub](https://github.com/simonscmap/cmap4r). Please follow the instruction below to install the package. Also, install some additional R package for processing and visualizing the data. 

```
## Package "cmap4r" for downloading and visualizing data 
devtools::install_github("simonscmap/cmap4r/cmap4r")

## Package for data processing:
install.packages("dbplyr")  
install.packages("plyr")

## Package for visualization:
install.packages("ggplot2")
install.packages("plotly")

```

### Save API Key as *keyring*

After installing *cmap4r*, use the **set_authorization** function to set/reset the API Key. 

```
library(cmap4r)

# To set the API authorization key
set_authorization()

# To reset the authorization key
set_authorization(reset = TRUE)
```



### Working example:

To download the data as a table from the  CMAP database in a given range of space and time, use appropriate retrieval functions:  
- get_spacetime
- get_section
- get_timeseries
- get_depthprofile
- exec_manualquery

```
library(cmap4r)
#
## Input: Table name; variable name.
table_name <- "tblArgoMerge_REP" # table name
sel_var <- "argo_merge_chl_adj" # choose variable
#
# Range variable [lat,lon,time]
lat1 = 20; lat2 = 24
lon1 = -170; lon2 = -150
dt1 = "2016-04-30"; dt2 = "2016-04-30"
depth1 <- 0; depth2 =  1500
#
## Subset selection:
tbl.subset <- get_depthprofile(table_name, sel_var, lat1, lat2, lon1, lon2,
                               dt1, dt2, depth1, depth2)
head(tbl.subset)
#
```
Refer **bookdown** to learn about using the data retrieval and visualization function available in the **cmap4r**.
