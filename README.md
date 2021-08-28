# CMAP for R Users

## Authors
Sangwon Hyun
Aditya Mishra
Jacob Bien
Christian Müller

![](caustics_grid.ng)


## Overview

[Simons CMAP](https://cmap.readthedocs.io/en/latest/index.html) is an initiative
from the [CBIOMES](https://cbiomes.org/) collaboration that provides an open
source database service to unify, visualize, and analyze ocean data sets from
the field campaign observations, remote sensing satellite, and model
outputs. The project is supported by the [Simons
Foundation](https://www.simonsfoundation.org/).

The `cmap4r` R package makes it easy for users to download, visualize, and process data from the CMAP database.



## Getting Started

CMAP hosts data on a Microsoft SQL Server. To retrieve data from the database,
`cmap4r` requires an API key. For this, the user needs to register on the
[SimonsCMAP](https://simonscmap.com/register) website and navigate to the
"APIKeys" tab on the webpage to obtain an API key. The package allows the user
to save the key as a **keyring variable** which will be used for data retrieval
whenever required.



### Installation
Currently, we host the latest version of the package on
[GitHub](https://github.com/simonscmap/cmap4r). Please follow the instructions
below to install the package and some additional R packages for processing and
visualizing the data.

```
## Install cmap4r:
devtools::install_github("simonscmap/cmap4r/cmap4r")

## Package for data processing:
install.packages("dplyr")  

## Packages for visualization:
install.packages("ggplot2")
install.packages("plotly")

```

### Saving API Key

After installing `cmap4r`, use the `set_authorization` function to set/reset the API Key. 

```
library(cmap4r)

# To set the API authorization key
set_authorization(cmap_key = "enter_api_key")

# To reset the authorization key
set_authorization(reset = TRUE)
```


### A First Example

Suppose we are interested in how chlorophyll concentration varies with depth in
a certain region of the ocean during a certain period of time.  The function
`get_depthprofile` gives us the average (and standard deviation) of a variable
of interest at each depth, where the averaging is performed for each depth over
a given range of longitudes, latitudes, and times.  We will look at chlorophyll
concentration as measured by the Argo Floats (which is the variable
`argo_merge_chl_adj` in the table `tblArgoMerge_REP`).

```
get_depthprofile(table_name = "tblArgoMerge_REP",
                 sel_var = "argo_merge_chl_adj",
                 lat1 = 20, lat2 = 24,
                 lon1 = -170, lon2 = -150,
                 dt1 = "2016-04-30", dt2 = "2016-04-30",
                 depth1 = 0, depth2 = 1500)
```

This gives a `tibble` with the desired information.

There are many functions like `get_depthprofile` for retrieving data from the CMAP database, including

- `get_spacetime`
- `get_section`
- `get_timeseries`
- `exec_manualquery`

For a complete list and many more examples of data retrieval and visualization,
please see the **tutorial** here: https://simonscmap.github.io/cmap4r/.
