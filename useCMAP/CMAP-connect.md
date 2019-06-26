CMAP for R users
================

Overview
--------

[Simons CMAP](https://cmap.readthedocs.io/en/latest/index.html) is an initiative from [CBIOMES](https://cbiomes.org/) collaboration to provide open source database service for to unify, visualize, and analyze ocean data sets such as field campaign observations, remote sensing satellite data, and model outputs. The project is supported by the Simons Foundation .

Currently, [CMAP](https://cmap.readthedocs.io/en/latest/index.html) host examples and function to analyze and process data for Python users. In order to make R users familiar to the data base, we provide examples in R to connect, process, analyze and visualize the data.

<!-- This is an R Markdown format used for publishing markdown documents to GitHub. When you click the **Knit** button all R code chunks are run and a markdown file (.md) suitable for publishing to GitHub is generated. -->
Installation
------------

CMAP host data on Microsoft SQL server. An R user will need following package: "DBI", for database interface; "odbc" for connecting to the database using *DBI* interface.

One canb perform required data processing; a) by downloading the data as *data frame* to local computer; or b) using function from the *dbplyr* package which facilitates data preparation on the database itself. Our *R script* examples will aim to demonstrate both the approach of data processing.

Please following the [link](https://db.rstudio.com/best-practices/drivers/) to install required driver for an operating system (*Windows, Linux, Mac*).

``` r
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

##
```

Connecting to the database
--------------------------

User can connect to the database directly using login creddential(*Direct connect*), or via DSN(Data Source Name). In the latter case, user need to define DSN once. Connection to the database is established via DSN. Following examples with demonstrate required steps in both the scenario.

``` r
## Approach I: Direct connection
library(DBI)
library(odbc)
con <- DBI::dbConnect(odbc(),
                      Driver = "libtdsodbc.so",     ## Free TDS driver used
                      Server = "128.208.239.15",    ## IP address of the server
                      Database = "Opedia",          ## Database name
                      UID = "ArmLab",               ## User ID
                      PWD = "ArmLab2018",           ## Password
                      Port = 1433)                  ## Port number
dbDisconnect(con)


## Approach II: Using defined DSN name "CMAP-MSSQL"
con <- dbConnect(odbc::odbc(), DSN="CMAP-MSSQL",UID="ArmLab",PWD="ArmLab2018")
dbDisconnect(con)
```

Catalog of the data:
--------------------

| Dataset Name                                                                                                                                                                                                                                                                                                                            | Table Name                       | Sensor    | Make        | Spatial Resolution | Temporal Resolution | Start Date | End Date   |
|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------|:----------|:------------|:-------------------|:--------------------|:-----------|:-----------|
| Reprocessed Sea Surface Height and Derived Variables                                                                                                                                                                                                                                                                                    | tblAltimetry\_REP                | Satellite | Observation | 1/4° X 1/4°        | Daily               | NA         | NA         |
| Global Ocean Wind L4 Near real Time 6 hourly                                                                                                                                                                                                                                                                                            | tblWind\_NRT                     | Satellite | Observation | 1/4° X 1/4°        | Six Hourly          | NA         | NA         |
| Chlorophyll Concentration (Optimal-Interpolation)                                                                                                                                                                                                                                                                                       | tblCHL\_OI\_REP                  | Satellite | Observation | 4km X 4km          | Daily               | NA         | NA         |
| Sea Surface Temperature                                                                                                                                                                                                                                                                                                                 | tblSST\_AVHRR\_OI\_NRT           | Satellite | Observation | 1/4° X 1/4°        | Daily               | NA         | NA         |
| Relative Vorticirty                                                                                                                                                                                                                                                                                                                     | tblVort\_REP                     | Satellite | Observation | 1/4° X 1/4°        | Daily               | NA         | NA         |
| Lagrangian Coherent Structures Reprocessed                                                                                                                                                                                                                                                                                              | tblLCS\_REP                      | Satellite | Observation | 1/25° X 1/25°      | Daily               | NA         | NA         |
| Biogeochemistry Analysis and Weekly Forcast                                                                                                                                                                                                                                                                                             | tblPisces\_NRT                   | Blend     | Model       | 1/2° X 1/2°        | Weekly              | NA         | NA         |
| Surface Ocean CO2 Atlas                                                                                                                                                                                                                                                                                                                 | tblSOCAT                         | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Global Eddy Dataset (ML)                                                                                                                                                                                                                                                                                                                | tblEddyCoresML                   | Satellite | Observation | Irregular          | Daily               | NA         | NA         |
| Global Eddy Dataset (Chelton)                                                                                                                                                                                                                                                                                                           | tblChelton                       | Satellite | Observation | Irregular          | Daily               | NA         | NA         |
| Underway Flow Cytometry                                                                                                                                                                                                                                                                                                                 | tblSeaFlow                       | In-Situ   | Observation | Irregular          | Three Minutes       | NA         | NA         |
| Biogeochemistry Climatology Model                                                                                                                                                                                                                                                                                                       | tblDarwin\_Nutrient\_Climatology | Blend     | Model       | 1/2° X 1/2°        | Monthly Climatology | NA         | NA         |
| Particulate cobalamin and pseudocobalamin concentrations from cruise KM1314, published in Heal et al 2017                                                                                                                                                                                                                               | tblCobalamin                     | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Primary Production HOT                                                                                                                                                                                                                                                                                                                  | tblHOT\_PP                       | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Particle Flux HOT                                                                                                                                                                                                                                                                                                                       | tblHOT\_ParticleFlux             | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Zooplankton Community Structure HOT                                                                                                                                                                                                                                                                                                     | tblHOT\_Macrozooplankton         | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Epi-Fluorescence Microscopy HOT                                                                                                                                                                                                                                                                                                         | tblHOT\_EpiMicroscopy            | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| CTD Hydrography HOT                                                                                                                                                                                                                                                                                                                     | tblHOT\_CTD                      | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Bottle Data HOT                                                                                                                                                                                                                                                                                                                         | tblHOT\_Bottle                   | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Reprocessed Argo Merge (Phys+Bio) Profiles                                                                                                                                                                                                                                                                                              | tblArgoMerge\_REP                | In-Situ   | Observation | Irregular          | Irregular           | NA         | NA         |
| Global Database of Prochlorococcus and Synechococcus                                                                                                                                                                                                                                                                                    | tblFlombaum                      | In-Situ   | Observation | Irregular          | Irregular           | 1987-09-17 | 2008-11-10 |
| Near-Real-Time Sea Surface Height and Derived Variables                                                                                                                                                                                                                                                                                 | tblAltimetry\_NRT                | Satellite | Observation | 1/4° X 1/4°        | Daily               | NA         | NA         |
| Near-Real-Time Chlorophyll Concentration (Optimal-Interpolation)                                                                                                                                                                                                                                                                        | tblCHL\_OI\_NRT                  | Satellite | Observation | 4km X 4km          | Daily               | NA         | NA         |
| Exact Amplicon Sequence Variants                                                                                                                                                                                                                                                                                                        | tblESV                           | In-Situ   | Observation | Irregular          | Irregular           | 2012-04-11 | 2012-05-11 |
| Physics Analysis and Daily Forcast                                                                                                                                                                                                                                                                                                      | tblMercator\_MLD\_NRT            | Blend     | Model       | 1/12° X 1/12°      | Daily               | NA         | NA         |
| The Soil Moisture Active Passive (SMAP) ocean surface salinity                                                                                                                                                                                                                                                                          | tblSSS\_NRT                      | Satellite | Observation | 70km X 70km        | Eight Day Running   | NA         | NA         |
| MODIS Aerosol Optical Depth Reprocessed                                                                                                                                                                                                                                                                                                 | tblModis\_AOD\_REP               | Satellite | Observation | 1° X 1°            | Daily               | 2002-07-01 | 2002-07-01 |
| BiGRAPA1: Prochlorococcus abundance and associated (bottle) metadata                                                                                                                                                                                                                                                                    | tblBottle\_Chisholm              | In-Situ   | Observation | Irregular          | Three Minutes       | 2010-11-19 | 2010-12-12 |
| BiGRAPA1: CTD data                                                                                                                                                                                                                                                                                                                      | tblCTD\_Chisholm                 | In-Situ   | Observation | Irregular          | Three Minutes       | 2010-11-19 | 2010-12-12 |
| 2017 Microbial Ecology of the Surface Ocean - SCOPE, Water Column Data                                                                                                                                                                                                                                                                  | tblMesoscope\_km1709             | In-Situ   | Observation | Irregular          | Three Minutes       | 2017-06-26 | 2017-07-12 |
| Single cell genomes of Prochlorococcus, Synechococcus, extracellular virus particles, and additional microorganisms from a diverse range of bacterial, archaeal, and viral groups, coming from a diverse set of marine environments: Isolation information and where to access 16S rRNA, genome sequencing, and genome annotation files | tblSingleCellGenomes\_Chisholm   | In-Situ   | Observation | Irregular          | Three Minutes       | 2009-07-14 | 2016-04-27 |
| Darwin Biogeochemistry 3 Day Averaged Model                                                                                                                                                                                                                                                                                             | tblDarwin\_3day                  | Blend     | Model       | 1/4° X 1/4°        | Three Days          | 1992-01-02 | 1992-01-02 |

<!-- ## Including Plots -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo=FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot. -->
