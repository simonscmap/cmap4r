test_that("Testing several functions", {
  ## Starting with something simpler: cruise_by_name

  ## ## Compare URLs
  ## cruiseName = "KOK1606"
  ## myquery = sprintf("EXEC uspCruiseByName '%s' ", cruiseName)
  ## url = query_url(myquery, config_type = "match")
  ## url_python = "https://simonscmap.com/api/data/query?query=EXEC+uspCruiseByName+%27KOK1606%27+"
  ## stopifnot(identical(url, url_python))


  ## load_all("~/repos/cmap4r/package/cmap4r") ## Temporary

  ## Simpler functions ####################
  cruisename = "KOK1606"
  cruise_by_name(cruisename, apiKey)
  cruise_bounds(cruisename, apiKey)
  get_catalog(apiKey)

  ## Subset ###############################
  subset(spName="uspSpaceTime",
         table='tblAMT13_Chisholm',
         variable='MIT9313PCR_Chisholm',
         dt1='2003-09-14',
         dt2='2003-10-13',
         lat1=-48,
         lat2=48,
         lon1=-52,
         lon2=-11,
         depth1=0,
         depth2=240,
         apiKey=apiKey)


  ## Atomic match #########################

  ## Parameters for colocalization
  spName = "uspMatch"
  sourceTable='tblAMT13_Chisholm'
  sourceVar= 'MIT9313PCR_Chisholm'
  targetTable='tblChl_REP'
  targetVar='chl'
  ## py
  dt1='2003-09-13 00:00:00'
  dt2= '2003-10-14 00:00:00'
  lat1=-48.25
  lat2=48.25
  lon1=-52.25
  lon2=-10.75
  depth1=0
  depth2=240
  temporalTolerance=4
  latTolerance=0.25
  lonTolerance=0.25
  depthTolerance=0
  atomic_match(spName, sourceTable, sourceVar, targetTable, targetVar, dt1, dt2,
               lat1, lat2, lon1, lon2, depth1, depth2, temporalTolerance,
               latTolerance, lonTolerance, depthTolerance, apiKey)


  ## Try out match()
  sourceTable='tblAMT13_Chisholm'
  sourceVar='MIT9313PCR_Chisholm'
  targetTables=c('tblAMT13_Chisholm', 'tblWOA_Climatology', 'tblChl_REP')
  targetVars=c('MIT9312PCR_Chisholm', 'phosphate_WOA_clim', 'chl')
  dt1='2003-09-14'
  dt2='2003-10-13'
  lat1=-48
  lat2=48
  lon1=-52
  lon2=-11
  depth1=0
  depth2=240
  temporalTolerance=c(0, 0, 1)
  latTolerance=c(0, 0.5, 0.25)
  lonTolerance=c(0, 0.5, 0.25)
  depthTolerance=c(0, 5, 0)
  dat = compile(sourceTable, sourceVar, targetTables, targetVars, dt1, dt2, lat1,
                lat2, lon1, lon2, depth1, depth2, temporalTolerance, latTolerance,
                lonTolerance, depthTolerance, apiKey)

  ## Now, try out the main function for colocalizing a cruise (note: we can only
  ## colocalize cruises that are already in CMAP.)
  cruise='diel'
  targetTables=c('tblSeaFlow', 'tblPisces_NRT')
  targetVars=c('synecho_abundance', 'NO3')
  depth1=0
  depth2=5
  temporalTolerance=c(0, 4)
  latTolerance=c(0, 0.25)
  lonTolerance=c(0, 0.25)
  depthTolerance=c(5, 5)
  dat = along_track(cruise,
                    targetTables,
                    targetVars,
                    depth1,
                    depth2,
                    temporalTolerance,
                    latTolerance,
                    lonTolerance,
                    depthTolerance,
                    apiKey)
  ## Expect a warning/error in NO3!
})
