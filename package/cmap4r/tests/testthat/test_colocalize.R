test_that("Testing several functions", {
  ## Starting with something simpler: cruise_by_name

  ## ## Compare URLs
  ## cruiseName = "KOK1606"
  ## myquery = sprintf("EXEC uspCruiseByName '%s' ", cruiseName)
  ## url = query_url(myquery, config_type = "match")
  ## url_python = "https://simonscmap.com/api/data/query?query=EXEC+uspCruiseByName+%27KOK1606%27+"
  ## stopifnot(identical(url, url_python))

  ## Simpler functions ####################
  la()
  apiKey = "31095550-d3d9-11e9-9174-fdf4e45bb057"
  cruiseName = "KOK1606"
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
})
