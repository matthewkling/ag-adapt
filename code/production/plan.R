
plan <- drake_plan(
  
  # time series datasets
  acs = process_acs(),
  ag_census = process_ag_census(),
  ts = bind_rows(ag_census, acs),
  
  # static datasets
  land_grant = process_land_grant(),
  soils = process_soils(),
  static = bind_rows(land_grant, soils),
  
  # climate data (complex temporal structure)
  climate = process_climate()
)