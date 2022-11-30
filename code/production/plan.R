
plan <- drake_plan(
  
  acs = process_acs(),
  ag_census = process_ag_census(),
  land_grant = process_land_grant(),
  soils = process_soils(),
  climate = process_climate(),
  ycom = process_ycom(),
  rma = process_rma(),
  
  results = export_outputs(acs, land_grant, soils, ag_census, climate, ycom)
)