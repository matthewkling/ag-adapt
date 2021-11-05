

compile_dictionary <- function(acs, land_grant, soils, climate){
  
  
  ## ACS ################
  
  dict <- tribble(
    ~variable, ~unit, ~desc,
    "population", "people", "total county population",
    "high_school_or_GED", "proportion", "proportion of county population with HS/GED as highest degree",
    "some_college_or_associates", "proportion", "proportion of county population with some college or associates degree as highest degree",
    "bachelors_or_higher", "proportion", "proportion of county population with bachelor's degree or higher as highest degree")
  
  acs <- acs %>%
    group_by(variable) %>%
    summarize(era = paste(unique(year), collapse = ", ")) %>%
    distinct() %>%
    left_join(dict) %>%
    mutate(source_dataset = "American Community Survey")
  
  
  ## CLIMATE ##########
  
  dict <- tribble(
    ~variable, ~unit, ~desc,
    "tmin", "deg C", "average daily minimum temperature",
    "tmean", "deg C", "average daily mean temperature",
    "tmax", "deg C", "average daily maximum temperature",
    "prec", "mm", "total precipitation",
    "prec1in", "days","number of days with more than 1 inch of precipitation",
    "tmax30c", "days","number of days tmax greater than 30 deg C",
    "gdd", "degree days", "degree days between 5 and  29 deg C",
    "edd", "degree days", "degree days above 29 deg C",
    "ffp", "days", "length of frost-free period",
    "pet", "mm", "potential evapotranspiration",
    "aet", "mm", "actual evapotranspiration",
    "cwd", "mm", "climatic water deficit",
    "prec1pct", "days", "number of days in the top 1% of local daily precipitation distribution",
    "tmax1pct", "days", "number of days in the top 1% of local daily maximum temperature distribution",
    "prec2sigma", "days", "number of days with precipitation exceeding 2 local standard deviations",
    "tmax2sigma", "days", "number of days with maximum temperature exceeding 2 local standard deviations")
  
  clim <- climate %>% ungroup() %>% 
    filter(fips == fips[1]) %>%
    select(-fips) %>%
    gather(variable, value, -season, -start, -end) %>%
    unite(era, start, end, sep = "-") %>%
    select(variable, era, season) %>%
    separate(variable, c("variable", "statistic"), "_") %>%
    mutate(statistic = ifelse(is.na(statistic), "mean", "z")) %>%
    left_join(dict) %>%
    group_by(variable, unit, desc) %>%
    summarize(era = paste(unique(era), collapse = ", "),
              season = paste(unique(season), collapse = ", "),
              statistic = paste(unique(statistic), collapse = ", ")) %>%
    mutate(source_dataset = "Schlenker 2009")

  
  ## ag census ############
  
  ag_census <- read_xlsx("data/nass/variables.xlsx") %>%
    mutate(domaincat_desc = ifelse(is.na(domaincat_desc), "", domaincat_desc)) %>%
    mutate(desc = paste(short_desc, domaincat_desc) %>% str_trim()) %>%
    mutate(name = ifelse(is.na(parent), name, parent)) %>%
    group_by(name) %>%
    summarize(unit = unit[1],
              desc = paste(desc, collapse = " + ")) %>%
    select(variable = name, unit, desc) %>%
    distinct() %>%
    mutate(era = "2012, 2017") %>%
    mutate(source_dataset = "NASS Ag Census")
  
  
  ## land grant ###########
  
  dict <- tribble(
    ~variable, ~unit, ~desc,
    "closest_in_state", "km", "distance from county centroid to closest in-state land grant university",
    "closest_overall", "km", "distance from county centroid to closest land grant university",
    "university_in_state", "name", "name of closest in-state land grant university",
    "university_overall", "name", "name of closest land grant university",
    "university_presence", "boolean", "presence of land grant university in county")
  
  land_grant <- land_grant %>%
    select(variable) %>%
    distinct() %>%
    left_join(dict) %>%
    mutate(source_dataset = "land grant university datasets")
  
  
  ## soils ################
  
  dict <- tribble(
    ~variable, ~unit, ~desc,
    "pct_clay", "proportion", "average clay content of cultivated land")
    
  soils <- soils %>%
    select(-fips) %>%
    gather(variable, value) %>%
    select(variable) %>%
    distinct() %>%
    left_join(dict) %>%
    mutate(source_dataset = "SoilGrids")
  
  
  ## YCOM ##################
  
  dict <- read_csv("data/YCOM/YCOM_2020_Metadata.csv") %>%
    rename(variable = YCOM.VARIABLE.NAME,
           desc = VARIABLE.DESCRIPTION) %>%
    spread(variable, desc) %>%
    janitor::clean_names() %>%
    gather(variable, desc)
  
  ycom <- ycom %>%
    select(variable) %>%
    distinct() %>%
    left_join(dict) %>%
    mutate(source_dataset = "YCOM")
  
  
  ## combined #############
  
  d <- bind_rows(ag_census, acs, land_grant, soils, clim, ycom) %>%
    select(source_dataset, variable, unit, description = desc, era, season, statistic) %>%
    mutate(description = str_to_lower(description))
  
  return(d)
}




export_outputs <- function(acs, land_grant, soils, ag_census, climate, ycom){
  
  dict <- compile_dictionary(acs, land_grant, soils, climate) %>%
    write_csv("data/output/variables.csv")
  
  bind_rows(acs %>% mutate(source_dataset = "ACS"), 
            ycom %>% mutate(source_dataset = "YCOM"), 
            ag_census %>% mutate(source_dataset = "AgCensus")) %>%
    select(source_dataset, variable, fips, year, value, raw, imputed, floor, ceiling) %>%
    write_csv("data/output/time_series_data.csv")
  
  soils %>%
    gather(variable, value, -fips) %>%
    mutate(value = as.character(value)) %>%
    bind_rows(land_grant) %>%
    write_csv("data/output/static_data.csv")
  
  climate  %>%
    write_csv("data/output/climate_data.csv")
  
  list(dict = dict, 
       ts = ts, 
       static = static, 
       climate = climate)
}

