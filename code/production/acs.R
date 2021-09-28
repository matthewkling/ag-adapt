
clean_fips <- function(data, state = "STATE_FIPS_CODE", county = "COUNTY_CODE"){
  data$fips <- paste0(str_pad(data[[state]], 2, "left", "0"),
                       str_pad(data[[county]], 3, "left", "0"))
  data <- data[, setdiff(names(data), c(state, county))]
  return(data)
  # data %>%
  #   unite("fips", 
  #         c(STATE_FIPS_CODE, COUNTY_CODE), 
  #         sep="", 
  #         remove=TRUE) %>%
  #   mutate(fips = str_pad(fips, 
  #                         width=5,
  #                         side="left",
  #                         pad="0"))
}


process_acs <- function(){
  # code by Antonio
  
  `%notin%` <- Negate(`%in%`)
  
  api.key.install("349f048c79fdba24fa2d6cef80a5e4443d72f2af")
  
  #------- POPULATION -------- 
  population <- read_xls("data/nass/acs_population.xls", skip=2)
  
  non_contiguous <- c("AK", "HI", "PR", "US")
  
  population <- population %>%
    select("fips" = "FIPStxt", 
           "state" = "State", 
           "county" = "Area_Name", 
           "population_2012" = "POP_ESTIMATE_2012", 
           "population_2017" = "POP_ESTIMATE_2017") %>%
    filter(!str_detect(fips, "000$"),
           state %notin% non_contiguous) %>%
    pivot_longer(4:5, 
                 names_to="year",
                 values_to="population") %>%
    mutate(county = str_remove_all(county, " County"),
           year = as.numeric(str_remove_all(year, "population_")))
  
  #------- EDUCATION -------- 
  variables <- c("STATE_FIPS_CODE", 
                 "COUNTY_CODE",
                 "high_school",
                 "GED_or_alternative",
                 "some_college_under_one_year",
                 "some_college_over_one_year",
                 "associates",
                 "bachelors",
                 "masters",
                 "professional",
                 "doctorate")
  
  education <- acs.fetch(endyear = 2017, 
                         geography = geo.make(state = "*",
                                              county = "*"),
                         table.number = "B15003",
                         dataset = "acs",
                         span = "5",
                         variable = c("B15003_017", "B15003_018", "B15003_019",
                                      "B15003_020", "B15003_021", "B15003_022", 
                                      "B15003_023", "B15003_024", "B15003_025"))
  
  education_2017 <- bind_cols(education@geography$state,
                              education@geography$county,
                              education@estimate %>% as_tibble())
  
  names(education_2017) <- variables
  
  education_2017 <- clean_fips(education_2017) %>%
    mutate(year = 2017)
  
  education <- acs.fetch(endyear = 2012, 
                         geography = geo.make(state = "*",
                                              county = "*"),
                         table.number = "B15003",
                         dataset = "acs",
                         span = "5",
                         variable = c("B15003_017", "B15003_018", "B15003_019",
                                      "B15003_020", "B15003_021", "B15003_022", 
                                      "B15003_023", "B15003_024", "B15003_025"))
  
  education_2012 <- bind_cols(education@geography$state,
                              education@geography$county,
                              education@estimate %>% as_tibble())
  
  names(education_2012) <- variables
  
  education_2012 <- clean_fips(education_2012) %>%
    mutate(year = 2012)
  
  #------- COMBINED -------- 
  entire_dataset <- population %>%
    left_join(bind_rows(education_2012, education_2017),
              by=c("fips", "year"))
  
  acs <- entire_dataset %>%
    mutate(high_school_or_GED = high_school + 
             GED_or_alternative,
           some_college_or_associates = some_college_under_one_year + 
             some_college_over_one_year +
             associates,
           bachelors_or_higher = bachelors +
             masters +
             professional +
             doctorate) %>%
    select(fips, state, county, year, population,
           high_school_or_GED, some_college_or_associates,
           bachelors_or_higher) %>%
    mutate(high_school_or_GED = high_school_or_GED/population,
           some_college_or_associates = some_college_or_associates/population,
           bachelors_or_higher = bachelors_or_higher/population)
  
  acs <- acs %>%
    gather(variable, value, population:bachelors_or_higher) %>%
    select(-state, -county)
  
  return(acs)
}
