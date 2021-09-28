

process_land_grant <- function(){
  # code by Antonio
  
  #------- GETTING THE DATA -------- 
  universities <- read_xlsx("data/land_grant/universities.xlsx") %>%
    filter(type == 1862)
  universities <- st_as_sf(universities, coords=c("long", "lat"), crs = 4326) 
  universities <- st_transform(universities, 26919)
  
  counties <- st_read("data/Counties/cb_2018_us_county_500k.shp") %>% 
    select(STATEFP,
           COUNTYFP,
           fips = GEOID, 
           geometry) %>% 
    # right_join(entire_dataset %>% 
    #              select(fips, county),
    #            by="fips") %>%
    group_by(fips) %>% 
    filter(row_number() == 1) %>%
    arrange(fips)
  
  counties <- st_transform(counties, 26919)
  
  #------- CALCULATING DISTANCES -------- 
  county_centroids <- st_centroid(counties)
  
  distances <- bind_cols(st_set_geometry(county_centroids, NULL),
                         st_distance(county_centroids, universities) %>% 
                           as_tibble())
  
  names(distances) <- st_set_geometry(universities, NULL) %>% 
    select(university) %>% 
    unlist() %>%
    c("STATEFP", "COUNTYFP", "fips"#, "county"
      , .)
  
  distances <- distances %>%
    pivot_longer(cols=4:61, names_to="university", values_to="distance") %>%
    left_join(st_set_geometry(universities, NULL) %>%
                select(university, fips),
              by = "university",
              suffix=c("_county", "_college")) %>%
    mutate(distance = as.numeric(distance),
           fips_college = str_trunc(fips_college, 2, ellipsis=""))
  
  output1 <- distances %>%
    group_by(fips_county) %>%
    mutate(closest_overall = min(distance)) %>%
    ungroup() %>% 
    filter(distance==closest_overall) %>%
    arrange(fips_county)
  
  output2 <- map(unique(distances$STATEFP), function(x) {
    distances %>%
      filter(STATEFP == x,
             fips_college == x) %>%
      # group_by(county) %>%
      group_by(STATEFP, COUNTYFP) %>%
      mutate(closest_in_state = min(distance)) %>%
      ungroup() %>% 
      filter(closest_in_state == distance)
  }) %>%
    bind_rows()
  
  land_grant <- output1 %>%
    select(fips = fips_county, university, closest_overall) %>%
    full_join(output2 %>%
                select(fips = fips_county, university, closest_in_state),
              by="fips",
              suffix=c("_overall", "_in_state"))
  
  land_grant <- land_grant[!duplicated(land_grant), ]
  
  land_grant <- land_grant %>%
    left_join(st_set_geometry(universities, NULL) %>% 
                select(fips) %>%
                mutate(university_presence = fips),
              by = "fips") %>%
    mutate(university_presence = ifelse(fips==university_presence, 1, 0),
           university_presence = as.numeric(university_presence),
           university_presence = ifelse(is.na(university_presence), 0, 1))
  
  land_grant <- land_grant %>%
    mutate(closest_in_state = round(closest_in_state * 0.00062137, 2),
           closest_overall = round(closest_overall * 0.00062137, 2))
  
  land_grant <- land_grant %>%
    gather(variable, value, -fips)
  
  return(land_grant)
}
