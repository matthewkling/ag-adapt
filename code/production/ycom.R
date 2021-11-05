
process_ycom <- function(){
  
  # load YCOM survey data
  files <- c(y2014 = "data/YCOM/YPCCC_County_n28_2015Jan5.csv",
             y2016 = "data/YCOM/YCOM_2016_Data.01.csv",
             y2018 = "data/YCOM/YCOM_2018_Data.csv",
             y2020 = "data/YCOM/YCOM_2020_Data.csv")
  d <- map(files, read_csv)
  
  # create standardized FIPS variable across survey years
  d$y2014 <- d$y2014 %>% 
    mutate(fips = Countycode)
  d$y2016 <- d$y2016 %>% 
    filter(GeoType == "County") %>%
    mutate(fips = GEOID)
  d$y2018 <- d$y2018 %>% 
    filter(GeoType == "County") %>%
    left_join(select(d$y2016, fips = GEOID, GeoName))
  d$y2020 <- d$y2020 %>% 
    filter(GeoType == "County") %>%
    mutate(fips = GEOID)
  
  # select the variables common to all survey years
  md <- map(names(d), function(x){
    data.frame(year = as.integer(str_remove(x, "y")),
               var = names(d[[x]])) %>%
      spread(var, year)
  }) %>%
    bind_rows() %>%
    t() %>%
    as.data.frame() %>%
    setNames(names(d)) %>%
    mutate_all(function(x) as.integer(!is.na(x))) %>%
    rownames_to_column("variable")
  vars <- md %>% 
    filter(y2014 + y2016 + y2018 + y2020 == 4) %>%
    pull(variable)
  d <- map(d, select, all_of(vars))
  
  # add year as additional variable
  d <- map(1:4, function(i) mutate(d[[i]], year = c(2014, 2016, 2018, 2020)[i]))
  
  # combine years and restructure
  d <- bind_rows(d) %>% 
    janitor::clean_names() %>%
    filter(!is.na(fips)) %>%
    mutate(fips = str_pad(fips, 5, "left", "0")) %>%
    gather(variable, value, -year, -fips)
  
  return(d)
}
