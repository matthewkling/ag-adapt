
# degree days, sine method, lower threshold only
degree_days <- function(tmin, tmax, threshold){
  gd <- function(tn, tx, th){
    p <- (th - tn) / (tx - tn) # proportion of way from tmin to tmax
    x <- suppressWarnings(- acos(2 * p - 1)) # radian distance thru cos curve from -pi:0
    i <- (pi + x + sin(x)) / pi # integral of cos curve from -pi to x
    j <- - x * 2 * p / pi # integral below threshold, from x to 0
    pos <- 1 - i - j # integral above threshold, from x to 0
    (tx - th) * pos / 2
  }
  case_when(threshold >= tmax ~ 0,
            threshold <= tmin ~ ((tmin + tmax)/2 - threshold),
            TRUE ~ gd(tmin, tmax, threshold))
}

# frost-free season
ffs <- function(tmin, end = c("start", "end"), threshold = 0){
  if(! length(tmin) %in% 365:366) stop("input must represent one whole year")
  
  # first day of frost-free season
  if(end == "start"){
    day <- 184 - which(tmin[184:1] < threshold)[1] + 2
    return(ifelse(is.na(day), 0, day))
  }
  
  # last day of frost-free season
  if(end == "end"){
    day <- which(tmin[185:length(tmin)] < threshold)[1] + 184 - 1
    return(ifelse(is.na(day), 365, day))
  }
}


county_climate <- function(fips = 1001, dirs, metadata){
  
  
  ## daily variables ################
  
  # faster to get year from filename than from date field
  read_dta_year <- function(x){
    y <- dirname(x) %>% gsub("fips|\\.dta", "", .) %>% 
      substr(nchar(.) - 3, nchar(.)) %>% as.integer()
    read_dta(x) %>% mutate(year = y)
  }
  
  # county centroid latitude, needed for aridity measures
  latitude <- metadata$latitude[metadata$fips == fips][1]
  
  # daily climate variables
  daily <- dirs %>% 
    paste0("/fips", fips, ".dta") %>% 
    map_df(read_dta_year) %>%
    setNames(tolower(names(.))) %>%
    
    mutate(tmean = (tmin + tmax) / 2,
           prec1in = prec > 25.4, # 25.4 mm = 1 inch
           tmax30c = tmax > 30,
           edd = degree_days(tmin, tmax, 29),
           gdd = degree_days(tmin, tmax, 5) - edd) %>%
    
    group_by(gridnumber, year) %>%
    arrange(datenum) %>%
    mutate(jday = 1:length(datenum),
           gs = between(jday, ffs(tmin, "start"), ffs(tmin, "end"))) %>%
    
    group_by(gridnumber) %>%
    arrange(datenum) %>%
    mutate(pet = hargreaves(S0 = ETSR(latitude, jday), 
                            tmin = tmin, tmean = tmean, tmax = tmax),
           prec5d = slide_dbl(prec, mean, .before = 4, .after = 0),
           aet = pmin(pet, prec5d),
           cwd = pet - aet) %>%
    
    ungroup() %>%
    select(-datenum, -jday, -prec5d)
  
  daily <- bind_rows(daily %>% mutate(season = "full_year"),
                     daily %>% filter(gs) %>% mutate(season = "growing_season"))
  
  
  ## daily anomalies ######
  
  ref_start <- 1982
  ref_end <- 2011
  
  sigma <- function(x, ref) (x - mean(x[ref])) / sd(x[ref])
  percentile <- function(x, ref, p) x > quantile(x[ref], p)
  
  daily <- daily %>%
    group_by(gridnumber, season) %>%
    mutate(ref_year = between(year, ref_start, ref_end),
           prec1pct = percentile(prec, ref_year, .99),
           tmax1pct = percentile(tmax, ref_year, .99),
           prec2sigma = sigma(prec, ref_year) >= 2,
           tmax2sigma = sigma(tmax, ref_year) >= 2)
  
  
  ## annual variables ###########
  
  annualize <- function(x){
    x %>%
      group_by(gridnumber, year, ref_year, season) %>%
      summarize(tmin = mean(tmin),
                tmean = mean(tmean),
                tmax = mean(tmax),
                prec = sum(prec),
                prec1in = sum(prec1in),
                tmax30c = sum(tmax30c),
                gdd = sum(gdd),
                edd = sum(edd),
                ffp = sum(gs),
                pet = sum(aet),
                aet = sum(aet),
                cwd = sum(cwd),
                prec1pct = sum(prec1pct),
                tmax1pct = sum(tmax1pct),
                prec2sigma = sum(prec2sigma),
                tmax2sigma = sum(tmax2sigma)) %>%
      
      group_by(gridnumber, season) %>%
      mutate_at(vars(-gridnumber, -year, -ref_year, -season),
                list(z = ~ sigma(., ref_year))) %>%
      ungroup()
  }
  annual <- daily %>% annualize()
  
  
  ## climatologies ##########
  
  climatize <- function(x, start, end){
    x %>% 
      filter(between(year, start, end)) %>% 
      select(-year, -ref_year) %>%
      group_by(gridnumber, season) %>% 
      summarize_all(mean) %>%
      mutate(start = start, end = end)
  }
  
  climatology <- tibble(start = c(1950, 1982, 2007, 2012),
                        end =   c(1980, 2011, 2011, 2016)) %>%
    pmap_dfr(climatize, x = annual)
  
  
  ## county means #################
  
  climatology %>%
    left_join(metadata) %>%
    group_by(fips, season, start, end) %>%
    summarize_at(vars(tmin:tmax2sigma_z),
                 list(~ weighted.mean(., w = croparea)))
  
}


process_climate <- function(){
  
  cell_fips <- read_dta("f:/schlenker/metadata/linkgridnumberFIPS.dta")
  
  crop_area <- read_dta("f:/schlenker/metadata/cropArea.dta")
  
  lat <- st_read("data/Counties/cb_2018_us_county_500k.shp") %>%
    st_centroid() %>%
    bind_cols(as.data.frame(st_coordinates(.))) %>%
    mutate(fips = paste0(STATEFP, COUNTYFP) %>% as.integer())
  lat <- tibble(fips = lat$fips, latitude = lat$Y)
  
  metadata <- cell_fips %>%
    left_join(crop_area) %>%
    left_join(lat) %>%
    setNames(tolower(names(.)))
  fipses <- unique(metadata$fips)
  
  dirs <- list.dirs("f:/schlenker/data_v2020", full.names = T)
  dirs <- dirs[2:length(dirs)]
  
  plan(multisession, workers = 4) # the main bottleneck is disk reads, not cpus
  d <- fipses %>%
    future_map_dfr(county_climate, dirs = dirs, metadata = metadata,
                   .progress = T, 
                   .options = furrr_options(seed = NULL))
  
  write_csv(d, "data/ignore/climate.csv")
  
  return(d)
}

process_climate2 <- function() read_csv("data/ignore/climate.csv")


