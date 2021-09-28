

get_adaptation_2012 <- function(){
  
  land_practices <- "data/nass/nass_census_2012_practices.xlsx"
  
  p <- #setdiff(excel_sheets(path=land_practices), c("US", "ALASKA", "HAWAII")) %>%
    excel_sheets(path=land_practices) %>%
    lapply(function(x) {read_excel(path=land_practices, sheet=x)}) %>%
    lapply(function(x) {x["DataValue"] <- lapply(x["DataValue"], as.character); x}) %>% 
    bind_rows() %>%
    mutate(COUNTY_CODE = str_pad(CountyFips, width=3, side="left", pad="0"),
           COUNTY_CODE = ifelse(COUNTY_CODE == "000", "NULL", COUNTY_CODE),
           AGG_LEVEL_DESC = case_when(StateCountyName == "US" ~ "NATIONAL",
                                      COUNTY_CODE == "NULL" ~ "STATE",
                                      TRUE ~ "COUNTY"),
           STATE_FIPS_CODE = case_when(StateFips == "US" ~ 99,
                                       TRUE ~ StateFips),
           DOMAINCAT_DESC = "",
           CENSUS_CHAPTER = 99, 
           CENSUS_TABLE = 99) %>%
    filter(STATE_FIPS_CODE != 0) %>%
    select(CENSUS_CHAPTER, 
           CENSUS_TABLE,
           AGG_LEVEL_DESC,
           STATE_FIPS_CODE,
           COUNTY_CODE,
           SHORT_DESC = DataItem,
           DOMAINCAT_DESC,
           VALUE = DataValue)
  p
}




# fill missing values via weighted allocation from parent political regions
fill_missing <- function(x, w, level, parent = "STATE", child = "COUNTY", 
                         stat = "sum"){
  if(!any(is.na(x[level == child]))) return(x)
  if(stat == "mean") x <- x * w
  y <- x
  px <- x[level == parent]
  cx <- x[level == child]
  cw <- w[level == child]
  ux <- px - sum(cx, na.rm = T)
  ui <- which(is.na(cx))
  cx[ui] <- ux * cw[ui] / sum(cw[ui])
  y[level == child] <- cx
  y[level == parent] <- px
  if(stat == "mean"){
    y <- y / w
    y[w == 0] <- 0
  }
  return(y)
}

# test that fill_missing works as expected
test_fill <- function(){
  ops <- 1:100
  acres <- rnorm(100, 100, 30)
  
  sum_acres <- sum(acres)
  sum_ops <- sum(ops)
  
  missing <- sample(ops, 5) 
  acres0 <- acres
  acres0[missing] <- NA
  
  y1 <- fill_missing(c(sum_acres, acres0),
                     c(sum_ops, ops),
                     c("STATE", rep("COUNTY", 100)))[2:101]
  
  
  acres_per_op <- acres / ops
  acres_per_op0 <- acres_per_op
  acres_per_op0[missing] <- NA
  mean_acres_per_op <- sum_acres / sum_ops
  
  y2 <- fill_missing(c(mean_acres_per_op, acres_per_op0),
                     c(sum_ops, ops),
                     c("STATE", rep("COUNTY", 100)),
                     stat = "mean")[2:101]
  
  if(sum(y1) != sum_acres) stop("failure")
  if(weighted.mean(y2, w) != mean_acres_per_op) stop("failure")
  if(mean(y1 / w) != mean(y2)) stop("failure")
  if(round(sum(y1) / sum(w), 5) != round(weighted.mean(y2, w), 5)) stop("failure")
  
  message("success")
}
# test_fill()



process_ag_census <- function(){
  
  ## data import ######################
  
  # "https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Census_Data_Query_Tool/2012_cdqt_data.txt.gz"
  # "https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Census_Data_Query_Tool/2017_cdqt_data.txt.gz"
  census_2012 <- read.delim("data/nass/2012_cdqt_data.txt", header = TRUE) %>%
    bind_rows(get_adaptation_2012())
  census_2017 <- read.delim("data/nass/2017_cdqt_data.txt", header = TRUE)
  
  # load metadata on variables to import
  md <- read_xlsx("data/nass/variables.xlsx") %>%
    mutate(domaincat_desc = ifelse(is.na(domaincat_desc), "", domaincat_desc))
  
  # filter data to selected variables
  get_var <- function(i, vars, d, year){
    v <- vars[i,]
    if(!is.na(v$year) & v$year != year) return(data.frame())
    d %>%
      filter(CENSUS_CHAPTER == v$chapter,
             CENSUS_TABLE == v$table,
             SHORT_DESC == v$short_desc, 
             DOMAINCAT_DESC == v$domaincat_desc) %>%
      mutate(name = v$name,
             year = year)
  }
  d <- bind_rows(map_dfr(1:nrow(md), get_var, vars = md, d = census_2012, year = 2012),
                 map_dfr(1:nrow(md), get_var, vars = md, d = census_2017, year = 2017)) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(value = str_remove_all(value, "\\,")) %>%
    select(agg = agg_level_desc, state_fips_code, county_code, name, year, value)
  
  
  ## missing values ################
  
  # convert implicit missing to explicit missing
  d <- d %>% select(agg, state_fips_code, county_code) %>%
    distinct() %>%
    expand_grid(name = unique(d$name), year = unique(d$year)) %>%
    full_join(d, .)
  
  # convert values to numeric, handling missing codes
  to_numeric <- function(x){
    x[is.na(x)] <- "0"
    x[x == "(Z)"] <- "0"
    x[x == "(D)"] <- NA
    x <- as.numeric(x)
    x
  }
  d <- d %>% mutate(value = to_numeric(value))
  
  
  # fill state from national, and then county from state
  fill <- function(var, year, stat, wvar, data){
    # if(var == "tile_drainage_acres_per_op") browser()
    y <- data %>% 
      filter(name %in% na.omit(c(var, wvar)))
    if(is.na(wvar)) return(y %>% mutate(value_imputed = value))
    y <- y %>% 
      mutate(name = case_when(name == var ~ "value",
                              name == wvar ~ "weight")) %>%
      spread(name, value) %>%
      group_by(year) %>%
      mutate(value_imputed = 
               fill_missing(value, weight, stat = stat, 
                            level = agg, parent = "NATIONAL", child = "STATE")) %>%
      group_by(state_fips_code, year) %>%
      mutate(value_imputed = 
               fill_missing(value_imputed, weight, stat = stat, 
                            level = agg, parent = "STATE", child = "COUNTY")) %>%
      mutate(name = var) %>%
      select(-weight)
    return(y)
  }
  
  d <- md %>%
    select(var = name, stat = statistic, wvar = fill_weight) %>%
    distinct() %>%
    pmap_df(fill, data = d) %>%
    filter(agg == "COUNTY") %>%
    select(-agg)
  
  
  ##  aggregate variables ###############
  
  d <- d %>%
    left_join(md %>% select(name, parent)) %>%
    mutate(name = ifelse(is.na(parent), name, parent)) %>%
    select(-parent) %>%
    group_by(state_fips_code, county_code, name, year) %>%
    mutate(value = sum(value)) # works because all parent vars are sums, not means
  
  
  ## final reformatting for downstream compatibility ####################
  
  d <- clean_fips(d, "state_fips_code", "county_code") %>%
    rename(variable = name,
           value_raw = value,
           value = value_imputed)
  
  return(d)
}