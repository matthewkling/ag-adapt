
process_soils <- function(){
  
  ## cultivated land ##################
  
  # # NASS national cultivated lands layer, from 
  # # https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php
  # cult <- raster("f:/NASS/2020_Cultivated_Layer/2020_Cultivated_Layer.img") %>%
  #   raster::aggregate(fact = 300) %>%
  #   reclassify(c(-Inf, 1, NA), right = F) %>%
  #   "-"(1)
  # writeRaster(cult, "data/cultivated_lands_agg300.tif", overwrite = T)
  # cult <- raster("data/cultivated_lands_agg300.tif")
  # 
  # # define target spatial grid
  # ext <- extent(cult) %>% "*"(1.5) %>% as("SpatialPolygons")
  # crs(ext) <- crs(cult)
  # template <- raster("f:/chelsa/v2/climatology/CHELSA_bio5_1981-2010_V.2.1.tif")
  # ext <- spTransform(ext, crs(template))
  # template <- crop(template, ext)
  # 
  # culp <- projectRaster(cult, template)
  # writeRaster(culp, "data/ignore/raster/cultivated_lands_agg300_chelsaproj.tif", overwrite = T)
  culp <- raster("data/ignore/raster/cultivated_lands_agg300_chelsaproj.tif")
  
  
  ## counties ###################
  
  # counties <- readOGR("data/Counties/cb_2018_us_county_500k.shp")
  # counties$FP <- as.integer(paste0(counties$STATEFP, counties$COUNTYFP))
  # counties <- spTransform(counties, crs(culp))
  # counties <- counties %>% st_as_sf() %>% fasterize(culp, field = "FP")
  # writeRaster(counties, "data/ignore/raster/counties_chelsaproj.tif", overwrite = T)
  counties <- raster("data/ignore/raster/counties_chelsaproj.tif")
  
  
  ## soils ###############
  
  # cf <- list.files("f:/SoilGrids/v1", full.names = T, pattern = "tif")
  # clay <- stack(cf[!grepl("xml", cf)])
  # ext <- extent(culp) %>% "*"(1.5) %>% as("SpatialPolygons")
  # crs(ext) <- crs(culp)
  # ext <- spTransform(ext, crs(clay))
  # clay <- clay %>% 
  #   crop(ext) %>% 
  #   mean() %>% 
  #   aggregate(4) %>% 
  #   projectRaster(culp)
  # writeRaster(clay, "data/ignore/raster/clay_chelsaproj.tif", overwrite = T)
  clay <- raster("data/ignore/raster/clay_chelsaproj.tif")
  
  
  ## combine and summarize ###########
  
  s <- stack(counties, culp, clay) %>%
    setNames(c("county_fips", "prop_cult", "pct_clay"))
  
  d <- cbind(coordinates(s), values(s)) %>%
    as.data.frame() %>% as_tibble() %>%
    na.omit() %>%
    group_by(county_fips) %>%
    summarize(pct_clay = weighted.mean(pct_clay, prop_cult)) %>%
    ungroup() %>%
    mutate(fips = str_pad(county_fips, 5, "left", "0")) %>%
    select(-county_fips)
  
  return(d)
}