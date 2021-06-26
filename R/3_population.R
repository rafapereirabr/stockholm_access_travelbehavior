
# source
# url: https://www.worldpop.org/geodata/summary?id=49886
# Recommended citation: Bondarenko M., Kerr D., Sorichetta A., and Tatem, A.J. 2020. Census/projection-disaggregated gridded population datasets for 189 countries in 2020 using Built-Settlement Growth Model (BSGM) outputs. WorldPop, University of Southampton, UK. doi:10.5258/SOTON/WP00684

##################### Load packages -------------------------------------------------

source("./R/1_LoadPackages.R")

library(sp)
library(raster)
library(stars)


# read national population data
pop <- raster::raster('./data-raw/population/swe_ppp_2020_constrained.tif')
pop <- st_transform(pop, 4674)


# function to extract pop data at high resolution of each metro area
get_metro_pop <- function(city){ # city <- 'mal'
  
# read administrative boundaries
metro <- readr::read_rds(file = paste0('./data/shapes/metro_',city,'.rds'))
  
# crop with area of the metro region
pop_city <- crop(pop, metro)

# convert to spatial vectorized data sf
pop_city_sf <- st_as_stars(pop_city)
pop_city_sf <- st_as_sf(pop_city_sf, as_points = FALSE, merge = TRUE)
head(pop_city_sf)
summary(pop_city_sf$swe_ppp_2020_constrained)

# mapview(pop_city_sf)

# add meto name
pop_city_sf$metro <- city

# Save metro areas Shape File
readr::write_rds(pop_city_sf, file = paste0('./data/shapes/pop_metro_',city,'.rds'))
sf::st_write(pop_city_sf, dsn= paste0('./data/shapes/pop_metro_',city,'.gpkg'))

}

# apply function
pblapply(X = c('mal', 'got', 'sto'), FUN=get_metro_pop)


a <- readr::read_rds(file = paste0('./data/shapes/pop_metro_',city,'.rds'))

