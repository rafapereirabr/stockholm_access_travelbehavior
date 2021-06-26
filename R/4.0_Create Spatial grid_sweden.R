

#This script:
###### 1: Load shape file of Metro areas
###### 2: create Function to create hexagonal grid
###### 3: Create Hexagonal grid
###### 4: Create point file to input in Open Trip Planner



###### Load packages -------------------------------------------------
source("./R/1_LoadPackages.R")


# Assign projection in UTM \\\\\ http://spatialreference.org/ref/epsg/wgs-84-utm-zone-33n/
myprojection_utm <- CRS("+proj=utm +units=m +zone=33N +ellps=WGS84 +datum=WGS84")
# myprojection_latlong <- CRS("+proj=longlat +zone=33N +ellps=WGS84 +datum=WGS84")
myprojection_latlong <- CRS("+init=epsg:4326 +proj=longlat +zone=33N +ellps=WGS84 ")


  



# function to get total area
save_hex <- function(city){     # city <- 'got'
  
  message(paste0(city, '\n generating total area'))
  
  #  1 Load shape file of Metro areas  and Land use
  # 1.1 administrative boundaries  ----------
  metro <- readr::read_rds(file = paste0('./data/shapes/metro_',city,'.rds'))
  
  
  # 1.1 urban areas ----------
  urbanmetro <- readr::read_rds(file = paste0('./data/shapes/urbanmetro_',city,'.rds'))
  urbanmetro <- dplyr::select(urbanmetro, metro, geometry)
  
  
  # 1.1 populated area ----------
  pop <- readr::read_rds(file = paste0('./data/shapes/pop_metro_',city,'.rds'))
  popborder <- pop %>% group_by(metro) %>% summarise() # dissolve borders
  
  
  # 1.1 read water areas from OSM ----------
  if(city=='got'){water <- st_read(dsn="./data-raw/shape_files/osm_got/ex_a7u54QQXsSMfxxqukvX7BAs25a37P_osm_waterareas.shp")}
  if(city=='mal'){water <- st_read(dsn="./data-raw/shape_files/osm_mal/ex_ZZRn8qVJJN8XjM5ceeyaAZEjrffK2_osm_waterareas.shp")}
  if(city=='sto'){water <- st_read(dsn="./data-raw/shape_files/osm_sto/edited_osm_waterareas.shp")}
  
  
  # 1.1 coast areas from OSM  ----------
  if(city=='got'){coast <- st_read(dsn="./data-raw/shape_files/sweden.water.coastline/gothenburg_sweden_water_coast.shp")}
  if(city=='mal'){coast <- st_read(dsn="./data-raw/shape_files/sweden.water.coastline/malmo_sweden_water_coast.shp")}
  if(city=='sto'){coast <- st_read(dsn="./data-raw/shape_files/sweden.water.coastline/stockholm_sweden_water_coast.shp")}
  
  # # read land use data from OSM
  #   if(city=='got'){landuse <- st_read(dsn="./data-raw/shape_files/osm_got/ex_a7u54QQXsSMfxxqukvX7BAs25a37P_osm_landusages.shp")}
  #   if(city=='sto'){landuse <- st_read(dsn="./data-raw/shape_files/osm_sto/ex_awZwFeZi14sKHjWUMUwTYrCAt3bYy_osm_landusages.shp")}
  #   if(city=='mal'){landuse <- st_read(dsn="./data-raw/shape_files/osm_mal/ex_ZZRn8qVJJN8XjM5ceeyaAZEjrffK2_osm_landusages.shp")}
  #   plot(landuse_mal)
  
  
  # 1.1 grocery_stores data  ----------
  grocery_stores <- st_read(dsn="./data-raw/grocery_stores2015/shape/Dagligvarubutiker2015.shp")
  
  
  # 1.1 Geolocated Hospitals  ----------
  health_centre <- readxl::read_excel(path = "./data-raw/health_centre/health_centre_coord.xlsx", sheet = city) %>% setDT()
  health_centre[, Latitude := as.numeric(Latitude)][, Longitude := as.numeric(Longitude)]
  health_centre <- sfheaders::sf_point(health_centre, x = 'Longitude', y='Latitude', keep = T)
  st_crs(health_centre) <- myprojection_latlong
  

    
  
  # 2. Assign projection  --------------
  metro <- st_transform(metro, crs = myprojection_latlong) 
  water <- st_transform(water, crs = myprojection_latlong) 
  coast <- st_transform(coast, crs = myprojection_latlong)
  popborder <- st_transform(popborder, crs = myprojection_latlong)
  urbanmetro <- st_transform(urbanmetro, crs = myprojection_latlong) 
  health_centre <- st_transform(health_centre, crs = myprojection_latlong) 
  grocery_stores <- st_transform(grocery_stores, crs = myprojection_latlong) 
  gc(reset = T)
  
  
  ## 3. buffer and crop point destinations  --------------
  #buffer 
  health_centre <- st_buffer(health_centre, dist = .001)
  grocery_stores <- st_buffer(grocery_stores, dist = .001)
  #crop
  health_centre <- health_centre[ metro,]
  grocery_stores <- grocery_stores[ metro,]
  # merge all buffers
  grocery_stores$type <- 'grocery'
  health_centre$type <- 'health'
  grocery_stores <- grocery_stores %>% group_by(type) %>% summarise()
  health_centre <- health_centre %>% group_by(type) %>% summarise()
  
  ggplot() +
    geom_sf(data=metro, fill='gray90') +
    geom_sf(data=urbanmetro, fill='gray80') +
    geom_sf(data=grocery_stores, color='blue') +
    geom_sf(data=health_centre, color='red') +
    theme_void()
  # mapview(metro) + urbanmetro + health_centre +grocery_stores
  
  
  # 4. join areas  --------------
  area_metro <- rbind(urbanmetro, popborder)
  area_metro2 <- st_join(area_metro, grocery_stores)
  area_metro3 <- st_join(area_metro2, health_centre)
  area_metro3$metro <- city
  area_metro3 <- area_metro3 %>% group_by(metro) %>% summarise()
  # mapview(area_metro3)
  
  
  # 5. crop water bodies --------------
  water$type <- 'water'
  coast$type <- 'water'
  water <- water %>% group_by(type) %>% summarise()
  coast <- coast %>% group_by(type) %>% summarise()
  
  # crop with area of the metro region
  area_metro4 <- st_difference(st_union(area_metro3), st_union(water))
  area_metro4 <- st_difference(st_union(area_metro4), st_union(coast))
  # mapview(area_metro4) + water

  
  
  ######  3 Function to create area of research (base for hexagon grid)  ---------------
  message('generating hex')
  make_hex <- function(poly, resolution=8) {  # poly = metro
    
    # projections
    poly <- st_transform(poly, crs = myprojection_latlong) 
    st_crs(poly)$epsg <- 4326
    
    # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
    hex_ids <- h3jsr::polyfill(poly, res = resolution, simple = TRUE)
    
    # pass the h3 ids to return the hexagonal grid
    hex_grid <- hex_ids %>% 
                h3jsr::h3_to_polygon(simple = FALSE) %>%
                rename(id_hex = h3_address) %>%
                mutate(metro = city) %>%
                na.omit() %>%
                st_sf()
    
    # add metro name
    hex_grid$metro <- city
    
    # hex cells in active areas
    hex_grid_active <- st_intersection(hex_grid, area_metro4)
    hex_grid_active2 <- subset(hex_grid, id_hex  %in% unique(hex_grid_active$id_hex))
    # mapview(area_metro4) + hex_grid_active
    # head(hex_grid_active)

    # projection
    hex_grid <- st_transform(hex_grid, crs = myprojection_latlong)
    hex_grid_active2 <- st_transform(hex_grid_active2, crs = myprojection_latlong)
    
    # save hex
    # readr::write_rds(hex_grid, file = paste0('./data/shapes/hex_grid_',city,'.rds'))
    sf::st_write(hex_grid, dsn= paste0('./data/shapes/hex_grid_',city,'.gpkg'))
    sf::st_write(hex_grid_active2, dsn= paste0('./data/shapes/hex_grid_active_',city,'.gpkg'))
  }

  # apply function
  make_hex(poly=metro, resolution=8)
  
  }




# apply function
pblapply(X = c('mal', 'got', 'sto'), FUN=save_hex)





a <- st_read('R:/Dropbox/other_projects/0_jean_capability1/spatial_grid/hex_sto.shp')




