source("./R/1_LoadPackages.R")


########## prepare origin-destination points

city = 'sto'

# read shape file of base areas
# hex1 <- sf::st_read(paste0('./data/shapes/hex_grid_active_',city,'.gpkg'))
baseareas <- sf::st_read(paste0('./data-raw/shape_files/Base_areas_Stockholm/basomr2010_110617.shp'))

# read population grid data at high resolution, get centroid of each cell
pop <- readr::read_rds(file = paste0('./data/shapes/pop_metro_',city,'.rds'))
pop <- sf::st_centroid(pop)

# Use the same projection
baseareas <- st_transform(baseareas, crs= st_crs(pop))

# identify for each grid cell the base are it belongs to
pop_baseareas <- sf::st_join(pop, baseareas)

# convert to data.frame
pop_baseareas_df <- sfheaders::sf_to_df(pop_baseareas, fill = T)
pop_baseareas_df <- setDT(pop_baseareas_df)[, .(swe_ppp_2020_constrained , BASKOD2010, x, y)]

sum(pop_baseareas_df$swe_ppp_2020_constrained)

# pop weighted centroid
centroids_weighted <- pop_baseareas_df[, .(x = weighted.mean(x = x, w = swe_ppp_2020_constrained),
                                           y = weighted.mean(x = y, w = swe_ppp_2020_constrained)
), by = BASKOD2010]

# back to sf
centroids_weighted <- na.omit(centroids_weighted)
centroids_weighted_sf <- sfheaders::sf_point(obj = centroids_weighted, x='x', y='y', keep = T)
st_crs(centroids_weighted_sf) <- st_crs(baseareas)


## save population-weighted centroids
# sf::st_write(centroids_weighted_sf, paste0('./data/shapes/centroids_pop-weighted_',city,'.gpkg'))



