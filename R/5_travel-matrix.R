# routing






###### Load packages -------------------------------------------------
source("./R/1_LoadPackages.R")

# routing plans
plan_weekday <- 
      tribble(
        ~id,    ~activity,    ~mode, ~departure_time, ~interval,         ~period,
          1,       'work',    'walk',              6,      180,           'peak',
          2,       'work', 'bicycle',              6,      180,           'peak',
          3,    'grocery',    'walk',             10,      120,           'peak',
          4,    'grocery',    'walk',             15,      120,           'peak',
          5, 'healthcare',    'walk',              8,      180,           'peak',
          6,       'work',    'walk',              5,       60,  'semi-peak-pre',
          7,       'work',    'walk',              9,      300, 'semi-peak-post',
          8,       'work', 'bicycle',              5,       60,  'semi-peak-pre',
          9,       'work', 'bicycle',              9,      300, 'semi-peak-post',
         10, 'healthcare',    'walk',              7,      120,      'semi-peak',
         11, 'healthcare',    'walk',             11,      240,      'semi-peak')


setDT(plan_weekday)
head(plan_weekday)

# Function to calculate travel time matrix
get_ttmatrix <- function(city){ # city='sto'

  
########## Get origin-destination points
  
# read shape file of base areas
  # hex1 <- sf::st_read(paste0('./data/shapes/hex_grid_active_',city,'.gpkg'))
  baseareas <- sf::st_read(paste0('./data-raw/shape_files/Base_areas_Stockholm/basomr2010_110617.shp'))

  centroids_weighted_sf <- st_read(paste0('./data/shapes/centroids_pop-weighted_',city,'.gpkg'))
  
  names(centroids_weighted_sf)[1] <- 'id'
  names(centroids_weighted_sf)

  ## check map
  # mapview(baseareas) + centroids_weighted_sf
  

########## start routing

# Build r5r_core 
datapath <- paste0('./data/routing/', city)
r5r_core <- r5r::setup_r5(data_path = datapath)



# loop over multople trip plans
for( i in plan_weekday$id){ # i = 4

  ## input
    activity <- plan_weekday$activity[i]
    departure_hour  <- plan_weekday$departure_time[i]
    time_window  <- plan_weekday$interval[i]
    period  <- plan_weekday$period[i]
    if( plan_weekday$mode[i] == 'walk'){mode <- c('walk', 'transit')}
    if( plan_weekday$mode[i] == 'bicycle'){mode <- c('bicycle', 'transit')}
    
    # set departure time
    departure_datetime <- as.POSIXct(paste0("13-01-2016", departure_hour,":00:00"),
                                     format = "%d-%m-%Y %H:%M:%S")
    
    
    # dest file
    dest_file <- paste0('./data/traveltime_matrix/ttmatrix_',city,'_', mode[1],'_',departure_hour,'_', activity, '.rds')
    dest_file_csv <- paste0('./data/traveltime_matrix/ttmatrix_',city,'_', mode[1],'_',departure_hour,'_', activity, '.csv')
    message(paste0(dest_file, " being processed/n" ) )
    
    # compute travel times
    ttm <- r5r::travel_time_matrix(r5r_core = r5r_core,
                                   origins = centroids_weighted_sf,
                                   destinations = centroids_weighted_sf,
                                   mode = mode,
                                   time_window = time_window,
                                   departure_datetime = departure_datetime,
                                   max_trip_duration = 90,
                                   percentiles = 50,
                                   walk_speed = 3.6,
                                   max_walk_dist = 900,
                                   verbose = F)
    
    # adding matrix info
    ttm[, activity := activity]
    ttm[, mode := paste0(mode, collapse = '-')]
    ttm[, departure_hour := departure_hour]
    ttm[, time_window := time_window]
    ttm[, period := period]
    setkey(ttm)

    # save
    readr::write_rds(x = ttm, file = dest_file, compress = 'gz')
    fwrite(x = ttm, file = dest_file_csv)
}
}



# apply function
get_ttmatrix(city='sto')


# a <- read_rds('./data/traveltime_matrix/ttmatrix_sto_walk_10_grocery.rds')
# b <- fread('./data/traveltime_matrix/ttmatrix_sto_walk_10_grocery.csv')

rds_to_csv <- function(i){
  a <- read_rds(i)
  f_name <- substring(i, 26, 100)
  f_name <- stringr::str_replace(string = f_name, pattern = '.rds',replacement =  '.csv')
  fwrite(a, f_name)
}

f <- list.files('./data/traveltime_matrix/', pattern = '.rds', full.names = T)
pblapply(X=f, FUN=rds_to_csv)
