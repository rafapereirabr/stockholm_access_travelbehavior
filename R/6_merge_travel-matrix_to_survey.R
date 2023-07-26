
###### Load packages -------------------------------------------------
source("./R/1_LoadPackages.R")


######### list travel time matrices -------------

# all files
files <- list.files('./data/traveltime_matrix', full.names = T)

# only keep .csv files and work
files <- files[files %like% 'work']
files <- files[files %like% '.csv']




######### read matrices and pile them up -------------

# function to read files
read_matrices <- function(i){ # i= 1

  # read ttmatrix
  f <- files[i]
  df <- fread(f)
  
  # get new column name
  df[, new_column := paste0(activity,'_',mode,'_',period)]
  df[, c('activity', 'mode', 'departure_hour', 'time_window', 'period') := NULL]
  head(df)
  
  return(df)
}

# read matrices and pile them up
dt <- pblapply(X=1:length(files), FUN=read_matrices)
dt <- rbindlist(dt)
head(dt)

# reshape data from long to wide format
dt_wide <- dcast(data=dt, formula = fromId+toId~new_column, value.var='travel_time')
head(dt_wide)





######### merge travel time estimates to houseold survey -------------

# read household survey
survey <- fread('./data-raw/household_survey/Weekday_work_trips_Combinations.csv')
head(survey)
names(survey) <- c('Trip_ID_number_proxy',
                   'base_area_home',
                   'base_area_trip_start',
                   'base_area_destination')


nrow( na.omit(survey) ) - nrow(survey)

survey_home <- left_join(x = survey,
                     y = dt_wide, 
                     by = c('base_area_home'='fromId',
                            'base_area_destination'='toId' ))

survey_trip_start <- left_join(x = survey,
                         y = dt_wide, 
                         by = c('base_area_trip_start'='fromId',
                                'base_area_destination'='toId' ))


######### savel household data with travel time estimates -------------

fwrite(x = survey_home, file='./data/survey_home_ttmatrix.csv')
fwrite(x = survey_trip_start, file='./data/survey_trip_start_ttmatrix.csv')

