#This script:

  ###### 1: Manually download spatial data (OSM and Sweden and administrative boundaries)
  ###### 2: Load shape file and Assign projection
  ###### 3: Select Metropolitan areas
  ###### 4: Save Metropolitan areas separately 


#### GTFS data
#https://transitfeeds.com/p/trafiklab/50?p=1


##################### Load packages -------------------------------------------------

source("./R/1_LoadPackages.R")
  
  
  
  
  

###### 1: Manually download spatial data  ----------------------
  
  # Open Street Maps files, Source: https://mapzen.com/data/metro-extracts/
  # shape file of  Administrative Areas in Sweden,  Source: JEAN
    

  
  
  
###### 2: Load shape file and Assign projection ----------------------
  
  
# read shape file
  municipality <- read_sf(dsn="./data-raw/shape_files/Municipalities_alla_kommuner", layer="alla_kommuner")      #read shape file
  plot(municipality)

  

  # Fix Non-english special characters
  municipality$KOM_NAMN <- as.character(municipality$KOM_NAMN)
  
  # encode using UTF-8
  Encoding(municipality$KOM_NAMN)  <- "UTF-8"
  table(municipality$KOM_NAMN)
  
  

  # Assign projection to lat lon
  myprojection_latlong <- CRS("+proj=longlat +zone=33N +ellps=WGS84 +datum=WGS84")
  
  
  st_crs(municipality)
  # municipality <- st_transform(municipality, myprojection_latlong)
  municipality <- st_transform(municipality, 4674) 
  
  
###### 3: Select Metropolitan areas ----------------------

# Define metro areas
  metro_sto <- c("Stockholm"
                 , "Huddinge"
                 , "Botkyrka"
                 , "Salem"
                 , "Södertälje"
                 , "Nykvarn"
                 , "Nynäshamn"
                 , "Haninge"
                 , "Tyresö"
                 , "Nacka"
                 , "Värmdö"
                 , "Lidingö"
                 , "Vaxholm"
                 , "Österåker"
                 , "Norrtälje"
                 , "Vallentuna"
                 , "Sigtuna"
                 , "Upplands-Bro"
                 , "Upplands Väsby"
                 , "Täby"
                 , "Sollentuna"
                 , "Danderyd"
                 , "Järfälla"
                 , "Ekerö"
                 , "Sundbyberg"
                 , "Solna")
  
  metro_got <- c("Ale"
                 , "Alingsås"
                 , "Göteborg"
                 , "Härryda"
                 , "Kungsbacka"
                 , "Kungälv"
                 , "Lerum"
                 , "Lilla Edet"
                 , "Mölndal"
                 , "Partille"
                 , "Stenungsund"
                 , "Tjörn"
                 , "Öckerö")
  
  metro_mal <- c("Malmö"
                 , "Vellinge"
                 , "Trelleborg"
                 , "Skurup"
                 , "Svedala"
                 , "Lund"
                 , "Staffanstorp"
                 , "Burlöv"
                 , "Lomma"
                 , "Kävlinge"
                 , "Eslöv"
                 , "Höör")


# select metros
metro_got <- municipality[municipality$KOM_NAMN %in% metro_got , ]
metro_sto <- municipality[municipality$KOM_NAMN %in% metro_sto , ]
metro_mal <- municipality[municipality$KOM_NAMN %in% metro_mal , ]

metro_got$metro <- "got"
metro_sto$metro <- "sto"
metro_mal$metro <- "mal"


# plot
  plot(metro_got, col="red") #plot map
  plot(metro_sto, col="red") #plot map
  plot(metro_mal, col="red") #plot map
    

  
    
###### 4: Save Metropolitan areas separately   ----------------------
  
# Save metro areas Shape File
  readr::write_rds(metro_got, file = './data/shapes/metro_got.rds')
  readr::write_rds(metro_sto, file = './data/shapes/metro_sto.rds')
  readr::write_rds(metro_mal, file = './data/shapes/metro_mal.rds')
  
    
# clean memory
gc(reset = T)
  


###### 5: Create map of metro urban areas    ----------------------

# read shape file of urban areas of Sweden
urbansweden <- st_read(dsn="./data-raw/shape_files/Dense_areas_Sweden", layer="To2015_SR99TM_region")      #read shape file
urbansweden <- st_transform(urbansweden, 4674) 


# function to select only urban areas within metropolitan region
get_urbanmetro <- function(city) {
  
  # read metro area
  metropoly <- readr::read_rds(file = paste0('./data/shapes/metro_',city,'.rds'))
  
  # Create border of the entire metro area
  # metropoly <- as(metropoly, 'Spatial')
  # metroborder <- rgeos::gUnaryUnion(metropoly, id = metropoly@data$metro) #dissolver
  metroborder <- metropoly %>% group_by(metro) %>% summarise()
  
  # projection
  metroborder <- st_transform(metroborder, 4674) 
  
  # Crop urban area and metro boundaries 
  urbanmetro <- urbansweden[metroborder, ]

  # add metro name
  urbanmetro$metro <- city
  
  # remove Z dimension of spatial data
  urbanmetro <- urbanmetro %>% st_sf() %>% st_zm( drop = T, what = "ZM")
  
  # Save urban areas Shape File
  readr::write_rds(urbanmetro, file = paste0('./data/shapes/urbanmetro_',city,'.rds'))
  }


# apply function
pblapply(X = c('mal', 'got', 'sto'), FUN=get_urbanmetro)

gc(reset = T)



