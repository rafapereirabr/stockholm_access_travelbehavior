
#####################Loadpackages-------------------------------------------------------

options(java.parameters='-Xmx23G')
options(scipen=9999999999)
library(r5r)

library(data.table)
data.table::setDTthreads(percent = 100)

library(dplyr)
library(sf)
library(sfheaders)
library(magrittr)
library(pbapply)
library(ggplot2)
library(cowplot)
library(readr)
library(RColorBrewer)
library(readxl)
library(rgeos)
library(rgdal)
library(h3jsr) # h3 hex remotes::install_github("obrl-soil/h3jsr")
library(mapview)
mapviewOptions(platform = 'leafgl')

options(digits=10) # numberofdigitstoshow
options(scipen=999) #disablescientificnotation



library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
library(data.table)
library(BAMMtools) # fast calculation of jenks natural breaks




# create Natural Jenks function
jenks_natural <- function(df, var, breaks){
  options(scipen=9999999999)
  
  # data <- copy(iris)
  # var <- "Petal.Length"
  # breaks <- 5
  
  
  # conver df to data.table
  data <- copy(df)
  setDT(data)
  
  # name of new column
  newvar <- paste0(var,"_jenks")
  
  # calculate jenks natural breaks
  data[, paste0(newvar) := as.character(cut(get(var), breaks= BAMMtools::getJenksBreaks(get(var), breaks), include.lowest = TRUE, dig.lab=5)) ]
  
  # Edit factor text
  data[, paste0(newvar) := str_replace_all(get(newvar), "\\[|\\(|\\]", "") ]
  data[, paste0(newvar) := stri_replace_all_regex(get(newvar), "[,]", " - ") ]
  
  # get factor labels
  jenks_labels  <- data[, get(newvar)]  %>% table %>% names() %>% sort(decreasing = F) 
  
  # recode variable
  data[, paste0(newvar) := factor(get(newvar), levels = jenks_labels)]
  
  return(data)
}
