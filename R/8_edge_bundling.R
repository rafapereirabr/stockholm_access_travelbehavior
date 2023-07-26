# source("./R/1_LoadPackages.R")
library(igraph)
library(ggforce)
library(edgebundle) # https://github.com/schochastics/edgebundle
library(ggplot2)
library(janitor)
library(xlsx)
library(sf)
library(sfheaders)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
# library(rnaturalearthhires)
library(maptiles)
library(tidyr)
library(tidyverse)
library(data.table)



###### Read spatial context data ------------------

sweden <- rnaturalearth::ne_countries(scale = "large", country = 'Sweden', returnclass = "sf")
baseareas <- sf::st_read('./data-raw/shape_files/Base_areas_Stockholm/basomr2010_110617.shp')

# Use the same projection
my_projection <- "EPSG:3857"
my_projection <- st_crs(sweden)

sweden <- st_transform(sweden, crs= my_projection)
baseareas <- st_transform(baseareas, crs= my_projection)


# stockholm bbox and boundary
sto_box <- st_bbox(baseareas)
sto_boundary <- st_union(baseareas)


### Map backgorund tile
# # Project to EPSG:3857
# nc <- st_transform(baseareas, "EPSG:3857")
# 
# # dowload tiles and compose raster (SpatRaster)
# base_tile <- get_tiles(baseareas,
#                        crop = TRUE,
#                        provider = "CartoDB.DarkMatterNoLabels", zoom = 10)
# # display map
# plot_tiles(base_tile)
# 
# # convert tile para df
# base_df <- terra::as.data.frame(base_tile, xy = TRUE)  %>%
#               mutate(hex = rgb(red, green, blue, maxColorValue = 255))
# 

###### Read Travel survey data ------------------

## Data dictionary

#' Household_income_0_1_2 
#' Those coded “0” have a household income in the “lower” decile, 1 is the upper and 2 is the middle (the majority).

#' Starttid_0_prepeak_1_peak_sysmiss_postpeak  
#' Those coded “0” have a departure time in the pre-peak period, those coded “1” depart during the peak and “missing value” is in the post-peak.

#' Sustainable_1_other_none_0_NEW 
#' Those coded “1” travel using some (combination of) sustainable modes of transport. Those coded “0” do not.



# excel 2368
od <- data.table::fread('./data-raw/household_survey/Results_OD_pairs_income_groups_sustainable_travel_no.csv')
head(od)

# drop observations with no destination
od <- subset(od, ! is.na(base_area_destination1_A ))


# recode:
od[, Household_income_0_1_2 := fcase(Household_income_0_1_2 == 0 ,'Low',
                                     Household_income_0_1_2 == 1 ,'High',
                                     Household_income_0_1_2 == 2 ,'Middle')]


od[, Starttid_0_prepeak_1_peak_sysmiss_postpeak := fcase(Starttid_0_prepeak_1_peak_sysmiss_postpeak == 0 ,'Pre_peak',
                                                         Starttid_0_prepeak_1_peak_sysmiss_postpeak == 1 ,'Peak',
                                                         is.na(Starttid_0_prepeak_1_peak_sysmiss_postpeak) ,'Post_peak')]

od[, Sustainable_1_other_none_0_NEW := fcase(Sustainable_1_other_none_0_NEW == 0 ,'Not_sustainable',
                                             Sustainable_1_other_none_0_NEW == 1 ,'Sustainable')]


head(od)
table(od$Household_income_0_1_2)

edges <- od[, .(flow = .N),
              by=.(base_area_home1_A, base_area_destination1_A, 
                   Household_income_0_1_2, Starttid_0_prepeak_1_peak_sysmiss_postpeak , Sustainable_1_other_none_0_NEW)]
head(edges)
table(edges$Household_income_0_1_2)
#> High    Low Middle 
#> 1603    816   8169 


# # reshape
# od_long_income <- melt(od, id.vars = c('base_area_home1_A', 'base_area_destination1_A'),
#                        measure.vars = 'Household_income_0_1_2') %>% 
#   setnames(., old = 'value', new = 'income')
# 
# od_long_period <- melt(od, id.vars = c('base_area_home1_A', 'base_area_destination1_A'),
#                        measure.vars = 'Starttid_0_prepeak_1_peak_sysmiss_postpeak') %>%
#   setnames(., old = 'value', new = 'period')
# 
# 
# od_long_mode <- melt(od, id.vars = c('base_area_home1_A', 'base_area_destination1_A'),
#                      measure.vars = 'Sustainable_1_other_none_0_NEW') %>%
#   setnames(., old = 'value', new = 'mode')
# 
# # merge
# edges <- left_join(od_long_income, od_long_period, by=c('base_area_home1_A', 'base_area_destination1_A')) %>% 
#   left_join(., od_long_mode, by = c('base_area_home1_A', 'base_area_destination1_A'))
# 
# 
# head(od_long_income)
# head(od_long_period)
# subset(od_long_period, base_area_home1_A  %in% od_long_income$base_area_home1_A )
# 
# edges[, c('variable', 'variable.x', 'variable.y') := NULL]

setnames(edges, 'base_area_home1_A' , 'base_area_orig') 
setnames(edges, 'base_area_destination1_A' , 'base_area_dest') 
setnames(edges, 'Household_income_0_1_2' , 'income')
setnames(edges, 'Starttid_0_prepeak_1_peak_sysmiss_postpeak' , 'period')
setnames(edges, 'Sustainable_1_other_none_0_NEW' , 'mode')
head(edges)



# # summary of trips y o-d pair
# edges <- edges[, .(flow = .N),
#              by=.(base_area_orig, base_area_dest, income, period, mode)]
# head(edges)

# remove missing values
edges2 <- na.omit(edges)

# expand data so that each trip is a single row
edges <- edges2 %>%
  mutate(flow = flow) %>%
  uncount(flow)

nrow(edges) == sum(edges2$flow)
setDT(edges)[, flow := 1]


# Add spatial coordinates do edges -----------------

# Population centroids
centroids <- sf::st_read('./data/shapes/centroids_pop-weighted_sto.gpkg')
centroids <- st_transform(centroids, crs= my_projection)

coordinates <- sfheaders::sf_to_df(centroids, fill = T)
setDT(coordinates)
setnames(coordinates, 'BASKOD2010' , 'base_area')
head(coordinates)

# bring coordinates of points at origins
edges[ coordinates, on=c('base_area_orig'='base_area'),
       c('lat_orig', 'lon_orig') := list(i.y , i.x) ]
 
# bring coordinates of points at destinations
edges[ coordinates, on=c('base_area_dest'='base_area'),
       c('lat_dest', 'lon_dest') := list(i.y , i.x) ]

# remove missing values
edges <- na.omit(edges)
head(edges)





###### Build network ------------------
# vertices: all airports with trips
vert <- coordinates[,.(base_area , x, y)]
vert <- unique(vert)

# function to create network
build_network <- function(edges2=edges, 
                          mode2= unique(edges$mode),
                          income2= unique(edges$income),
                          period2= unique(edges$period),
                          d = 12, # 2
                          w = 4, # 2
                          s = 20 # 20 no paper, 10 de teste
                         ){
  
  edges2 <- subset(edges2, 
                   mode %in% mode2 & 
                   period %in% period2 &
                  income %in% income2
                   )
  
  
  # build igraph network
  g <- igraph::graph_from_data_frame(d = edges2, directed = T, vertices = vert)
  
  # coordinates of vertices
  xy <- cbind(V(g)$x, V(g)$y)
  
  ### Edge Bundling
  
  # Edge-Path Bundling
  pbundle <- edge_bundle_path(g, xy, 
                              max_distortion = d,
                              weight_fac = w,
                              segments = s)
  
 # a <-  ggplot() +
 #    geom_sf(data=sweden , fill='gray10', color=NA) +
 #    geom_sf(data=sto_boundary , fill=NA, color='gray', size=.5) +
 #    # coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]-0.7),
 #    #          ylim = c(sto_box[[2]]+0.2, sto_box[[4]]-0.2) ) +
 # 
 #    # coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]-0.7),
 #    #          ylim = c(sto_box[[2]]+0.4, sto_box[[4]]-0.6) ) +
 # 
 #    coord_sf(xlim = c(sto_box[[1]]+0.6, sto_box[[3]]-1.3),
 #             ylim = c(sto_box[[2]]+0.6, sto_box[[4]]-0.8) ) +
 #    geom_path(data = pbundle, aes(x, y, group = group),
 #              col = "#9d0191", size = 0.05, alpha=.2) +
 #    geom_path(data = pbundle, aes(x, y, group = group),
 #              col = "white", size = 0.005, alpha=.2)
 #    ggsave(a, file= 'test.png')
  
  if(length(period2)>1){ period2 <- 'all'}
  if(length(income2)>1){ income2 <- 'all'}
  if(length(mode2)>1){ mode2 <- 'all'}
  # colnames
  pbundle$period <- period2
  pbundle$income <- income2
  pbundle$mode <- mode2
  
    
    
  # output
  output_name <- paste0( income2,'_',
    mode2 ,'_', period2,'_d', d,'_w', w,'_s', s)
  
  newList <- list(pbundle)
  names(newList) <- output_name
  list2env(newList ,.GlobalEnv)
  print(output_name)
}


######### compare income only --------------------------
table(edges$income)
build_network(income2='High')
build_network(income2='Middle')
build_network(income2='Low')

df_plot_income <- rbind(High_all_all_d12_w4_s20,
                        Middle_all_all_d12_w4_s20,
                        Low_all_all_d12_w4_s20)
head(df_plot_income)

setDT(df_plot_income)
df_plot_income[, income := factor(income, levels=c('Low', 'Middle', 'High'))]


table(edges$income)
table(df_plot_income$income)


### Figure income

fig_income <-  
  ggplot() +
  # geom_tile(data = base_df, aes(x, y, fill = hex), color=NA) +
  # scale_fill_identity() +
  geom_sf(data=sweden , fill='gray10', color=NA) +
  geom_sf(data=sto_boundary , fill=NA, color='gray', size=.5) +
  geom_sf(data=sto_boundary , fill=NA, color='gray', size=.5) +
  # coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]-0.7),
  #          ylim = c(sto_box[[2]]+0.4, sto_box[[4]]-0.6) ) +
  coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]),
          ylim = c(sto_box[[2]], sto_box[[4]]) ) +
  geom_path(data = df_plot_income, aes(x, y, group = group),
            col = "#9d0191", linewidth = 0.5, alpha=.1) +
  geom_path(data = df_plot_income, aes(x, y, group = group),
            col = "white", linewidth = 0.05, alpha=.1) +
  facet_wrap(.~income, nrow=1 ) +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        strip.background=element_rect(colour=NA, fill="gray95") )

ggsave(fig_income, file= './figures/income_e2dge_bundling_path222.png', dpi=200,
       width = 16,height = 8, units = 'cm')







######### compare Mode Vs Period --------------------------

# generate networks
build_network(period2='Peak', mode2='Sustainable')
build_network(period2='Peak', mode2='Not_sustainable')
build_network(period2='Post_peak', mode2='Sustainable')
build_network(period2='Post_peak', mode2='Not_sustainable')

df_mode_period <- rbind(all_Sustainable_Peak_d12_w4_s20,
                        all_Not_sustainable_Peak_d12_w4_s20,
                        all_Sustainable_Post_peak_d12_w4_s20,
                        all_Not_sustainable_Post_peak_d12_w4_s20)


# ### Figure

fig_mode_period <-  
  
  ggplot() +
  geom_sf(data=sweden , fill='gray10', color=NA) +
  geom_sf(data=sto_boundary , fill=NA, color='gray', size=.5) +
  # coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]-0.7),
  #         ylim = c(sto_box[[2]]+0.4, sto_box[[4]]-0.6) ) +
  coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]),
           ylim = c(sto_box[[2]], sto_box[[4]]) ) +
  geom_path(data = df_mode_period, aes(x, y, group = group),
            col = "#9d0191", size = 0.5, alpha=.1) +
  geom_path(data = df_mode_period, aes(x, y, group = group),
            col = "white", size = 0.05, alpha=.1) +
   facet_grid(mode ~ period ) +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        strip.background=element_rect(colour=NA, fill="gray95") )
beepr::beep()


ggsave(fig_mode_period, file= './figures/mode_vs_period_e2dge_bundling_path.png', dpi=200,
       width = 16,height = 16, units = 'cm')
beepr::beep()


###### joint figures
fig1 <- fig_income / fig_mode_period + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(widths = c(1, 1), heights = c(1, 3), nrow  = 2) #, design ="1 2")

ggsave(fig1, file= './figures/e2dge_bundling_path_patchw.png', dpi=300,
       width = 16,height = 25, units = 'cm')
beepr::beep()


# plot_fun <- function(edge_b){ # edge_b <- High_Peak_d2_w2_s20
#       temp_figure <-  ggplot() +
#                       geom_sf(data=baseareas , fill='gray10', color='gray30') +
#                       geom_path(data = edge_b, aes(x, y, group = group),
#                                 col = "#9d0191", size = 0.05, alpha=.2) +
#                       geom_path(data = edge_b, aes(x, y, group = group),
#                                 col = "white", size = 0.005, alpha=.2) +
#                       # ggraph::theme_graph(background = "gray")
#                       theme_classic() +
#                       theme(axis.line=element_blank(),
#                             axis.text=element_blank(),
#                             axis.ticks=element_blank(),
#                             axis.title=element_blank())
#       
#       return(temp_figure)
# }
# 
# 
# # create figures
# fig_high_peak <- plot_fun(High_Peak_d2_w2_s20)
# fig_low_peak <- plot_fun(Low_Peak_d2_w2_s20)
# 
# fig_high_post_peak <- plot_fun(High_Post_peak_d2_w2_s20)
# fig_low_post_peak <- plot_fun(Low_Post_peak_d2_w2_s20)



# fig <- fig_high_peak + fig_low_peak + fig_high_post_peak + fig_low_post_peak + plot_layout(ncol = 2)







######### compare Mode Vs Period AND Mode Vs income --------------------------

# generate networks
build_network(period2='Peak', mode2='Sustainable' , income2 = 'High')
build_network(period2='Peak', mode2='Sustainable' , income2 = 'Low')

build_network(period2='Peak', mode2='Not_sustainable' , income2 = 'High')
build_network(period2='Peak', mode2='Not_sustainable' , income2 = 'Low')

build_network(period2='Post_peak', mode2='Sustainable' , income2 = 'High')
build_network(period2='Post_peak', mode2='Sustainable' , income2 = 'Low')

build_network(period2='Post_peak', mode2='Not_sustainable' , income2 = 'High')
build_network(period2='Post_peak', mode2='Not_sustainable' , income2 = 'Low')


df_mode_period_income <- rbind(High_Sustainable_Peak_d12_w4_s20
                                  , Low_Sustainable_Peak_d12_w4_s20
                                  , High_Not_sustainable_Peak_d12_w4_s20
                                  , Low_Not_sustainable_Peak_d12_w4_s20
                                  , High_Sustainable_Post_peak_d12_w4_s20
                                  , Low_Sustainable_Post_peak_d12_w4_s20
                                  , High_Not_sustainable_Post_peak_d12_w4_s20
                                  , Low_Not_sustainable_Post_peak_d12_w4_s20
                                )

setDT(df_mode_period_income)
df_mode_period_income[, var := paste0(income, '\n', mode)]
head(df_mode_period_income)


# ### Figure

fig_mode_period_income <-  
  
  ggplot() +
  geom_sf(data=sweden , fill='gray10', color=NA) +
  geom_sf(data=sto_boundary , fill=NA, color='gray', size=.5) +
  coord_sf(xlim = c(sto_box[[1]], sto_box[[3]]-0.7),
           ylim = c(sto_box[[2]]+0.4, sto_box[[4]]-0.6) ) +
  geom_path(data = df_mode_period_income, aes(x, y, group = group),
            col = "#9d0191", size = 0.5, alpha=.1) +
  geom_path(data = df_mode_period_income, aes(x, y, group = group),
            col = "white", size = 0.05, alpha=.1) +
  facet_grid(var ~ period ) +
  theme_classic() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        strip.background=element_rect(colour=NA, fill="gray95") )



ggsave(fig_mode_period_income, file= './figures/mode_period_income_zoom.png', dpi=200,
       width = 16,height = 21, units = 'cm')
beepr::beep()



#' attempts
#' keep edge bundling but aggregate
#' heatmaps (tile maps)
#' chord diagram - Circular Visualization of Flow Matrices
#' 
#' overlapping - overline


overline com path do r5r