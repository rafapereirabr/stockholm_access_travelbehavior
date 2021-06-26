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


# read rail network
rail1 <- sf::st_read(paste0('./data-raw/shape_files/rail/Kollektivtrafik_spar_RUFS2050.shp'))
rail2 <- sf::st_read(paste0('./data-raw/shape_files/rail/Railway_national.shp'))


# recode rail
rail1 <- rail1 %>% mutate(type = case_when( bana %like% 'Lidingöbanan'~ 'Commuter train',
                                          bana %like% 'Roslagsbanan'~ 'Commuter train',
                                          bana %like% 'Saltsjöbanan'~ 'Commuter train',
                                          bana %like% 'Pendeltåg'~ 'Commuter train',
                                          bana %like% 'Nockebybanan'~ 'Light Railway',
                                          bana %like% 'Spårvägslinje 7'~ 'Light Railway',
                                          bana %like% 'Tvärbanan'~ 'Light Railway',
                                          bana %like% 'Tunnelbana Grön'~ 'Metro',
                                          bana %like% 'Tunnelbana Röd'~ 'Metro',
                                          bana %like% 'Tunnelbana Blå'~ 'Metro'))


rail1 <- select(rail1, 'type', 'geometry')
rail2$type <- 'National rail'
rail2 <- select(rail2, 'type', 'geometry')
names(rail1)
names(rail2)

rail2 <- st_transform(rail2, crs= st_crs(rail1))
rail <- rbind(rail1, rail2)
table(rail$type)


######## MAPS ---------------------------

## add pop info to base areas
head(pop_baseareas)

# sum pop from grid to base area
pop_total <- setDT(pop_baseareas)[, .(pop_total=sum(swe_ppp_2020_constrained, na.rm=T)), by=BASN2010]

# merge
baseareas <- left_join(baseareas, pop_total)


## calculate Density of base areas
baseareas$areakm2 <- st_area(baseareas)
baseareas$areakm2 <- as.numeric(baseareas$areakm2) / 10e5
baseareas$pop_densitykm2 <- baseareas$pop_total / baseareas$areakm2
head(baseareas)

summary(baseareas$pop_densitykm2)
summary(baseareas$areakm2)
summary(baseareas$pop_total)

sum(baseareas$pop_total, na.rm=T)
baseareas$pop_densitykm2 <- ifelse(is.na(baseareas$pop_densitykm2), 0, baseareas$pop_densitykm2)
baseareas$pop_total <- ifelse(is.na(baseareas$pop_total), 0, baseareas$pop_densitykm2)


# projection
baseareas2 <- copy(baseareas)
st_crs(rail)

baseareas2 <- st_transform(baseareas, crs= 4326) # sweden 3011 | global 4326 
rail <- st_transform(rail, crs= 4326)


# create Categories
summary(baseareas2$pop_densitykm2)

baseareas2 <- jenks_natural(df=baseareas2, var='pop_densitykm2', breaks=6)
table(baseareas2$pop_densitykm2_jenks, useNA = 'always')


baseareas2[, pop_densitykm2_jenks := factor(x = pop_densitykm2_jenks,
                                            levels = c('0 - 2585.6',
                                                       '2585.6 - 6701.2',
                                                       '6701.2 - 14678',
                                                       '14678 - 26329',
                                                       '26329 - 39895'))]

## Plot whole area ------------------
baseareas2 <- st_sf(baseareas2)

high_density_poly <- subset(baseareas2, pop_densitykm2>=7000)
bbox <- st_bbox(high_density_poly)


bbox[1] <- 17.95 # xmin
bbox[3] <- 18.12 # xmax 
bbox[2] <- 59.295 # ymin
bbox[4] <- 59.38 # ymin
# bbox[4] <- bbox[[2]] + (bbox[[4]] - bbox[[2]]) /2  # ymax


  
 
high_density_poly = st_as_sfc(bbox)

ggplot() + 
  geom_sf(data=baseareas2, aes(fill=pop_densitykm2_jenks),color=alpha("gray80", 0.5)) +
  scale_fill_brewer(palette = 'Purples', direction = 1) +
  # geom_sf(data=centroids_weighted_sf, color='gray', alpha=.7, size=.1) +
  geom_sf(data=rail, aes(color=type, group=type), alpha=.5, show.legend = "line", size=1) +
  scale_color_manual(values=c("#ef3b2c", "#006d2c", "#08519c", 'red'), labels=c("Commuter train", "Light Railway", "Metro", "National rail")) +
  coord_sf( xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T)


ggplot() + 
  geom_sf(data=baseareas2, aes(fill=pop_densitykm2_jenks),color=alpha("gray80", 0.5)) +
  geom_sf(data=high_density_poly, color='red', fill=NA, alpha=.5) 
  

fig_all <-  ggplot() + 
            geom_sf(data=baseareas2, aes(fill=pop_densitykm2_jenks), color=alpha("gray80", 0.5)) +
            scale_fill_brewer(palette = 'Purples', direction = 1) +
            geom_sf(data=high_density_poly, color='red', fill=NA, alpha=.5, size=1) +
            geom_sf(data=rail, aes(color=type), alpha=.5, show.legend = "line") +
            # geom_sf(data=rail, aes(color=type, size=type, group=type), alpha=.5, show.legend = "line") +
            scale_color_manual(values=c("#ef3b2c", "#006d2c", "#08519c", 'red'), labels=c("Commuter train", "Light Railway", "Metro", "National rail")) +
            labs(color='') +
            ggsn::scalebar(data=baseareas2,
                           dist = 10, dist_unit = "km", st.size=3, height=0.01,
                           transform = TRUE, location='bottomleft' ) +
            labs(fill= bquote('Density of\nResidents per'~ Km^2)) +
            theme_void() +
            theme(legend.position = c(.1, .74)) +
            guides(fill = guide_legend(order = 1), 
                   color = guide_legend(order = 2))



## Plot zoom ------------------
fig_zoom <- ggplot() + 
            geom_sf(data=baseareas2, aes(fill=pop_densitykm2_jenks),color=alpha("gray80", 0.5)) +
            scale_fill_brewer(palette = 'Purples', direction = 1) +
            # geom_sf(data=centroids_weighted_sf, color='gray', alpha=.7, size=.1) +
            geom_sf(data=rail, aes(color=type, group=type), alpha=.5, show.legend = "line", size=1) +
            scale_color_manual(values=c("#ef3b2c", "#006d2c", "#08519c", 'red'), labels=c("Commuter train", "Light Railway", "Metro", "National rail")) +
            coord_sf( xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T) + 
            ggsn::scalebar(x.min = bbox[1], x.max = bbox[3],
                           y.min = bbox[2], y.max = bbox[4],
                           dist = 2, dist_unit = "km", st.size=3, height=0.01, 
                           transform = TRUE, location='bottomleft' ) +
            theme_void() +
            theme(legend.position = 'none')

fig <- cowplot::plot_grid(fig_all, fig_zoom)

ggsave(fig, filename = './figures/fig1_map_pop_density_zoom4.png', 
       dpi=300, 
       width = 10,
       height = 6)

