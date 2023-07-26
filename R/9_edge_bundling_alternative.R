
setDT(df_plot_income)
df_plot_income[, count := 1]
df_plot_income <- df_plot_income[order(group, period, income)]

ddd <- df_plot_income[, .(count = sum(count)), by=.(group,x, y)]
unique(ddd)
df_line <- sfheaders::sf_linestring(obj = ddd,
                                    x = 'x', 
                                    y= 'y',
                                    linestring_id = 'group',
                                    keep = T)
head(df_line)
library(stplanr)
library(sf)

m <- subset(df_line, income == 'Low')
m <- stplanr::overline(sl = m, attrib = "count", fun = sum, ncores = 4, simplify = F)
head(m)

t <- ggplot(data = m, aes(color=count)) + 
  geom_sf( alpha=.1) +
  coord_sf()

  

  
# 
# ggplot(mm, aes(factor(base_area_orig), factor(base_area_dest), fill= count)) + 
#   geom_tile() +
#   st_coordinates()
# 
#   scale_fill_viridis_c(option = 'E', direction = ) +
#   facet_wrap(.~income, nrow=1 ) +
#   theme_classic() +
#   theme(axis.line=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title=element_blank(),
#         strip.background=element_rect(colour=NA, fill="gray95") )

ggsave(t, file= './figures/test_count_overLOW.png', dpi=300,
       width = 16, height = 8, units = 'cm')
