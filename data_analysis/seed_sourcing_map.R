#Create a map of seed sources!

#Load packages
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(sp)
library(ggpubr)

#good tutorial
#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#State boundaries
states <- map_data("state")
GB_states <- subset(states, region %in% c("california", "oregon", "idaho", "nevada", "utah", "washington"))

#Coordinates of seed sources
sites <- data.frame(
  long = c(-122.022639,
           -119.687049,
           -119.074338,
           -120.157141,
           -112.338827,
           -115.468103,
           -117.886094
  ),
  lat = c(41.913596,
          43.456899,
          42.597319,
          40.489488,
          39.716758,
          40.959264,
          44.097867
  ), 
  names = c("Norcross", "NGBER", "Roaring Springs", "Susanville", "Little Sahara", "Elko", "Vale"),
  stringsAsFactors = FALSE
)

#Download the shapefile of Great Basin boundaries
shp <- readOGR(dsn = file.path("C:/Users/Lina/Dropbox/Academics/GIS/Eastern Oregon/boundaries/GreatBasin.shp"))
germG <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Plot!
map <- ggplot(data = GB_states) + 
  geom_polygon(data = germG, aes(x = long, y = lat, group = group), fill = "grey", color = "grey", alpha = 0.5)+
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3)+
  geom_point(data = sites, aes(x = long, y = lat), color = "black", size = 4.5)+
  geom_point(data = sites, aes(x = long, y = lat, col = names), size = 4)+
  scale_color_manual(values = c("#D8B365", "#F6E8C3", "#FFC0CB", "#01665E","#5AB4AC", "#C7EAE5", "#8C510A"  ))+
  geom_text(data = sites, aes(label=names, x = long, y = lat),vjust = -0.7, size = 4.8)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        text = element_text(size=18))

ggarrange(map, pca, nrow = 1, ncol = 2, labels  = c("(a)", "(b)"))
