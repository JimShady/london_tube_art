library(tidyverse)
library(sf)
library(osmdata)
library(glue)

tubes     <- read_csv('https://raw.githubusercontent.com/JimShady/TubeAir/master/Line%20Location%20Data/tube_information.csv') %>%
  st_as_sf(coords = c("x", "y"), crs = 27700)

thames   <- st_read('https://raw.githubusercontent.com/KCL-ERG/useful_geography/master/thames.geojson')

northern  <- filter(tubes, line == 'Northern')

northern  <- st_buffer(northern, 500) %>%
  st_transform(4326)

for (i in 1:nrow(northern)) {
  
  message(glue("Done {i}"))
  
  a_station <- northern[i,]
  
  osm_data  <- opq(bbox = st_bbox(a_station)) %>% 
    add_osm_feature(key = 'building') %>%
    osmdata_sf() %>%
    .$osm_polygons %>%
    select(geometry) %>%
    mutate(station = a_station$name) %>%
    st_intersection(a_station)
  
  if (i == 1) {result <- osm_data} else {result <- bind_rows(result, osm_data)}
  
}

pdf("outputs/northern.pdf", width = 11.7, height = 16.5)

ggplot(result) +
  geom_sf(col = NA, fill = 'black') +
  #geom_sf_label(aes(label = station)) +
  ggtitle('Northern') +
  theme(axis.line = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 40)) +
  geom_sf(data = st_intersection(thames, 
                                 st_as_sfc(st_bbox(northern))),
          color = NA,
          fill = 'lightblue')

dev.off()
