library(tidyverse)
library(sf)
library(osmdata)
library(glue)
library(gganimate)

tubes     <- read_csv('https://raw.githubusercontent.com/JimShady/TubeAir/master/Line%20Location%20Data/tube_information.csv') %>%
              st_as_sf(coords = c("x", "y"), crs = 27700)

thames   <- st_read('https://raw.githubusercontent.com/KCL-ERG/useful_geography/master/thames.geojson')

central  <- filter(tubes, line == 'Central')

central  <- st_buffer(central, 500) %>%
              st_transform(4326)

for (i in 1:nrow(central)) {
  
  message(glue("Done {i}"))
  
  a_station <- central[i,]
  
  buildings  <- opq(bbox = st_bbox(a_station)) %>% 
    add_osm_feature(key = 'building') %>%
    osmdata_sf() %>%
    .$osm_polygons %>%
    select(geometry) %>%
    mutate(station = a_station$name) %>%
    st_intersection(a_station)
  
  roads     <- opq(bbox = st_bbox(a_station)) %>% 
    add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    select(geometry) %>%
    mutate(station = a_station$name) %>%
    st_intersection(a_station)
  
  plot <- ggplot() +
    geom_sf(data=st_buffer(st_transform(a_station,27700),2)) +
    geom_sf(data = buildings, col = NA, fill = 'red') +
    geom_sf(data = roads) +
    theme(axis.line = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 36,colour='red'),
          ) +
    ggtitle(toupper(a_station$name))
  
  png(glue("{i}_.png"), width=500, height=500)
  print(plot)
  dev.off()
  
  system("cmd.exe", input = glue('cd {getwd()} && "C:\\Program Files\\ImageMagick-7.0.10-Q16-HDRI\\magick" convert {i}_.png {i}_.gif'))
  unlink(glue("{i}_.png"))
  
}

system("cmd.exe", 
       input = glue('cd {getwd()} && "C:\\Program Files\\ImageMagick-7.0.10-Q16-HDRI\\magick" convert -delay 80 *.gif -loop 0 central.gif'))