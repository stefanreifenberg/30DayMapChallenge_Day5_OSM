library(tidyverse)
library(sf)
library(ggtext)
library(showtext)
library(gganimate)
library(here)

freiburg_districts <- read_sf("data/stadtbezirke.shp") |>
  st_union() |>
  st_transform(4326)

freiburg_water <- sf::read_sf("data/gis_osm_water_a_free_1.shp",
                              layer = "gis_osm_water_a_free_1") |>
  st_intersection(freiburg_districts)

freiburg_roads <- sf::read_sf("data/gis_osm_roads_free_1.shp",
                              layer = "gis_osm_roads_free_1") |>
  filter(!fclass %in% c("footway", "steps", "bridleway", "path", "pedestrian", "cycleway")) |>
  st_intersection(freiburg_districts)

freiburg_buildings <- sf::read_sf("data/gis_osm_buildings_a_free_1.shp",
                                  layer = "gis_osm_buildings_a_free_1") |>
  st_intersection(freiburg_districts)

freiburg_landuse <- sf::read_sf("data/gis_osm_landuse_a_free_1.shp",
                                layer = "gis_osm_landuse_a_free_1")

freiburg_nature <- sf::read_sf("data/gis_osm_natural_a_free_1.shp",
                               layer = "gis_osm_natural_a_free_1")

freiburg_rural <- freiburg_landuse |>
  rbind(freiburg_nature) |>
  mutate(rural = if_else(fclass %in% c("forest", "grass", "meadow", "nature_reserve", "scrub", "heath", "beach", "cliff"), "1", "0")) |>
  filter(rural == "1") |>
  st_intersection(freiburg_districts)

anim <- ggplot() +
  geom_sf(data = freiburg_districts,
          fill = "grey92",
          color = "grey92",
          lwd = 0.1) +
  geom_sf(data = freiburg_rural,
          fill = "#8cbf58",
          color = "#8cbf58",
          lwd = 0.05) +
  geom_sf(data = freiburg_water,
          fill = "#accdef",
          color = "#accdef") +
  geom_sf(data = freiburg_roads,
          color = "grey80",
          lwd = 0.2) +
  geom_sf(data = freiburg_buildings,
          color = "grey40",
          lwd = 0.2) +
  theme(  panel.background = element_rect(fill = "#e3decc", colour = "#e3decc",
                                          size = 0, linetype = "solid"),
          plot.background = element_rect(fill = "#ece9dc")) +
  theme_void() +
  transition_layers(layer_length = 1,
                    transition_length = 2,
                    from_blank = FALSE,
                    layer_order = c(1:6,5:1)) +
  enter_grow()

g1 <- gganimate::animate(anim, fps = 30, width=1600, height=1600, duration = 6, bg = '#e3decc',  renderer = av_renderer())
gganimate::anim_save("freiburg.webm", g1)
