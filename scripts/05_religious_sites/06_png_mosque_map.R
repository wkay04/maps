library(sf)
library(osmdata)
library(ggplot2)
library(ggspatial)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("Creating PNG mosque map...\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)
bbox <- st_bbox(ad34)

# Query OSM
q <- opq(bbox) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")

osm_data <- osmdata_sf(q)

# Combine points and polygons
mosques <- bind_rows(
  osm_data$osm_points,
  if(!is.null(osm_data$osm_polygons)) st_centroid(osm_data$osm_polygons) else NULL
) %>% st_transform(4326)

# Filter to AD34
mosques <- st_filter(mosques, ad34)

cat(sprintf("Found %d mosques\n", nrow(mosques)))

# Create map
map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles") +
  geom_sf(data = ad34, fill = NA, color = "red", linewidth = 2) +
  geom_sf(data = mosques, color = "darkgreen", size = 5, shape = 18) +
  geom_sf_text(
    data = mosques,
    aes(label = ifelse(is.na(name), "", name)),
    size = 3,
    nudge_y = 0.002,
    color = "black",
    fontface = "bold",
    bg.colour = "white",
    bg.r = 0.1
  ) +
  labs(
    title = "Mosques and Islamic Centers in Assembly District 34",
    subtitle = sprintf("Data from OpenStreetMap - %d locations", nrow(mosques)),
    caption = "Red boundary: AD34 | Green diamonds: Mosques"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("outputs/maps/mosques_ad34.png", map, width = 11, height = 8.5, dpi = 300)
cat("✓ Saved: outputs/maps/mosques_ad34.png\n")
