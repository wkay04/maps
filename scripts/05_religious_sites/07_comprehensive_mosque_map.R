library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(ggspatial)
library(tidygeocoder)

setwd("~/Mayoral Results AD34")

cat("Creating comprehensive mosque map...\n")

# Load manually compiled data
mosques <- read.csv("data/mosques_manual.csv")

cat(sprintf("Loaded %d mosques from database\n", nrow(mosques)))

# Geocode all addresses
cat("Geocoding addresses...\n")
mosques_geo <- mosques %>%
  geocode(address, method = "osm", lat = latitude, long = longitude, verbose = FALSE)

success <- sum(!is.na(mosques_geo$latitude))
cat(sprintf("Successfully geocoded %d/%d locations\n", success, nrow(mosques)))

# Convert to sf
mosques_sf <- mosques_geo %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Load AD34
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)

# Mark which are in AD34
mosques_sf <- mosques_sf %>%
  mutate(in_ad34 = as.logical(st_within(geometry, ad34, sparse = FALSE)))

cat(sprintf("\nMosques in AD34: %d\n", sum(mosques_sf$in_ad34)))
cat(sprintf("Mosques nearby: %d\n", sum(!mosques_sf$in_ad34)))

# Print list
cat("\n========================================\n")
cat("MOSQUES IN AD34:\n")
cat("========================================\n")
mosques_sf %>%
  filter(in_ad34) %>%
  st_drop_geometry() %>%
  select(name, address, phone) %>%
  as.data.frame() %>%
  print()

cat("\n========================================\n")
cat("NEARBY MOSQUES (OUTSIDE AD34):\n")
cat("========================================\n")
mosques_sf %>%
  filter(!in_ad34) %>%
  st_drop_geometry() %>%
  select(name, address, phone) %>%
  as.data.frame() %>%
  print()

# Create map
cat("\nCreating map...\n")

# Separate in/out
mosques_in <- mosques_sf %>% filter(in_ad34)
mosques_out <- mosques_sf %>% filter(!in_ad34)

map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 13, cachedir = "cache/tiles") +
  geom_sf(data = ad34, fill = alpha("blue", 0.1), color = "red", linewidth = 2.5) +
  geom_sf(data = mosques_in, color = "darkgreen", size = 6, shape = 18) +
  geom_sf(data = mosques_out, color = "orange", size = 4, shape = 17) +
  geom_sf_text(
    data = mosques_in,
    aes(label = name),
    size = 2.5,
    nudge_y = 0.003,
    color = "black",
    fontface = "bold",
    check_overlap = TRUE
  ) +
  labs(
    title = "Islamic Centers & Mosques in and around Assembly District 34",
    subtitle = sprintf("Green diamonds: In AD34 (%d) | Orange triangles: Nearby (%d)",
                      nrow(mosques_in), nrow(mosques_out)),
    caption = "Sources: OSM, verified addresses, community sources | Red boundary: AD34"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/maps/mosques_comprehensive.png", map, width = 14, height = 10, dpi = 300)

cat("\n✓ Saved: outputs/maps/mosques_comprehensive.png\n")

# Save data
write.csv(
  mosques_sf %>% st_drop_geometry() %>% arrange(desc(in_ad34), name),
  "outputs/tables/mosques_comprehensive.csv",
  row.names = FALSE
)
cat("✓ Saved: outputs/tables/mosques_comprehensive.csv\n\n")
