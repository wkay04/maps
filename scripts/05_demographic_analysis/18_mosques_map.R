# ============================================================
# MOSQUES MAP FOR ASSEMBLY DISTRICT 34
# ============================================================
# Creates a map showing all mosques and Islamic centers in AD34
# Uses OpenStreetMap data for accurate location information
# ============================================================

library(tidyverse)
library(sf)
library(osmdata)
library(tigris)
library(ggspatial)
library(viridis)
library(ggrepel)

# Set OSM tile cache directory
options(osmdata_cachedir = "cache")

setwd("~/Mayoral Results AD34")

cat("\n🕌 MOSQUES MAP - ASSEMBLY DISTRICT 34\n")
cat("======================================\n\n")

# ============================================================
# LOAD AD34 BOUNDARY
# ============================================================

cat("📍 Loading Assembly District 34 boundary...\n")

sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>%
  filter(NAMELSAD == "Assembly District 34") %>%
  st_transform(4326)  # OSM uses WGS84

# Get bounding box for AD34 (expanded to capture nearby institutions)
bbox <- st_bbox(ad34)
bbox_expanded <- bbox + c(-0.015, -0.015, 0.015, 0.015)

# Transform AD34 to Web Mercator for better OSM tile rendering
ad34_mercator <- st_transform(ad34, 3857)

cat("  Loaded AD34 boundary\n\n")

# No need to load separate street data - using OSM tiles

# ============================================================
# DOWNLOAD MOSQUE DATA FROM OPENSTREETMAP
# ============================================================

cat("📥 Downloading places of worship from OpenStreetMap...\n")
cat("   (This may take a minute...)\n\n")

# Query OSM for places of worship
osm_query <- opq(bbox = bbox_expanded) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship")

# Download data
places_of_worship <- osmdata_sf(osm_query)

# Combine points and polygons (convert polygons to centroids)
worship_points <- places_of_worship$osm_points
worship_polygons <- places_of_worship$osm_polygons

if (!is.null(worship_polygons) && nrow(worship_polygons) > 0) {
  worship_poly_cents <- st_centroid(worship_polygons)
  worship_all <- bind_rows(
    worship_points %>% select(osm_id, name, religion, denomination, geometry),
    worship_poly_cents %>% select(osm_id, name, religion, denomination, geometry)
  )
} else {
  worship_all <- worship_points %>%
    select(osm_id, name, religion, denomination, geometry)
}

cat("  Downloaded", nrow(worship_all), "total places of worship\n")

# Filter for Islamic institutions
mosques_all <- worship_all %>%
  filter(
    # By religion tag
    str_detect(tolower(coalesce(religion, "")), "muslim|islam") |
    # By name (various transliterations and terms)
    str_detect(tolower(coalesce(name, "")), "mosque|masjid|masijd|masgid|islamic|muslim|jama|jamia|jamaat") |
    # By denomination
    str_detect(tolower(coalesce(denomination, "")), "sunni|shia|sufi|ahmadi")
  ) %>%
  st_transform(4326) %>%
  mutate(
    # Mark which are in AD34
    in_ad34 = lengths(st_intersects(., ad34)) > 0,
    # Create display name
    display_name = case_when(
      !is.na(name) & name != "" ~ name,
      TRUE ~ "Islamic Center"
    )
  )

cat("  Found", nrow(mosques_all), "Islamic institutions in search area\n")

# ============================================================
# SEPARATE MOSQUES BY LOCATION
# ============================================================

cat("\n🕌 Organizing Islamic institutions...\n\n")

mosques_in_ad34 <- mosques_all %>% filter(in_ad34)
mosques_nearby <- mosques_all %>% filter(!in_ad34)

cat("  -", nrow(mosques_in_ad34), "within Assembly District 34\n")
cat("  -", nrow(mosques_nearby), "in nearby areas\n")

# Print names of mosques found in AD34
if (nrow(mosques_in_ad34) > 0) {
  cat("\nIslamic institutions in AD34:\n")
  for (i in 1:nrow(mosques_in_ad34)) {
    cat(sprintf("  %2d. %s\n", i, mosques_in_ad34$display_name[i]))
  }
}

# Print nearby mosques
if (nrow(mosques_nearby) > 0) {
  cat("\nNearby Islamic institutions (outside AD34):\n")
  for (i in 1:min(10, nrow(mosques_nearby))) {
    cat(sprintf("  %2d. %s\n", i, mosques_nearby$display_name[i]))
  }
  if (nrow(mosques_nearby) > 10) {
    cat(sprintf("  ... and %d more\n", nrow(mosques_nearby) - 10))
  }
}
cat("\n")

# ============================================================
# CREATE MAP
# ============================================================

cat("🗺️  Generating map...\n\n")

# Create directory for output
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# Prepare label coordinates
label_data_in <- mosques_in_ad34 %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

label_data_nearby <- mosques_nearby %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# Create map with OSM base layer
mosque_map <- ggplot() +
  # OSM base layer - higher zoom for street detail
  annotation_map_tile(type = "osm", zoomin = 1, cachedir = "cache") +

  # Nearby mosques (lighter color, smaller)
  geom_sf(data = mosques_nearby,
          color = "gray40",
          fill = "gray80",
          shape = 21,
          size = 2.5,
          stroke = 1,
          alpha = 0.6) +

  # AD34 boundary
  geom_sf(data = ad34,
          fill = NA,
          color = "red",
          linewidth = 1,
          linetype = "solid") +

  # Mosques in AD34 (prominent)
  geom_sf(data = mosques_in_ad34,
          color = "darkgreen",
          fill = "lightgreen",
          shape = 21,
          size = 5,
          stroke = 2,
          alpha = 0.9) +

  # Labels for mosques in AD34 (prominent, cleaner)
  geom_label_repel(
    data = label_data_in,
    aes(x = lon, y = lat, label = display_name),
    size = 4,
    fontface = "bold",
    color = "darkgreen",
    fill = alpha("white", 0.9),
    label.padding = unit(0.25, "lines"),
    box.padding = 0.8,
    point.padding = 0.5,
    min.segment.length = 0,
    max.overlaps = 25,
    force = 3,
    seed = 42,
    segment.color = "darkgreen",
    segment.size = 0.5
  ) +

  # Labels for nearby mosques (smaller, less prominent)
  geom_label_repel(
    data = label_data_nearby,
    aes(x = lon, y = lat, label = display_name),
    size = 2.8,
    color = "gray20",
    fill = alpha("white", 0.85),
    label.padding = unit(0.2, "lines"),
    box.padding = 0.5,
    point.padding = 0.3,
    min.segment.length = 0.2,
    max.overlaps = 20,
    force = 1.5,
    seed = 42,
    segment.color = "gray50",
    segment.size = 0.3
  ) +

  # Labels
  labs(
    title = "Islamic Centers and Mosques in and near Assembly District 34",
    subtitle = "Queens, NY (Jackson Heights, Corona, East Elmhurst, Astoria)\nGreen = Within AD34 | Gray = Nearby",
    caption = "Data: OpenStreetMap & Census TIGER/Line | Boundary: NYS Assembly District 34 (2025)"
  ) +

  # Theming
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", lineheight = 1.2),
    plot.caption = element_text(size = 8, color = "gray50"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +

  # Add scale bar and north arrow
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in"),
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "in"),
    width = unit(0.8, "in")
  ) +

  # Set coordinate system
  coord_sf(crs = 4326,
           xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
           ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
           expand = TRUE)

# ============================================================
# SAVE MAP
# ============================================================

cat("💾 Saving map...\n")

# Save as high-resolution PNG
ggsave(
  "outputs/maps/ad34_mosques_map.png",
  plot = mosque_map,
  width = 14,
  height = 11,
  dpi = 200,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/ad34_mosques_map.png\n")

# Also save mosque locations as CSV
mosques_save_in <- mosques_in_ad34 %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    location = "In AD34"
  ) %>%
  st_drop_geometry() %>%
  select(location, display_name, name, religion, denomination, lon, lat)

mosques_save_nearby <- mosques_nearby %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    location = "Nearby"
  ) %>%
  st_drop_geometry() %>%
  select(location, display_name, name, religion, denomination, lon, lat)

mosques_save_all <- bind_rows(mosques_save_in, mosques_save_nearby)

write_csv(mosques_save_all, "outputs/tables/ad34_mosques_locations.csv")
cat("  ✓ Saved: outputs/tables/ad34_mosques_locations.csv\n")

cat("\n✅ ISLAMIC CENTERS MAP COMPLETE!\n")
cat("   View the map at: outputs/maps/ad34_mosques_map.png\n")
cat("   Found", nrow(mosques_in_ad34), "Islamic institutions in Assembly District 34\n")
cat("   Found", nrow(mosques_nearby), "additional institutions in nearby areas\n\n")
