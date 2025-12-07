library(sf)
library(tidyverse)
library(osmdata)
library(tigris)
library(ggplot2)
library(ggspatial)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("Mapping Islamic Centers in AD34\n")
cat("========================================\n\n")

# Load AD34 boundary
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_3857 <- st_transform(ad34, 3857)
ad34_4326 <- st_transform(ad34, 4326)

# Get bounding box for AD34
bbox <- st_bbox(ad34_4326)

cat("\nQuerying OpenStreetMap for Islamic places of worship...\n")

# Query OSM for mosques and Islamic centers
# Using multiple tags to capture all variations
osm_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")

# Get the data
mosques_data <- osmdata_sf(osm_query)

# Combine points and polygons
mosques_points <- mosques_data$osm_points
mosques_polygons <- mosques_data$osm_polygons

cat(sprintf("Found %d mosque points\n", nrow(mosques_points)))
cat(sprintf("Found %d mosque polygons\n", ifelse(is.null(mosques_polygons), 0, nrow(mosques_polygons))))

# Convert polygons to centroids if they exist
if (!is.null(mosques_polygons) && nrow(mosques_polygons) > 0) {
  mosques_poly_centers <- st_centroid(mosques_polygons)
  # Combine with points
  common_cols <- intersect(names(mosques_points), names(mosques_poly_centers))
  mosques_all <- bind_rows(
    mosques_points %>% select(any_of(common_cols)),
    mosques_poly_centers %>% select(any_of(common_cols))
  )
} else {
  mosques_all <- mosques_points
}

# Filter to only those within AD34
cat("\nFiltering to AD34 boundaries...\n")
mosques_all <- st_transform(mosques_all, 3857)
mosques_in_ad34 <- st_filter(mosques_all, ad34_3857, .predicate = st_within)

cat(sprintf("\n✓ Found %d Islamic centers/mosques in AD34\n\n", nrow(mosques_in_ad34)))

# Print details
if (nrow(mosques_in_ad34) > 0) {
  cat("Islamic Centers Found:\n")
  cat("========================================\n")
  for (i in 1:nrow(mosques_in_ad34)) {
    name <- ifelse(!is.na(mosques_in_ad34$name[i]), mosques_in_ad34$name[i], "Unnamed")
    cat(sprintf("%d. %s\n", i, name))
    if ("addr.street" %in% names(mosques_in_ad34) && !is.na(mosques_in_ad34$addr.street[i])) {
      cat(sprintf("   Address: %s", mosques_in_ad34$addr.street[i]))
      if ("addr.housenumber" %in% names(mosques_in_ad34) && !is.na(mosques_in_ad34$addr.housenumber[i])) {
        cat(sprintf(" %s", mosques_in_ad34$addr.housenumber[i]))
      }
      cat("\n")
    }
  }
  cat("\n")
}

# Create the map
cat("Creating map...\n")

mosques_4326 <- st_transform(mosques_in_ad34, 4326)
ad34_map <- st_transform(ad34_3857, 4326)

map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles") +
  geom_sf(data = ad34_map, fill = NA, color = "red", linewidth = 1.2) +
  geom_sf(data = mosques_4326, color = "#059669", size = 4, shape = 18) +
  geom_sf_label(
    data = mosques_4326,
    aes(label = ifelse(!is.na(name), name, "Unnamed")),
    size = 3,
    nudge_y = 0.0015,
    color = "black",
    fill = alpha("white", 0.8)
  ) +
  labs(
    title = "Islamic Centers and Mosques in Assembly District 34",
    subtitle = "Data from OpenStreetMap",
    caption = "Red boundary: AD34 | Green diamonds: Mosques/Islamic Centers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save the map
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/maps/ad34_mosques_map.png", map, width = 11, height = 8.5, dpi = 300)

cat("\n✓ Map saved to: outputs/maps/ad34_mosques_map.png\n\n")

# Save the data as CSV for reference
if (nrow(mosques_in_ad34) > 0) {
  # Get available columns
  available_cols <- intersect(
    c("name", "addr.housenumber", "addr.street", "addr.city", "addr.postcode",
      "denomination", "website", "phone", "opening_hours"),
    names(mosques_in_ad34)
  )

  mosques_df <- mosques_in_ad34 %>%
    st_drop_geometry() %>%
    select(any_of(available_cols)) %>%
    arrange(name)

  write.csv(mosques_df, "outputs/tables/ad34_mosques.csv", row.names = FALSE)
  cat("✓ Data saved to: outputs/tables/ad34_mosques.csv\n\n")
}

cat("========================================\n")
cat("Complete!\n")
cat("========================================\n")
