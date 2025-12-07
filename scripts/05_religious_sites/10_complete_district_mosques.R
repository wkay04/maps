library(sf)
library(osmdata)
library(ggplot2)
library(ggspatial)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("COMPLETE AD34 MOSQUE MAP - FULL DISTRICT\n")
cat("========================================\n\n")

# Load AD34
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)

# Get full bbox and expand significantly to ensure we don't miss anything
bbox <- st_bbox(ad34)
cat("AD34 Bounding Box:\n")
cat(sprintf("Longitude: %.6f to %.6f\n", bbox[1], bbox[3]))
cat(sprintf("Latitude:  %.6f to %.6f\n", bbox[2], bbox[4]))

# Expand by 0.02 degrees (~2km) to catch everything nearby
bbox_large <- bbox + c(-0.02, -0.02, 0.02, 0.02)
cat("\nExpanded search area (with buffer):\n")
cat(sprintf("Longitude: %.6f to %.6f\n", bbox_large[1], bbox_large[3]))
cat(sprintf("Latitude:  %.6f to %.6f\n\n", bbox_large[2], bbox_large[4]))

# Query 1: amenity=place_of_worship + religion=muslim
cat("Query 1: amenity=place_of_worship + religion=muslim\n")
q1 <- opq(bbox_large) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")
osm1 <- osmdata_sf(q1)
cat(sprintf("  Points: %d, Polygons: %d\n",
            ifelse(is.null(osm1$osm_points), 0, nrow(osm1$osm_points)),
            ifelse(is.null(osm1$osm_polygons), 0, nrow(osm1$osm_polygons))))

# Query 2: building=mosque
cat("Query 2: building=mosque\n")
q2 <- opq(bbox_large) %>%
  add_osm_feature(key = "building", value = "mosque")
osm2 <- osmdata_sf(q2)
cat(sprintf("  Points: %d, Polygons: %d\n",
            ifelse(is.null(osm2$osm_points), 0, nrow(osm2$osm_points)),
            ifelse(is.null(osm2$osm_polygons), 0, nrow(osm2$osm_polygons))))

# Combine all
all_results <- list()
for (osm in list(osm1, osm2)) {
  if (!is.null(osm$osm_points) && nrow(osm$osm_points) > 0) {
    all_results[[length(all_results) + 1]] <- osm$osm_points
  }
  if (!is.null(osm$osm_polygons) && nrow(osm$osm_polygons) > 0) {
    all_results[[length(all_results) + 1]] <- st_centroid(osm$osm_polygons)
  }
}

# Standardize and combine
all_cols <- unique(unlist(lapply(all_results, names)))
all_results_std <- lapply(all_results, function(df) {
  for (col in setdiff(all_cols, names(df))) {
    df[[col]] <- NA
  }
  df %>% select(all_of(all_cols))
})

mosques_all <- bind_rows(all_results_std) %>%
  st_transform(4326) %>%
  distinct(geometry, .keep_all = TRUE)

cat(sprintf("\nTotal unique locations found: %d\n", nrow(mosques_all)))

# Separate IN vs NEAR AD34
mosques_all$in_ad34 <- as.logical(st_within(mosques_all, ad34, sparse = FALSE))

mosques_in <- mosques_all %>% filter(in_ad34)
mosques_near <- mosques_all %>% filter(!in_ad34)

cat(sprintf("IN AD34: %d\n", nrow(mosques_in)))
cat(sprintf("NEAR AD34: %d\n", nrow(mosques_near)))

# Print IN AD34
cat("\n========================================\n")
cat("MOSQUES IN AD34:\n")
cat("========================================\n")
for (i in 1:nrow(mosques_in)) {
  coords <- st_coordinates(mosques_in[i,])
  name <- ifelse(is.na(mosques_in$name[i]), "Unnamed", mosques_in$name[i])
  cat(sprintf("%d. %s\n", i, name))

  if ("addr:street" %in% names(mosques_in) && !is.na(mosques_in$`addr:street`[i])) {
    addr <- mosques_in$`addr:street`[i]
    if ("addr:housenumber" %in% names(mosques_in) && !is.na(mosques_in$`addr:housenumber`[i])) {
      addr <- paste(mosques_in$`addr:housenumber`[i], addr)
    }
    cat(sprintf("   Address: %s\n", addr))
  }
  cat(sprintf("   Location: %.6f, %.6f\n", coords[1], coords[2]))
  cat("\n")
}

# Print NEAR AD34 (within buffer)
cat("========================================\n")
cat("MOSQUES NEAR AD34 (within 2km):\n")
cat("========================================\n")
for (i in 1:nrow(mosques_near)) {
  coords <- st_coordinates(mosques_near[i,])
  name <- ifelse(is.na(mosques_near$name[i]), "Unnamed", mosques_near$name[i])
  cat(sprintf("%d. %s\n", i, name))

  if ("addr:street" %in% names(mosques_near) && !is.na(mosques_near$`addr:street`[i])) {
    addr <- mosques_near$`addr:street`[i]
    if ("addr:housenumber" %in% names(mosques_near) && !is.na(mosques_near$`addr:housenumber`[i])) {
      addr <- paste(mosques_near$`addr:housenumber`[i], addr)
    }
    cat(sprintf("   Address: %s\n", addr))
  }
  cat(sprintf("   Location: %.6f, %.6f\n", coords[1], coords[2]))
  cat("\n")
}

# Create comprehensive map
cat("Creating map...\n")

map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 13, cachedir = "cache/tiles") +
  geom_sf(data = ad34, fill = alpha("blue", 0.08), color = "red", linewidth = 3) +
  geom_sf(data = mosques_in, color = "darkgreen", size = 8, shape = 18) +
  geom_sf(data = mosques_near, color = "orange", size = 5, shape = 17, alpha = 0.7) +
  geom_sf_text(
    data = mosques_in,
    aes(label = ifelse(is.na(name), "", name)),
    size = 2.8,
    nudge_y = 0.003,
    color = "black",
    fontface = "bold",
    check_overlap = TRUE
  ) +
  geom_sf_text(
    data = mosques_near,
    aes(label = ifelse(is.na(name), "", name)),
    size = 2.2,
    nudge_y = 0.003,
    color = "darkred",
    fontface = "italic",
    check_overlap = TRUE
  ) +
  labs(
    title = "Mosques and Islamic Centers - Assembly District 34 (Complete)",
    subtitle = sprintf("Green diamonds: In AD34 (%d) | Orange triangles: Nearby (%d)",
                      nrow(mosques_in), nrow(mosques_near)),
    caption = "Data: OpenStreetMap (amenity=place_of_worship+religion=muslim, building=mosque)\nRed boundary: AD34 exact | Search radius: 2km"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 9),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("outputs/maps/mosques_complete_district.png", map, width = 16, height = 12, dpi = 300)
cat("\n✓ SAVED: outputs/maps/mosques_complete_district.png\n")

# Save data
bind_rows(
  mosques_in %>% mutate(location = "IN AD34"),
  mosques_near %>% mutate(location = "NEAR AD34")
) %>%
  st_drop_geometry() %>%
  select(name, location, religion, building, `addr:street`, `addr:housenumber`, phone) %>%
  write.csv("outputs/tables/mosques_complete_district.csv", row.names = FALSE)
cat("✓ SAVED: outputs/tables/mosques_complete_district.csv\n\n")

cat("========================================\n")
cat("COMPLETE!\n")
cat("========================================\n")
