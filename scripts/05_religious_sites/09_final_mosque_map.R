library(sf)
library(osmdata)
library(ggplot2)
library(ggspatial)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("FINAL COMPREHENSIVE MOSQUE MAP\n")
cat("========================================\n\n")

# Load AD34
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)
bbox <- st_bbox(ad34)
bbox_exp <- bbox + c(-0.01, -0.01, 0.01, 0.01)

# Query 1: amenity=place_of_worship + religion=muslim
cat("Query 1: amenity=place_of_worship + religion=muslim\n")
q1 <- opq(bbox_exp) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")
osm1 <- osmdata_sf(q1)

# Query 2: building=mosque
cat("Query 2: building=mosque\n")
q2 <- opq(bbox_exp) %>%
  add_osm_feature(key = "building", value = "mosque")
osm2 <- osmdata_sf(q2)

# Query 3: name contains mosque/masjid/islamic
cat("Query 3: name contains 'mosque' or 'masjid'\n")
q3 <- opq(bbox_exp) %>%
  add_osm_feature(key = "name", value = "mosque", value_exact = FALSE, match_case = FALSE)
osm3 <- osmdata_sf(q3)

q4 <- opq(bbox_exp) %>%
  add_osm_feature(key = "name", value = "masjid", value_exact = FALSE, match_case = FALSE)
osm4 <- osmdata_sf(q4)

q5 <- opq(bbox_exp) %>%
  add_osm_feature(key = "name", value = "islamic", value_exact = FALSE, match_case = FALSE)
osm5 <- osmdata_sf(q5)

# Combine all results
all_results <- list()

# Add points
for (osm in list(osm1, osm2, osm3, osm4, osm5)) {
  if (!is.null(osm$osm_points) && nrow(osm$osm_points) > 0) {
    all_results[[length(all_results) + 1]] <- osm$osm_points
  }
}

# Add polygons (as centroids)
for (osm in list(osm1, osm2, osm3, osm4, osm5)) {
  if (!is.null(osm$osm_polygons) && nrow(osm$osm_polygons) > 0) {
    all_results[[length(all_results) + 1]] <- st_centroid(osm$osm_polygons)
  }
}

cat(sprintf("\nCollected %d result sets\n", length(all_results)))

# Find common columns
if (length(all_results) > 0) {
  all_cols <- unique(unlist(lapply(all_results, names)))

  # Standardize
  all_results_std <- lapply(all_results, function(df) {
    for (col in setdiff(all_cols, names(df))) {
      df[[col]] <- NA
    }
    df %>% select(all_of(all_cols))
  })

  # Combine
  mosques_all <- bind_rows(all_results_std) %>%
    st_transform(4326) %>%
    distinct(geometry, .keep_all = TRUE)

  # Filter to AD34
  mosques_ad34 <- st_filter(mosques_all, ad34)

  cat(sprintf("Found %d unique mosque locations in AD34\n", nrow(mosques_ad34)))

  # Print details
  cat("\n========================================\n")
  cat("MOSQUES FOUND:\n")
  cat("========================================\n")

  for (i in 1:nrow(mosques_ad34)) {
    coords <- st_coordinates(mosques_ad34[i,])
    name <- ifelse(is.na(mosques_ad34$name[i]), "Unnamed", mosques_ad34$name[i])

    cat(sprintf("%d. %s\n", i, name))

    # Religion tag
    if ("religion" %in% names(mosques_ad34) && !is.na(mosques_ad34$religion[i])) {
      cat(sprintf("   Religion: %s\n", mosques_ad34$religion[i]))
    }

    # Building type
    if ("building" %in% names(mosques_ad34) && !is.na(mosques_ad34$building[i])) {
      cat(sprintf("   Building: %s\n", mosques_ad34$building[i]))
    }

    # Amenity
    if ("amenity" %in% names(mosques_ad34) && !is.na(mosques_ad34$amenity[i])) {
      cat(sprintf("   Amenity: %s\n", mosques_ad34$amenity[i]))
    }

    # Address
    if ("addr:street" %in% names(mosques_ad34) && !is.na(mosques_ad34$`addr:street`[i])) {
      addr <- mosques_ad34$`addr:street`[i]
      if ("addr:housenumber" %in% names(mosques_ad34) && !is.na(mosques_ad34$`addr:housenumber`[i])) {
        addr <- paste(mosques_ad34$`addr:housenumber`[i], addr)
      }
      cat(sprintf("   Address: %s\n", addr))
    }

    cat(sprintf("   Coords: %.6f, %.6f\n", coords[1], coords[2]))
    cat("\n")
  }

  # Create map
  cat("Creating final map...\n")

  map <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles") +
    geom_sf(data = ad34, fill = alpha("blue", 0.05), color = "red", linewidth = 2.5) +
    geom_sf(data = mosques_ad34, color = "darkgreen", size = 8, shape = 18) +
    geom_sf_text(
      data = mosques_ad34,
      aes(label = ifelse(is.na(name), "Mosque", name)),
      size = 3,
      nudge_y = 0.0025,
      color = "black",
      fontface = "bold",
      check_overlap = FALSE
    ) +
    labs(
      title = "Mosques and Islamic Centers in Assembly District 34",
      subtitle = sprintf("Complete OpenStreetMap data - %d locations found", nrow(mosques_ad34)),
      caption = "Data: OpenStreetMap (amenity=place_of_worship+religion=muslim, building=mosque, name search)\nRed boundary: AD34 | Green diamonds: Mosques"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 8),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  ggsave("outputs/maps/mosques_final.png", map, width = 14, height = 10, dpi = 300)
  cat("\n✓ SAVED: outputs/maps/mosques_final.png\n")

  # Save data
  mosques_ad34 %>%
    st_drop_geometry() %>%
    select(name, religion, building, amenity, `addr:street`, `addr:housenumber`, phone, website) %>%
    write.csv("outputs/tables/mosques_final.csv", row.names = FALSE)
  cat("✓ SAVED: outputs/tables/mosques_final.csv\n\n")

} else {
  cat("ERROR: No results found\n")
}

cat("========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
