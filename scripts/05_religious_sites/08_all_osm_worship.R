library(sf)
library(osmdata)
library(ggplot2)
library(ggspatial)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("Querying ALL places of worship from OSM...\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)
bbox <- st_bbox(ad34)

# Expand bbox slightly
bbox_expanded <- bbox + c(-0.005, -0.005, 0.005, 0.005)

# Query ALL worship places
cat("\nQuerying all places of worship...\n")
q_all <- opq(bbox_expanded) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship")

all_worship <- osmdata_sf(q_all)

# Combine points and polygons
all_pts <- all_worship$osm_points
all_poly <- all_worship$osm_polygons

cat(sprintf("Found %d worship points\n", ifelse(is.null(all_pts), 0, nrow(all_pts))))
cat(sprintf("Found %d worship polygons\n", ifelse(is.null(all_poly), 0, nrow(all_poly))))

# Combine
worship_combined <- bind_rows(
  all_pts,
  if(!is.null(all_poly)) st_centroid(all_poly) else NULL
) %>% st_transform(4326)

# Filter to AD34
worship_ad34 <- st_filter(worship_combined, ad34)

cat(sprintf("\nTotal worship places in AD34: %d\n", nrow(worship_ad34)))

# Break down by religion
cat("\n========================================\n")
cat("BREAKDOWN BY RELIGION:\n")
cat("========================================\n")
table(worship_ad34$religion, useNA = "ifany") %>% print()

# Show all Muslim/Islamic places
cat("\n========================================\n")
cat("ISLAMIC CENTERS/MOSQUES:\n")
cat("========================================\n")
mosques <- worship_ad34 %>%
  filter(religion == "muslim" | grepl("islam|mosque|masjid", tolower(name), ignore.case = TRUE))

if (nrow(mosques) > 0) {
  for (i in 1:nrow(mosques)) {
    coords <- st_coordinates(mosques[i,])
    cat(sprintf("%d. %s\n", i,
                ifelse(is.na(mosques$name[i]), "Unnamed", mosques$name[i])))
    cat(sprintf("   Religion field: %s\n",
                ifelse(is.na(mosques$religion[i]), "Not specified", mosques$religion[i])))
    if ("addr:full" %in% names(mosques) && !is.na(mosques$`addr:full`[i])) {
      cat(sprintf("   Address: %s\n", mosques$`addr:full`[i]))
    } else if ("addr:street" %in% names(mosques) && !is.na(mosques$`addr:street`[i])) {
      addr <- mosques$`addr:street`[i]
      if ("addr:housenumber" %in% names(mosques) && !is.na(mosques$`addr:housenumber`[i])) {
        addr <- paste(mosques$`addr:housenumber`[i], addr)
      }
      cat(sprintf("   Address: %s\n", addr))
    }
    cat(sprintf("   Coords: %.6f, %.6f\n\n", coords[1], coords[2]))
  }
} else {
  cat("No mosques found\n")
}

# Create comprehensive map with ALL worship places
cat("\nCreating map...\n")

# Separate by religion
mosques_only <- worship_ad34 %>% filter(religion == "muslim")
other_worship <- worship_ad34 %>% filter(is.na(religion) | religion != "muslim")

map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles") +
  geom_sf(data = ad34, fill = NA, color = "red", linewidth = 2) +
  geom_sf(data = mosques_only, color = "darkgreen", size = 6, shape = 18, alpha = 0.9) +
  geom_sf(data = other_worship, color = "gray40", size = 3, shape = 3, alpha = 0.5) +
  geom_sf_text(
    data = mosques_only,
    aes(label = ifelse(is.na(name), "Mosque", name)),
    size = 2.5,
    nudge_y = 0.002,
    color = "darkgreen",
    fontface = "bold",
    check_overlap = TRUE
  ) +
  labs(
    title = "Places of Worship in Assembly District 34 (OpenStreetMap)",
    subtitle = sprintf("Green diamonds: Mosques (%d) | Gray crosses: Other religious sites (%d)",
                      nrow(mosques_only), nrow(other_worship)),
    caption = "Data from OpenStreetMap | Red boundary: AD34"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("outputs/maps/all_worship_osm.png", map, width = 11, height = 8.5, dpi = 300)
cat("✓ Saved: outputs/maps/all_worship_osm.png\n")

# Save data
worship_ad34 %>%
  st_drop_geometry() %>%
  select(name, religion, `addr:street`, `addr:housenumber`, phone, website) %>%
  write.csv("outputs/tables/all_worship_osm.csv", row.names = FALSE)
cat("✓ Saved: outputs/tables/all_worship_osm.csv\n\n")
