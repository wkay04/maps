# ============================================================
# RELIGIOUS INSTITUTIONS PROXIMITY ANALYSIS
# ============================================================
# Uses OpenStreetMap data on actual mosques, temples, synagogues
# to measure proximity and density of religious institutions
# by Election District
#
# BETTER THAN POPULATION PROXIES BECAUSE:
# - Religious communities tend to live near their places of worship
# - Actual mosque/temple/synagogue locations are observable data
# - Can measure both distance and density
#
# STILL HAS LIMITATIONS:
# - Assumes proximity = population (not perfect but reasonable)
# - OSM data completeness varies
# - Some people live far from their place of worship
# ============================================================

library(tidyverse)
library(sf)
library(osmdata)

setwd("~/Mayoral Results AD34")

cat("\n🕌 RELIGIOUS INSTITUTIONS PROXIMITY ANALYSIS\n")
cat("============================================\n\n")

# ============================================================
# LOAD ELECTION DISTRICT BOUNDARIES
# ============================================================

cat("📍 Loading Election District boundaries...\n")

shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_ed_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(4326)  # OSM uses WGS84

# Calculate ED centroids
ed_centroids <- ad34_ed_shp %>%
  st_centroid() %>%
  st_transform(3857)  # For distance calculations

cat("  Loaded", nrow(ad34_ed_shp), "Election Districts\n\n")

# Get bounding box for AD34 (expanded slightly for buffer)
bbox <- st_bbox(ad34_ed_shp)
bbox_expanded <- bbox + c(-0.01, -0.01, 0.01, 0.01)

# ============================================================
# DOWNLOAD OPENSTREETMAP DATA
# ============================================================

cat("📥 Downloading places of worship from OpenStreetMap...\n")
cat("   (This may take a minute...)\n\n")

# Query OSM for places of worship
osm_query <- opq(bbox = bbox_expanded) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship")

# Download data
tryCatch({
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

}, error = function(e) {
  cat("  ⚠️  Error downloading OSM data:", e$message, "\n")
  cat("  Will use cached data if available\n")
  worship_all <- NULL
})

if (is.null(worship_all) || nrow(worship_all) == 0) {
  cat("\n❌ No OSM data available. Exiting.\n")
  quit(save = "no", status = 1)
}

# ============================================================
# CATEGORIZE BY RELIGION TYPE
# ============================================================

cat("\n🏛️  Categorizing places of worship by religion...\n\n")

# Classify by religion tag
worship_categorized <- worship_all %>%
  mutate(
    religion_type = case_when(
      str_detect(tolower(religion), "muslim|islam") ~ "Muslim",
      str_detect(tolower(religion), "hindu") ~ "Hindu",
      str_detect(tolower(religion), "jewish|judaism") ~ "Jewish",
      str_detect(tolower(religion), "christian") ~ "Christian",
      str_detect(tolower(religion), "buddhist") ~ "Buddhist",
      str_detect(tolower(religion), "sikh") ~ "Sikh",
      TRUE ~ "Other/Unknown"
    )
  ) %>%
  st_transform(3857)  # Transform to meters for distance calculations

# Summary by type
religion_summary <- worship_categorized %>%
  st_drop_geometry() %>%
  count(religion_type) %>%
  arrange(desc(n))

cat("Places of worship by religion:\n")
for (i in 1:nrow(religion_summary)) {
  cat(sprintf("  %-20s %4d\n",
              religion_summary$religion_type[i],
              religion_summary$n[i]))
}

cat("\n")

# ============================================================
# CALCULATE DISTANCE METRICS
# ============================================================

cat("📏 Calculating distance to nearest place of worship...\n\n")

# Function to calculate distance to nearest institution of type
calc_nearest_distance <- function(ed_cents, institutions) {
  if (nrow(institutions) == 0) {
    return(rep(NA_real_, nrow(ed_cents)))
  }

  distances <- st_distance(ed_cents, institutions)
  apply(distances, 1, min)
}

# Calculate for each religion type
distance_metrics <- ed_centroids %>%
  st_drop_geometry() %>%
  select(ED, ED_num)

# Muslim institutions
muslim_places <- worship_categorized %>% filter(religion_type == "Muslim")
distance_metrics$dist_to_mosque <- as.numeric(
  calc_nearest_distance(ed_centroids, muslim_places)
)

# Hindu institutions
hindu_places <- worship_categorized %>% filter(religion_type == "Hindu")
distance_metrics$dist_to_temple <- as.numeric(
  calc_nearest_distance(ed_centroids, hindu_places)
)

# Jewish institutions
jewish_places <- worship_categorized %>% filter(religion_type == "Jewish")
distance_metrics$dist_to_synagogue <- as.numeric(
  calc_nearest_distance(ed_centroids, jewish_places)
)

# Christian institutions
christian_places <- worship_categorized %>% filter(religion_type == "Christian")
distance_metrics$dist_to_church <- as.numeric(
  calc_nearest_distance(ed_centroids, christian_places)
)

cat("Distance metrics calculated\n\n")

# ============================================================
# CALCULATE DENSITY METRICS (COUNT WITHIN RADIUS)
# ============================================================

cat("📊 Calculating density (count within 800m radius)...\n\n")

# Function to count institutions within radius
count_within_radius <- function(ed_cents, institutions, radius_m = 800) {
  if (nrow(institutions) == 0) {
    return(rep(0, nrow(ed_cents)))
  }

  # Create buffer around each ED centroid
  ed_buffers <- st_buffer(ed_cents, dist = radius_m)

  # Count intersections
  intersections <- st_intersects(ed_buffers, institutions)
  sapply(intersections, length)
}

# Calculate density for each type
distance_metrics$mosques_nearby <- count_within_radius(
  ed_centroids, muslim_places, radius_m = 800
)

distance_metrics$temples_nearby <- count_within_radius(
  ed_centroids, hindu_places, radius_m = 800
)

distance_metrics$synagogues_nearby <- count_within_radius(
  ed_centroids, jewish_places, radius_m = 800
)

distance_metrics$churches_nearby <- count_within_radius(
  ed_centroids, christian_places, radius_m = 800
)

cat("Density metrics calculated\n\n")

# ============================================================
# MERGE WITH ELECTION RESULTS
# ============================================================

cat("🗳️  Merging with election results...\n")

election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, Sliwa_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

analysis_data <- distance_metrics %>%
  left_join(election_results, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct))

cat("  Merged data for", nrow(analysis_data), "EDs\n\n")

# ============================================================
# CORRELATION ANALYSIS
# ============================================================

cat("📊 CORRELATION ANALYSIS: PROXIMITY TO RELIGIOUS INSTITUTIONS\n")
cat("=============================================================\n\n")

# Distance correlations (negative = closer = higher support)
cat("DISTANCE TO NEAREST (negative r = closer areas have higher Mamdani support):\n")
cat(sprintf("  %-30s r = %7.3f\n",
            "Distance to nearest mosque:",
            cor(analysis_data$dist_to_mosque, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Distance to nearest temple:",
            cor(analysis_data$dist_to_temple, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Distance to nearest synagogue:",
            cor(analysis_data$dist_to_synagogue, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Distance to nearest church:",
            cor(analysis_data$dist_to_church, analysis_data$Mamdani_pct,
                use = "complete.obs")))

cat("\nDENSITY (positive r = more institutions nearby = higher Mamdani support):\n")
cat(sprintf("  %-30s r = %7.3f\n",
            "Mosques within 800m:",
            cor(analysis_data$mosques_nearby, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Temples within 800m:",
            cor(analysis_data$temples_nearby, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Synagogues within 800m:",
            cor(analysis_data$synagogues_nearby, analysis_data$Mamdani_pct,
                use = "complete.obs")))
cat(sprintf("  %-30s r = %7.3f\n",
            "Churches within 800m:",
            cor(analysis_data$churches_nearby, analysis_data$Mamdani_pct,
                use = "complete.obs")))

cat("\n")

# ============================================================
# TOP/BOTTOM EDs
# ============================================================

cat("🕌 EDs CLOSEST TO MOSQUES:\n")
cat(sprintf("%-8s %15s %15s %12s\n", "ED", "Dist (m)", "Mosques 800m", "Mamdani %"))
cat(rep("-", 60), "\n")

closest_mosque <- analysis_data %>%
  filter(!is.na(dist_to_mosque)) %>%
  arrange(dist_to_mosque) %>%
  head(10)

for (i in 1:nrow(closest_mosque)) {
  cat(sprintf("%-8s %15.0f %15d %12.1f\n",
              closest_mosque$ED[i],
              closest_mosque$dist_to_mosque[i],
              closest_mosque$mosques_nearby[i],
              closest_mosque$Mamdani_pct[i]))
}

cat("\n🛕 EDs CLOSEST TO TEMPLES:\n")
cat(sprintf("%-8s %15s %15s %12s\n", "ED", "Dist (m)", "Temples 800m", "Mamdani %"))
cat(rep("-", 60), "\n")

closest_temple <- analysis_data %>%
  filter(!is.na(dist_to_temple)) %>%
  arrange(dist_to_temple) %>%
  head(10)

for (i in 1:nrow(closest_temple)) {
  cat(sprintf("%-8s %15.0f %15d %12.1f\n",
              closest_temple$ED[i],
              closest_temple$dist_to_temple[i],
              closest_temple$temples_nearby[i],
              closest_temple$Mamdani_pct[i]))
}

cat("\n✡️  EDs CLOSEST TO SYNAGOGUES:\n")
cat(sprintf("%-8s %15s %15s %12s\n", "ED", "Dist (m)", "Synagogues 800m", "Mamdani %"))
cat(rep("-", 60), "\n")

closest_synagogue <- analysis_data %>%
  filter(!is.na(dist_to_synagogue)) %>%
  arrange(dist_to_synagogue) %>%
  head(10)

for (i in 1:nrow(closest_synagogue)) {
  cat(sprintf("%-8s %15.0f %15d %12.1f\n",
              closest_synagogue$ED[i],
              closest_synagogue$dist_to_synagogue[i],
              closest_synagogue$synagogues_nearby[i],
              closest_synagogue$Mamdani_pct[i]))
}

cat("\n")

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 SAVING RESULTS\n")
cat("==================\n\n")

dir.create("outputs/analysis/corrected", showWarnings = FALSE, recursive = TRUE)

write_csv(analysis_data, "outputs/analysis/corrected/religious_proximity_by_ed.csv")
cat("  ✓ Saved: religious_proximity_by_ed.csv\n")

# Save institution locations
worship_save <- worship_categorized %>%
  st_transform(4326) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

write_csv(worship_save, "outputs/analysis/corrected/places_of_worship_osm.csv")
cat("  ✓ Saved: places_of_worship_osm.csv\n")

cat("\n✅ RELIGIOUS INSTITUTIONS PROXIMITY ANALYSIS COMPLETE!\n")
cat("   Better proxy than population data because:\n")
cat("   - Religious communities cluster near their places of worship\n")
cat("   - Observable institutional data vs demographic estimates\n")
