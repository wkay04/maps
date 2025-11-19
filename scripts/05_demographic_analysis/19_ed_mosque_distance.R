# ============================================================
# ELECTION DISTRICT TO MOSQUE DISTANCE ANALYSIS
# ============================================================
# Calculates distance from each ED to nearest mosque
# ============================================================

library(tidyverse)
library(sf)
library(units)

setwd("~/Mayoral Results AD34")

cat("\n📏 ED TO MOSQUE DISTANCE ANALYSIS\n")
cat("===================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📂 Loading data...\n")

# Load mosque locations
mosques <- read_csv("outputs/tables/ad34_mosques_locations.csv", show_col_types = FALSE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)  # Use meters for distance calculation

cat("  Loaded", nrow(mosques), "mosques\n")

# Load Election District boundaries
ed_files <- list.files("data/raw/election", pattern = "Election_Districts\\.shp$",
                       full.names = TRUE, recursive = TRUE)

if (length(ed_files) == 0) {
  stop("Election District shapefile not found!")
}

eds <- st_read(ed_files[1], quiet = TRUE) %>%
  st_transform(3857)

cat("  Loaded", nrow(eds), "Election Districts\n")

# Filter to Queens County if needed
if ("COUNTY" %in% names(eds)) {
  eds <- eds %>% filter(str_detect(COUNTY, "Queens|41"))
  cat("  Filtered to", nrow(eds), "Queens EDs\n")
}

cat("\n")

# ============================================================
# CALCULATE DISTANCES
# ============================================================

cat("📐 Calculating distances to nearest mosque...\n\n")

# Get ED centroids
ed_centroids <- st_centroid(eds)

# Calculate distance from each ED to nearest mosque
ed_distances <- ed_centroids %>%
  mutate(
    # Find nearest mosque
    nearest_mosque_dist = st_distance(geometry, mosques, by_element = FALSE) %>%
      apply(1, min) %>%
      set_units("m") %>%
      set_units("km"),
    # Convert to numeric km
    distance_km = as.numeric(nearest_mosque_dist),
    distance_miles = distance_km * 0.621371
  )

# Join back to full ED data
eds_with_distance <- eds %>%
  mutate(row_id = row_number()) %>%
  st_drop_geometry() %>%
  left_join(
    ed_distances %>%
      mutate(row_id = row_number()) %>%
      st_drop_geometry() %>%
      select(row_id, distance_km, distance_miles),
    by = "row_id"
  ) %>%
  select(-row_id)

cat("✓ Calculated distances for", nrow(eds_with_distance), "Election Districts\n\n")

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("📊 DISTANCE STATISTICS:\n")
cat("========================\n\n")

summary_stats <- eds_with_distance %>%
  summarize(
    min_km = min(distance_km, na.rm = TRUE),
    q25_km = quantile(distance_km, 0.25, na.rm = TRUE),
    median_km = median(distance_km, na.rm = TRUE),
    mean_km = mean(distance_km, na.rm = TRUE),
    q75_km = quantile(distance_km, 0.75, na.rm = TRUE),
    max_km = max(distance_km, na.rm = TRUE),
    min_mi = min(distance_miles, na.rm = TRUE),
    median_mi = median(distance_miles, na.rm = TRUE),
    mean_mi = mean(distance_miles, na.rm = TRUE),
    max_mi = max(distance_miles, na.rm = TRUE)
  )

cat(sprintf("Minimum:    %.2f km (%.2f miles)\n", summary_stats$min_km, summary_stats$min_mi))
cat(sprintf("25th %%ile: %.2f km\n", summary_stats$q25_km))
cat(sprintf("Median:     %.2f km (%.2f miles)\n", summary_stats$median_km, summary_stats$median_mi))
cat(sprintf("Mean:       %.2f km (%.2f miles)\n", summary_stats$mean_km, summary_stats$mean_mi))
cat(sprintf("75th %%ile: %.2f km\n", summary_stats$q75_km))
cat(sprintf("Maximum:    %.2f km (%.2f miles)\n\n", summary_stats$max_km, summary_stats$max_mi))

# Distance categories
distance_categories <- eds_with_distance %>%
  mutate(
    category = case_when(
      distance_km < 0.5 ~ "< 0.5 km (walkable)",
      distance_km < 1.0 ~ "0.5-1.0 km",
      distance_km < 2.0 ~ "1.0-2.0 km",
      distance_km < 5.0 ~ "2.0-5.0 km",
      TRUE ~ "> 5.0 km"
    )
  ) %>%
  count(category) %>%
  arrange(category)

cat("DISTANCE DISTRIBUTION:\n")
cat("----------------------\n")
for (i in 1:nrow(distance_categories)) {
  cat(sprintf("%-20s: %4d EDs (%.1f%%)\n",
              distance_categories$category[i],
              distance_categories$n[i],
              100 * distance_categories$n[i] / nrow(eds_with_distance)))
}
cat("\n")

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 Saving results...\n")

# Save full results
write_csv(eds_with_distance, "outputs/tables/ed_mosque_distances.csv")
cat("  ✓ Saved: outputs/tables/ed_mosque_distances.csv\n")

# Save summary
write_csv(summary_stats, "outputs/tables/ed_mosque_distance_summary.csv")
cat("  ✓ Saved: outputs/tables/ed_mosque_distance_summary.csv\n")

cat("\n✅ ANALYSIS COMPLETE!\n")
cat(sprintf("   Analyzed %d Election Districts\n", nrow(eds_with_distance)))
cat(sprintf("   Average distance to nearest mosque: %.2f km (%.2f miles)\n\n",
            summary_stats$mean_km, summary_stats$mean_mi))
