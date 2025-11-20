# ============================================================
# MOSQUE PROXIMITY HEATMAP & CORRELATION ANALYSIS
# ============================================================
# Creates a heatmap of Islamic worship centers and analyzes
# correlation between mosque proximity and:
#   1. Voter turnout
#   2. Votes for Zohran Mamdani
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(ggspatial)
library(viridis)
library(units)
library(patchwork)

# Set OSM tile cache directory
options(osmdata_cachedir = "cache")

setwd("~/Mayoral Results AD34")

cat("\n🕌 MOSQUE PROXIMITY HEATMAP & CORRELATION ANALYSIS\n")
cat("===================================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📂 Loading data...\n")

# Load mosque locations
mosques <- read_csv("outputs/tables/ad34_mosques_locations.csv", show_col_types = FALSE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)  # Use meters for distance calculation

cat("  ✓ Loaded", nrow(mosques), "mosques\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>%
  filter(NAMELSAD == "Assembly District 34") %>%
  st_transform(3857)

cat("  ✓ Loaded AD34 boundary\n")

# Load Election District boundaries
ed_files <- list.files("data/raw/election", pattern = "Election_Districts\\.shp$",
                       full.names = TRUE, recursive = TRUE)

if (length(ed_files) == 0) {
  stop("❌ Election District shapefile not found!")
}

eds_all <- st_read(ed_files[1], quiet = TRUE) %>%
  st_transform(3857)

# Filter to AD34 EDs (starting with "34")
eds <- eds_all %>%
  filter(str_detect(Election_D, "^34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_remove(Election_D, "^34"))
  )

cat("  ✓ Loaded", nrow(eds), "Election Districts in AD34\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             show_col_types = FALSE) %>%
  mutate(ED = as.character(ED))  # Ensure ED is character for joining

cat("  ✓ Loaded election results\n")

# Note: Turnout percentage by ED not available in current data
# total_votes is available in election results
turnout_data <- NULL
cat("  ℹ Using total votes from election results (turnout % by ED not available)\n\n")

# ============================================================
# CALCULATE DISTANCES
# ============================================================

cat("📏 Calculating distances from EDs to nearest mosque...\n")

# Get ED centroids
ed_centroids <- st_centroid(eds)

# Calculate distance from each ED to nearest mosque
ed_distances <- ed_centroids %>%
  mutate(
    # Find nearest mosque
    nearest_mosque_dist = st_distance(geometry, mosques, by_element = FALSE) %>%
      apply(1, min) %>%
      set_units("m"),
    # Convert to numeric meters and km
    distance_m = as.numeric(nearest_mosque_dist),
    distance_km = distance_m / 1000,
    # Ensure ED is character for joining
    ED = as.character(ED)
  ) %>%
  st_drop_geometry() %>%
  select(ED, ED_num, distance_m, distance_km)

cat("  ✓ Calculated distances for", nrow(ed_distances), "Election Districts\n\n")

# ============================================================
# MERGE ALL DATA
# ============================================================

cat("🔗 Merging election results with distance data...\n")

# Merge election results with distance
analysis_data <- election_results %>%
  left_join(ed_distances, by = c("ED", "ED_num"))

# Try to add turnout data if available
if (!is.null(turnout_data)) {
  # Check what columns are available
  if ("registered_voters" %in% names(turnout_data)) {
    analysis_data <- analysis_data %>%
      left_join(
        turnout_data %>% select(ED, registered_voters, any_of("turnout_pct")),
        by = "ED"
      )
  }

  # Calculate turnout if we have registration numbers
  if ("registered_voters" %in% names(analysis_data) && !"turnout_pct" %in% names(analysis_data)) {
    analysis_data <- analysis_data %>%
      mutate(
        turnout_pct = if_else(!is.na(registered_voters) & registered_voters > 0,
                             (total_votes / registered_voters) * 100,
                             NA_real_)
      )
  }
}

# If we don't have turnout_pct, create a placeholder
if (!"turnout_pct" %in% names(analysis_data)) {
  cat("  ⚠ No turnout percentage data available\n")
  analysis_data <- analysis_data %>%
    mutate(turnout_pct = NA_real_)
}

cat("  ✓ Merged data for", nrow(analysis_data), "Election Districts\n\n")

# ============================================================
# CREATE SPATIAL DATA FOR MAPPING
# ============================================================

cat("🗺️  Preparing spatial data...\n")

# Join analysis data back to ED geometry
eds_analysis <- eds %>%
  left_join(analysis_data, by = c("ED", "ED_num")) %>%
  st_transform(4326)  # Transform to WGS84 for mapping

# Transform mosques and AD34 for mapping
mosques_map <- st_transform(mosques, 4326)
ad34_map <- st_transform(ad34, 4326)

cat("  ✓ Spatial data ready\n\n")

# ============================================================
# CORRELATION ANALYSIS
# ============================================================

cat("📊 CORRELATION ANALYSIS\n")
cat("========================\n\n")

# Filter complete cases for correlation
complete_data_votes <- analysis_data %>%
  filter(!is.na(distance_km) & !is.na(Zohran_Mamdani))

complete_data_turnout <- analysis_data %>%
  filter(!is.na(distance_km) & !is.na(turnout_pct))

cat("Analyzing", nrow(complete_data_votes), "EDs for vote correlation\n")
if (nrow(complete_data_turnout) > 0) {
  cat("Analyzing", nrow(complete_data_turnout), "EDs for turnout correlation\n\n")
} else {
  cat("⚠ No turnout data available for correlation\n\n")
}

# 1. Distance vs Zohran votes (raw count)
cor_zohran_votes <- cor.test(complete_data_votes$distance_km,
                              complete_data_votes$Zohran_Mamdani,
                              method = "pearson")

# 2. Distance vs Zohran percentage
cor_zohran_pct <- cor.test(complete_data_votes$distance_km,
                            complete_data_votes$Mamdani_pct,
                            method = "pearson")

# 3. Distance vs Turnout (if data available)
if (nrow(complete_data_turnout) > 10) {
  cor_turnout <- cor.test(complete_data_turnout$distance_km,
                          complete_data_turnout$turnout_pct,
                          method = "pearson")
  has_turnout <- TRUE
} else {
  cor_turnout <- NULL
  has_turnout <- FALSE
}

# 4. Distance vs Total Votes
cor_total_votes <- cor.test(complete_data_votes$distance_km,
                             complete_data_votes$total_votes,
                             method = "pearson")

# Print results
cat("🎯 KEY FINDINGS:\n")
cat("─────────────────\n\n")

cat(sprintf("1. Distance vs Zohran Votes (raw count):\n"))
cat(sprintf("   Correlation: %.3f\n", cor_zohran_votes$estimate))
cat(sprintf("   P-value: %.4f %s\n",
            cor_zohran_votes$p.value,
            if_else(cor_zohran_votes$p.value < 0.05, "✓ SIGNIFICANT", "")))
cat(sprintf("   Interpretation: %s\n\n",
            if_else(cor_zohran_votes$estimate < 0,
                   "Closer to mosque = MORE Zohran votes",
                   "Further from mosque = MORE Zohran votes")))

cat(sprintf("2. Distance vs Zohran Vote Share (percentage):\n"))
cat(sprintf("   Correlation: %.3f\n", cor_zohran_pct$estimate))
cat(sprintf("   P-value: %.4f %s\n",
            cor_zohran_pct$p.value,
            if_else(cor_zohran_pct$p.value < 0.05, "✓ SIGNIFICANT", "")))
cat(sprintf("   Interpretation: %s\n\n",
            if_else(cor_zohran_pct$estimate < 0,
                   "Closer to mosque = HIGHER Zohran %",
                   "Further from mosque = HIGHER Zohran %")))

if (has_turnout) {
  cat(sprintf("3. Distance vs Voter Turnout:\n"))
  cat(sprintf("   Correlation: %.3f\n", cor_turnout$estimate))
  cat(sprintf("   P-value: %.4f %s\n",
              cor_turnout$p.value,
              if_else(cor_turnout$p.value < 0.05, "✓ SIGNIFICANT", "")))
  cat(sprintf("   Interpretation: %s\n\n",
              if_else(cor_turnout$estimate < 0,
                     "Closer to mosque = HIGHER turnout",
                     "Further from mosque = HIGHER turnout")))
} else {
  cat("3. Distance vs Voter Turnout:\n")
  cat("   ⚠ Turnout data not available\n\n")
}

cat(sprintf("4. Distance vs Total Votes Cast:\n"))
cat(sprintf("   Correlation: %.3f\n", cor_total_votes$estimate))
cat(sprintf("   P-value: %.4f %s\n\n",
            cor_total_votes$p.value,
            if_else(cor_total_votes$p.value < 0.05, "✓ SIGNIFICANT", "")))

# ============================================================
# CREATE HEATMAP VISUALIZATION
# ============================================================

cat("🎨 Creating mosque density heatmap...\n")

# Create output directory
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# Main heatmap: Distance to nearest mosque
map_distance <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, cachedir = "cache") +
  geom_sf(data = eds_analysis,
          aes(fill = distance_km),
          color = "white",
          linewidth = 0.3,
          alpha = 0.75) +
  geom_sf(data = mosques_map,
          color = "darkgreen",
          fill = "lightgreen",
          shape = 21,
          size = 4,
          stroke = 1.5) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "red",
          linewidth = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Distance to\nNearest Mosque\n(km)",
    direction = -1,  # Reverse so closer = warmer
    breaks = seq(0, max(eds_analysis$distance_km, na.rm = TRUE), by = 0.5)
  ) +
  labs(
    title = "Mosque Proximity Heatmap - Assembly District 34",
    subtitle = "Distance from each Election District to nearest Islamic worship center",
    caption = "Data: OpenStreetMap | Boundary: NYS Assembly District 34 (2025)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50"),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.7, "in"),
    width = unit(0.7, "in")
  )

# Save heatmap
ggsave(
  "outputs/maps/mosque_proximity_heatmap.png",
  plot = map_distance,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/mosque_proximity_heatmap.png\n\n")

# ============================================================
# CREATE SCATTER PLOTS
# ============================================================

cat("📈 Creating correlation scatter plots...\n")

# Scatter plot: Distance vs Zohran votes
plot_zohran_votes <- ggplot(complete_data_votes, aes(x = distance_km, y = Zohran_Mamdani)) +
  geom_point(aes(size = total_votes), alpha = 0.6, color = "#440154FF") +
  geom_smooth(method = "lm", color = "red", se = TRUE, alpha = 0.2) +
  scale_size_continuous(name = "Total Votes", range = c(2, 8)) +
  labs(
    title = "Distance to Mosque vs Zohran Votes",
    x = "Distance to Nearest Mosque (km)",
    y = "Votes for Zohran Mamdani",
    subtitle = sprintf("Pearson r = %.3f, p = %.4f",
                      cor_zohran_votes$estimate,
                      cor_zohran_votes$p.value)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Scatter plot: Distance vs Zohran percentage
plot_zohran_pct <- ggplot(complete_data_votes, aes(x = distance_km, y = Mamdani_pct)) +
  geom_point(aes(size = total_votes), alpha = 0.6, color = "#31688EFF") +
  geom_smooth(method = "lm", color = "red", se = TRUE, alpha = 0.2) +
  scale_size_continuous(name = "Total Votes", range = c(2, 8)) +
  labs(
    title = "Distance to Mosque vs Zohran Vote Share",
    x = "Distance to Nearest Mosque (km)",
    y = "Zohran Vote Share (%)",
    subtitle = sprintf("Pearson r = %.3f, p = %.4f",
                      cor_zohran_pct$estimate,
                      cor_zohran_pct$p.value)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Scatter plot: Distance vs Turnout (if available)
if (has_turnout) {
  plot_turnout <- ggplot(complete_data_turnout, aes(x = distance_km, y = turnout_pct)) +
    geom_point(aes(size = total_votes), alpha = 0.6, color = "#35B779FF") +
    geom_smooth(method = "lm", color = "red", se = TRUE, alpha = 0.2) +
    scale_size_continuous(name = "Total Votes", range = c(2, 8)) +
    labs(
      title = "Distance to Mosque vs Voter Turnout",
      x = "Distance to Nearest Mosque (km)",
      y = "Turnout (%)",
      subtitle = sprintf("Pearson r = %.3f, p = %.4f",
                        cor_turnout$estimate,
                        cor_turnout$p.value)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )

  # Combine scatter plots
  combined_plots <- (plot_zohran_votes | plot_zohran_pct) /
                    (plot_turnout | plot_spacer()) +
                    plot_annotation(
                      title = "Mosque Proximity Impact Analysis - AD34",
                      subtitle = "Analyzing relationship between distance to Islamic worship centers and electoral outcomes",
                      theme = theme(
                        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
                      )
                    )
} else {
  # Combine scatter plots (without turnout)
  combined_plots <- (plot_zohran_votes | plot_zohran_pct) +
                    plot_annotation(
                      title = "Mosque Proximity Impact Analysis - AD34",
                      subtitle = "Analyzing relationship between distance to Islamic worship centers and electoral outcomes",
                      theme = theme(
                        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
                      )
                    )
}

# Save combined plot
ggsave(
  "outputs/maps/mosque_correlation_analysis.png",
  plot = combined_plots,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/mosque_correlation_analysis.png\n\n")

# ============================================================
# SAVE SUMMARY STATISTICS
# ============================================================

cat("💾 Saving summary statistics...\n")

# Create summary table
if (has_turnout) {
  summary_table <- tibble(
    Metric = c(
      "Distance vs Zohran Votes",
      "Distance vs Zohran %",
      "Distance vs Turnout %",
      "Distance vs Total Votes"
    ),
    Correlation = c(
      cor_zohran_votes$estimate,
      cor_zohran_pct$estimate,
      cor_turnout$estimate,
      cor_total_votes$estimate
    ),
    P_Value = c(
      cor_zohran_votes$p.value,
      cor_zohran_pct$p.value,
      cor_turnout$p.value,
      cor_total_votes$p.value
    ),
    Significant = c(
      cor_zohran_votes$p.value < 0.05,
      cor_zohran_pct$p.value < 0.05,
      cor_turnout$p.value < 0.05,
      cor_total_votes$p.value < 0.05
    ),
    Interpretation = c(
      if_else(cor_zohran_votes$estimate < 0,
              "Closer to mosque = MORE votes",
              "Further = MORE votes"),
      if_else(cor_zohran_pct$estimate < 0,
              "Closer to mosque = HIGHER %",
              "Further = HIGHER %"),
      if_else(cor_turnout$estimate < 0,
              "Closer to mosque = HIGHER turnout",
              "Further = HIGHER turnout"),
      if_else(cor_total_votes$estimate < 0,
              "Closer to mosque = MORE total votes",
              "Further = MORE total votes")
    )
  )
} else {
  summary_table <- tibble(
    Metric = c(
      "Distance vs Zohran Votes",
      "Distance vs Zohran %",
      "Distance vs Total Votes"
    ),
    Correlation = c(
      cor_zohran_votes$estimate,
      cor_zohran_pct$estimate,
      cor_total_votes$estimate
    ),
    P_Value = c(
      cor_zohran_votes$p.value,
      cor_zohran_pct$p.value,
      cor_total_votes$p.value
    ),
    Significant = c(
      cor_zohran_votes$p.value < 0.05,
      cor_zohran_pct$p.value < 0.05,
      cor_total_votes$p.value < 0.05
    ),
    Interpretation = c(
      if_else(cor_zohran_votes$estimate < 0,
              "Closer to mosque = MORE votes",
              "Further = MORE votes"),
      if_else(cor_zohran_pct$estimate < 0,
              "Closer to mosque = HIGHER %",
              "Further = HIGHER %"),
      if_else(cor_total_votes$estimate < 0,
              "Closer to mosque = MORE total votes",
              "Further = MORE total votes")
    )
  )
}

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(summary_table, "outputs/tables/mosque_correlation_summary.csv")

cat("  ✓ Saved: outputs/tables/mosque_correlation_summary.csv\n")

# Save detailed ED-level data
write_csv(analysis_data, "outputs/tables/ed_mosque_analysis_complete.csv")
cat("  ✓ Saved: outputs/tables/ed_mosque_analysis_complete.csv\n\n")

# ============================================================
# FINAL SUMMARY
# ============================================================

cat("✅ ANALYSIS COMPLETE!\n")
cat("═══════════════════════\n\n")
cat("📁 Output files:\n")
cat("   - outputs/maps/mosque_proximity_heatmap.png\n")
cat("   - outputs/maps/mosque_correlation_analysis.png\n")
cat("   - outputs/tables/mosque_correlation_summary.csv\n")
cat("   - outputs/tables/ed_mosque_analysis_complete.csv\n\n")

cat("🎯 Key Takeaways:\n")
if (has_turnout && cor_turnout$p.value < 0.05) {
  cat(sprintf("   ✓ SIGNIFICANT turnout correlation (r=%.3f, p=%.4f)\n",
              cor_turnout$estimate, cor_turnout$p.value))
}
if (cor_zohran_votes$p.value < 0.05) {
  cat(sprintf("   ✓ SIGNIFICANT Zohran vote correlation (r=%.3f, p=%.4f)\n",
              cor_zohran_votes$estimate, cor_zohran_votes$p.value))
}
if (cor_zohran_pct$p.value < 0.05) {
  cat(sprintf("   ✓ SIGNIFICANT Zohran %% correlation (r=%.3f, p=%.4f)\n",
              cor_zohran_pct$estimate, cor_zohran_pct$p.value))
}
cat("\n")
