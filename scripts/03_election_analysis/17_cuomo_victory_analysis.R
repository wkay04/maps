# ============================================================
# CUOMO VICTORY ANALYSIS - AD34 MAYORAL RACE
# ============================================================
# Maps where Cuomo won, margin of victory, and turnout by ED
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(ggspatial)
library(viridis)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🗳️  CUOMO VICTORY ANALYSIS - AD34\n")
cat("=====================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📂 Loading data...\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             show_col_types = FALSE) %>%
  mutate(
    ED = as.character(ED),
    # Calculate margin of victory (winner's % minus runner-up's %)
    margin_pct = case_when(
      winner == "Mamdani" ~ Mamdani_pct - Cuomo_pct,
      winner == "Cuomo" ~ Cuomo_pct - Mamdani_pct,
      TRUE ~ 0
    ),
    # Calculate absolute vote margin
    margin_votes = case_when(
      winner == "Mamdani" ~ Zohran_Mamdani - Andrew_Cuomo,
      winner == "Cuomo" ~ Andrew_Cuomo - Zohran_Mamdani,
      TRUE ~ 0
    ),
    # Two-candidate vote share (excluding other candidates)
    two_candidate_total = Zohran_Mamdani + Andrew_Cuomo,
    mamdani_two_candidate_pct = (Zohran_Mamdani / two_candidate_total) * 100,
    cuomo_two_candidate_pct = (Andrew_Cuomo / two_candidate_total) * 100
  )

cat("  ✓ Loaded", nrow(election_results), "Election Districts\n")

# Summarize winners
winner_summary <- election_results %>%
  count(winner) %>%
  arrange(desc(n))

cat("\nWinner Summary:\n")
for (i in 1:nrow(winner_summary)) {
  cat(sprintf("  %s: %d EDs (%.1f%%)\n",
              winner_summary$winner[i],
              winner_summary$n[i],
              100 * winner_summary$n[i] / nrow(election_results)))
}
cat("\n")

# Cuomo victories
cuomo_eds <- election_results %>% filter(winner == "Cuomo")
cat("Cuomo won in", nrow(cuomo_eds), "Election Districts:\n")
if (nrow(cuomo_eds) > 0) {
  for (i in 1:nrow(cuomo_eds)) {
    cat(sprintf("  ED %s: Cuomo %d (%.1f%%) vs Mamdani %d (%.1f%%) - Margin: %.1f%%\n",
                cuomo_eds$ED[i],
                cuomo_eds$Andrew_Cuomo[i],
                cuomo_eds$Cuomo_pct[i],
                cuomo_eds$Zohran_Mamdani[i],
                cuomo_eds$Mamdani_pct[i],
                cuomo_eds$margin_pct[i]))
  }
}
cat("\n")

# Load Election District boundaries
ed_files <- list.files("data/raw/election", pattern = "Election_Districts\\.shp$",
                       full.names = TRUE, recursive = TRUE)

eds_all <- st_read(ed_files[1], quiet = TRUE) %>%
  st_transform(3857)

# Filter to AD34 EDs
eds <- eds_all %>%
  filter(str_detect(Election_D, "^34")) %>%
  mutate(
    ED = as.character(Election_D),
    ED_num = as.numeric(str_remove(Election_D, "^34"))
  )

cat("  ✓ Loaded", nrow(eds), "ED boundaries\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>%
  filter(NAMELSAD == "Assembly District 34") %>%
  st_transform(3857)

cat("  ✓ Loaded AD34 boundary\n\n")

# ============================================================
# MERGE ELECTION DATA WITH GEOGRAPHY
# ============================================================

cat("🔗 Merging election results with ED boundaries...\n")

eds_results <- eds %>%
  left_join(election_results, by = c("ED", "ED_num")) %>%
  st_transform(4326)  # Transform to WGS84 for mapping

ad34_map <- st_transform(ad34, 4326)

cat("  ✓ Merged", nrow(eds_results), "EDs with election results\n\n")

# ============================================================
# MAP 1: WINNER MAP (CUOMO VS MAMDANI)
# ============================================================

cat("🗺️  Creating winner map...\n")

dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# Create winner colors
winner_colors <- c("Mamdani" = "#2E5FA7", "Cuomo" = "#C41E3A")

map_winners <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, cachedir = "cache") +
  geom_sf(data = eds_results,
          aes(fill = winner),
          color = "white",
          linewidth = 0.4,
          alpha = 0.8) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 1.2) +
  scale_fill_manual(
    values = winner_colors,
    name = "Winner",
    na.value = "gray80"
  ) +
  labs(
    title = "Mayoral Race Winner by Election District",
    subtitle = "Assembly District 34 - 2025 Mayoral Primary\nManmdani (Blue) vs Cuomo (Red)",
    caption = "Data: NYC BOE | Boundary: NYS Assembly District 34 (2025)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", lineheight = 1.2),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "in"),
    width = unit(0.8, "in")
  )

ggsave(
  "outputs/maps/ad34_cuomo_vs_mamdani_winners.png",
  plot = map_winners,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/ad34_cuomo_vs_mamdani_winners.png\n\n")

# ============================================================
# MAP 2: MARGIN OF VICTORY HEATMAP
# ============================================================

cat("🔥 Creating margin of victory heatmap...\n")

# Create diverging margin (positive = Mamdani lead, negative = Cuomo lead)
eds_results <- eds_results %>%
  mutate(
    margin_diverging = case_when(
      winner == "Mamdani" ~ margin_pct,
      winner == "Cuomo" ~ -margin_pct,
      TRUE ~ 0
    )
  )

map_margin <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, cachedir = "cache") +
  geom_sf(data = eds_results,
          aes(fill = margin_diverging),
          color = "white",
          linewidth = 0.3,
          alpha = 0.85) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 1.2) +
  scale_fill_gradient2(
    low = "#C41E3A",      # Cuomo red
    mid = "#FFFFFF",       # White for close races
    high = "#2E5FA7",      # Mamdani blue
    midpoint = 0,
    name = "Margin of\nVictory (%)",
    labels = function(x) {
      ifelse(x < 0,
             paste0("Cuomo +", abs(x)),
             paste0("Mamdani +", x))
    },
    limits = c(-max(abs(eds_results$margin_diverging), na.rm = TRUE),
               max(abs(eds_results$margin_diverging), na.rm = TRUE))
  ) +
  labs(
    title = "Margin of Victory by Election District",
    subtitle = "Assembly District 34 - 2025 Mayoral Primary\nBlue = Mamdani Win | Red = Cuomo Win | Intensity = Margin Size",
    caption = "Data: NYC BOE | Boundary: NYS Assembly District 34 (2025)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", lineheight = 1.2),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    legend.key.height = unit(1.2, "in"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "in"),
    width = unit(0.8, "in")
  )

ggsave(
  "outputs/maps/ad34_margin_of_victory_heatmap.png",
  plot = map_margin,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/ad34_margin_of_victory_heatmap.png\n\n")

# ============================================================
# MAP 3: TURNOUT HEATMAP
# ============================================================

cat("📊 Creating turnout heatmap...\n")

map_turnout <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, cachedir = "cache") +
  geom_sf(data = eds_results,
          aes(fill = total_votes),
          color = "white",
          linewidth = 0.3,
          alpha = 0.85) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 1.2) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Total Votes\nCast",
    direction = 1,
    trans = "sqrt",  # Use square root transformation for better distribution
    breaks = c(100, 250, 500, 750, 1000),
    labels = c("100", "250", "500", "750", "1000+")
  ) +
  labs(
    title = "Voter Turnout by Election District",
    subtitle = "Assembly District 34 - 2025 Mayoral Primary\nTotal votes cast in each Election District",
    caption = "Data: NYC BOE | Boundary: NYS Assembly District 34 (2025)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", lineheight = 1.2),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1, "in"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "in"),
    width = unit(0.8, "in")
  )

ggsave(
  "outputs/maps/ad34_turnout_heatmap.png",
  plot = map_turnout,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/ad34_turnout_heatmap.png\n\n")

# ============================================================
# COMBINED VISUALIZATION
# ============================================================

cat("🎨 Creating combined visualization...\n")

# Create simplified versions for combined plot
map_winners_simple <- ggplot() +
  geom_sf(data = eds_results,
          aes(fill = winner),
          color = "white",
          linewidth = 0.3) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 0.8) +
  scale_fill_manual(
    values = winner_colors,
    name = "Winner",
    na.value = "gray80"
  ) +
  labs(title = "Winner by ED") +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

map_margin_simple <- ggplot() +
  geom_sf(data = eds_results,
          aes(fill = margin_diverging),
          color = "white",
          linewidth = 0.3) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 0.8) +
  scale_fill_gradient2(
    low = "#C41E3A",
    mid = "#FFFFFF",
    high = "#2E5FA7",
    midpoint = 0,
    name = "Margin (%)"
  ) +
  labs(title = "Margin of Victory") +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

map_turnout_simple <- ggplot() +
  geom_sf(data = eds_results,
          aes(fill = total_votes),
          color = "white",
          linewidth = 0.3) +
  geom_sf(data = ad34_map,
          fill = NA,
          color = "black",
          linewidth = 0.8) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Votes",
    trans = "sqrt"
  ) +
  labs(title = "Total Turnout") +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

combined_map <- (map_winners_simple | map_margin_simple | map_turnout_simple) +
  plot_annotation(
    title = "AD34 Mayoral Primary Analysis - 2025",
    subtitle = "Winner Distribution, Victory Margins, and Turnout Patterns",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
    )
  )

ggsave(
  "outputs/maps/ad34_combined_analysis.png",
  plot = combined_map,
  width = 16,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("  ✓ Saved: outputs/maps/ad34_combined_analysis.png\n\n")

# ============================================================
# SAVE SUMMARY STATISTICS
# ============================================================

cat("💾 Saving summary statistics...\n")

# Overall statistics
overall_stats <- tibble(
  Metric = c(
    "Total EDs",
    "Mamdani Victories",
    "Cuomo Victories",
    "Mamdani Win %",
    "Total Votes Cast",
    "Average Votes per ED",
    "Median Margin of Victory",
    "Average Margin of Victory"
  ),
  Value = c(
    nrow(election_results),
    sum(election_results$winner == "Mamdani", na.rm = TRUE),
    sum(election_results$winner == "Cuomo", na.rm = TRUE),
    round(100 * sum(election_results$winner == "Mamdani", na.rm = TRUE) / nrow(election_results), 1),
    sum(election_results$total_votes, na.rm = TRUE),
    round(mean(election_results$total_votes, na.rm = TRUE), 0),
    round(median(election_results$margin_pct, na.rm = TRUE), 1),
    round(mean(election_results$margin_pct, na.rm = TRUE), 1)
  )
)

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(overall_stats, "outputs/tables/ad34_election_summary.csv")
cat("  ✓ Saved: outputs/tables/ad34_election_summary.csv\n")

# Cuomo victories detail
if (nrow(cuomo_eds) > 0) {
  cuomo_detail <- cuomo_eds %>%
    select(ED, ED_num, Andrew_Cuomo, Cuomo_pct, Zohran_Mamdani, Mamdani_pct,
           total_votes, margin_pct, margin_votes) %>%
    arrange(desc(margin_pct))

  write_csv(cuomo_detail, "outputs/tables/cuomo_victory_eds.csv")
  cat("  ✓ Saved: outputs/tables/cuomo_victory_eds.csv\n")
}

# Complete ED-level data
ed_complete <- election_results %>%
  select(ED, ED_num, winner, Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada,
         Curtis_Sliwa, total_votes, Mamdani_pct, Cuomo_pct,
         margin_pct, margin_votes, mamdani_two_candidate_pct,
         cuomo_two_candidate_pct) %>%
  arrange(desc(margin_pct))

write_csv(ed_complete, "outputs/tables/ad34_ed_complete_results.csv")
cat("  ✓ Saved: outputs/tables/ad34_ed_complete_results.csv\n\n")

# ============================================================
# FINAL SUMMARY
# ============================================================

cat("✅ ANALYSIS COMPLETE!\n")
cat("═══════════════════════\n\n")

cat("📁 Output files:\n")
cat("   Maps:\n")
cat("   - outputs/maps/ad34_cuomo_vs_mamdani_winners.png\n")
cat("   - outputs/maps/ad34_margin_of_victory_heatmap.png\n")
cat("   - outputs/maps/ad34_turnout_heatmap.png\n")
cat("   - outputs/maps/ad34_combined_analysis.png\n")
cat("\n   Tables:\n")
cat("   - outputs/tables/ad34_election_summary.csv\n")
cat("   - outputs/tables/cuomo_victory_eds.csv\n")
cat("   - outputs/tables/ad34_ed_complete_results.csv\n\n")

cat("🗳️  Election Summary:\n")
cat(sprintf("   Total EDs: %d\n", nrow(election_results)))
cat(sprintf("   Mamdani: %d EDs (%.1f%%)\n",
            sum(election_results$winner == "Mamdani", na.rm = TRUE),
            100 * sum(election_results$winner == "Mamdani", na.rm = TRUE) / nrow(election_results)))
cat(sprintf("   Cuomo: %d EDs (%.1f%%)\n",
            sum(election_results$winner == "Cuomo", na.rm = TRUE),
            100 * sum(election_results$winner == "Cuomo", na.rm = TRUE) / nrow(election_results)))
cat(sprintf("   Total Votes: %s\n", format(sum(election_results$total_votes, na.rm = TRUE), big.mark = ",")))
cat(sprintf("   Average Margin: %.1f%%\n\n", mean(election_results$margin_pct, na.rm = TRUE)))
