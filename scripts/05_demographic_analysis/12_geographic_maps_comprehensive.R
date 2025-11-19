# ============================================================
# Comprehensive Geographic Maps - Vote Share & Ethnicity
# ============================================================
# Visualize the geography of AD34 showing electoral and demographic patterns

library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)
library(scales)
library(ggspatial)

setwd("~/Mayoral Results AD34")

cat("\n🗺️  COMPREHENSIVE GEOGRAPHIC MAPPING\n")
cat("====================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading election and demographic data...\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE)

# Load demographics
demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE)

# Load foreign-born data (select only the columns we need, excluding duplicates)
foreign_born <- read_csv("outputs/analysis/foreign_born_asian_by_ed.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, ED_num, india, bangladesh, pakistan, nepal, china, korea, philippines, vietnam)

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

# Filter to AD34 and transform to WGS84 for mapping
ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(4326)  # WGS84 for mapping

cat("  ✓ Loaded", nrow(ad34_shp), "Election Districts\n\n")

# ============================================================
# MERGE ALL DATA
# ============================================================

cat("🔗 Merging electoral and demographic data...\n")

# Check join keys
cat("  Sample EDs from shapefile:", paste(head(ad34_shp$ED, 3), collapse=", "), "\n")
cat("  Sample EDs from election:", paste(head(election_results$ED, 3), collapse=", "), "\n")

# Merge everything
map_data <- ad34_shp %>%
  left_join(election_results, by = c("ED", "ED_num"))

cat("  ✓ After election join:", nrow(map_data), "rows,", sum(!is.na(map_data$Mamdani_pct)), "with Mamdani data\n")

map_data <- map_data %>%
  left_join(demographics, by = c("ED", "ED_num"))

cat("  ✓ After demographics join:", nrow(map_data), "rows\n")

map_data <- map_data %>%
  left_join(foreign_born, by = c("ED", "ED_num"))

cat("  ✓ After foreign-born join:", nrow(map_data), "rows\n")

# Filter to only EDs with election data
# Use base R filtering to avoid sf issues
eds_with_data <- !is.na(map_data$Mamdani_pct)
map_data <- map_data[eds_with_data, ]

cat("  ✓ After filtering to EDs with data:", nrow(map_data), "rows\n")

# Calculate additional metrics
map_data <- map_data %>%
  mutate(
    turnout_pct = 100 * total_votes / total_pop,
    # Foreign-born percentages (only if columns exist)
    pct_china = if("china" %in% names(.)) 100 * china / total_pop else NA_real_,
    pct_bangladesh = if("bangladesh" %in% names(.)) 100 * bangladesh / total_pop else NA_real_,
    pct_india = if("india" %in% names(.)) 100 * india / total_pop else NA_real_,
    pct_korea = if("korea" %in% names(.)) 100 * korea / total_pop else NA_real_,
    # Winner categorical
    winner_cat = case_when(
      Mamdani_pct > 50 ~ "Mamdani (>50%)",
      Mamdani_pct > Cuomo_pct & Mamdani_pct > Sliwa_pct ~ "Mamdani (plurality)",
      Cuomo_pct > Sliwa_pct ~ "Cuomo",
      TRUE ~ "Sliwa"
    ),
    winner_cat = factor(winner_cat, levels = c("Mamdani (>50%)", "Mamdani (plurality)",
                                                 "Cuomo", "Sliwa"))
  )

cat("  ✓ Merged data for", nrow(map_data), "EDs\n\n")

# Get AD34 boundary for overlay
ad34_boundary <- map_data %>%
  st_union() %>%
  st_cast("MULTIPOLYGON")

# ============================================================
# MAPPING THEME
# ============================================================

map_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ============================================================
# MAP 1: MAMDANI VOTE SHARE
# ============================================================

cat("🗺️  Creating Map 1: Mamdani Vote Share...\n")

m1 <- ggplot(map_data) +
  geom_sf(aes(fill = Mamdani_pct), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 50,
    name = "Mamdani %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Mamdani Vote Share by Election District",
    subtitle = "2025 NYC Mayoral Election - Assembly District 34",
    caption = "Green = Strong Mamdani | Yellow = Competitive | Red = Weak Mamdani"
  ) +
  map_theme()

# ============================================================
# MAP 2: WINNER BY ED
# ============================================================

cat("🗺️  Creating Map 2: Winner by ED...\n")

m2 <- ggplot(map_data) +
  geom_sf(aes(fill = winner_cat), color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "Mamdani (>50%)" = "#1a9850",
      "Mamdani (plurality)" = "#a6d96a",
      "Cuomo" = "#4575b4",
      "Sliwa" = "#d73027"
    ),
    name = "Winner",
    drop = FALSE
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Winner by Election District",
    subtitle = "2025 NYC Mayoral Election - Assembly District 34"
  ) +
  map_theme()

# ============================================================
# MAP 3: TURNOUT
# ============================================================

cat("🗺️  Creating Map 3: Voter Turnout...\n")

m3 <- ggplot(map_data) +
  geom_sf(aes(fill = turnout_pct), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Turnout %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Voter Turnout by Election District",
    subtitle = "Total Votes as % of Total Population"
  ) +
  map_theme()

# ============================================================
# MAP 4: HISPANIC POPULATION
# ============================================================

cat("🗺️  Creating Map 4: Hispanic Population...\n")

m4 <- ggplot(map_data) +
  geom_sf(aes(fill = pct_hispanic), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Hispanic %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Hispanic/Latino Population Concentration",
    subtitle = "ACS 2018-2022 5-year estimates"
  ) +
  map_theme()

# ============================================================
# MAP 5: CHINESE POPULATION
# ============================================================

cat("🗺️  Creating Map 5: Chinese Population...\n")

m5 <- ggplot(map_data %>% filter(!is.na(pct_china))) +
  geom_sf(aes(fill = pct_china), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Chinese %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Foreign-Born Chinese Population",
    subtitle = "China-born residents (ACS 2018-2022)"
  ) +
  map_theme()

# ============================================================
# MAP 6: BANGLADESHI POPULATION
# ============================================================

cat("🗺️  Creating Map 6: Bangladeshi Population...\n")

m6 <- ggplot(map_data %>% filter(!is.na(pct_bangladesh))) +
  geom_sf(aes(fill = pct_bangladesh), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "rocket",
    name = "Bangladesh %",
    labels = percent_format(scale = 1),
    na.value = "grey90"
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Foreign-Born Bangladeshi Population",
    subtitle = "Bangladesh-born residents (ACS 2018-2022)"
  ) +
  map_theme()

# ============================================================
# MAP 7: ASIAN POPULATION
# ============================================================

cat("🗺️  Creating Map 7: Asian Population...\n")

m7 <- ggplot(map_data) +
  geom_sf(aes(fill = pct_asian), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "turbo",
    name = "Asian %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "Asian Population Concentration",
    subtitle = "All Asian alone (ACS 2018-2022)"
  ) +
  map_theme()

# ============================================================
# MAP 8: WHITE POPULATION
# ============================================================

cat("🗺️  Creating Map 8: White Population...\n")

m8 <- ggplot(map_data) +
  geom_sf(aes(fill = pct_white), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "cividis",
    name = "White %",
    labels = percent_format(scale = 1)
  ) +
  geom_sf(data = ad34_boundary, fill = NA, color = "black", linewidth = 1) +
  labs(
    title = "White Population Concentration",
    subtitle = "White alone, non-Hispanic (ACS 2018-2022)"
  ) +
  map_theme()

# ============================================================
# SAVE INDIVIDUAL MAPS
# ============================================================

cat("\n💾 Saving individual maps...\n")

dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/maps/01_mamdani_vote_share.png", m1, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 01_mamdani_vote_share.png\n")

ggsave("outputs/maps/02_winner_by_ed.png", m2, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 02_winner_by_ed.png\n")

ggsave("outputs/maps/03_turnout.png", m3, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 03_turnout.png\n")

ggsave("outputs/maps/04_hispanic_population.png", m4, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 04_hispanic_population.png\n")

ggsave("outputs/maps/05_chinese_population.png", m5, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 05_chinese_population.png\n")

ggsave("outputs/maps/06_bangladeshi_population.png", m6, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 06_bangladeshi_population.png\n")

ggsave("outputs/maps/07_asian_population.png", m7, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 07_asian_population.png\n")

ggsave("outputs/maps/08_white_population.png", m8, width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: 08_white_population.png\n")

# ============================================================
# COMBINED MULTI-PANEL MAPS
# ============================================================

cat("\n🗺️  Creating multi-panel comparison maps...\n")

# Panel 1: Electoral Maps
electoral_panel <- (m1 | m2) / (m3 | m2) +
  plot_annotation(
    title = "Electoral Geography of AD34",
    subtitle = "2025 NYC Mayoral Election",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("outputs/maps/combined_electoral_maps.png", electoral_panel,
       width = 16, height = 12, dpi = 300)
cat("  ✓ Saved: combined_electoral_maps.png\n")

# Panel 2: Demographic Comparison
demo_panel <- (m4 | m7) / (m5 | m6) +
  plot_annotation(
    title = "Demographic Geography of AD34",
    subtitle = "Major Ethnic Groups by Election District",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("outputs/maps/combined_demographic_maps.png", demo_panel,
       width = 16, height = 12, dpi = 300)
cat("  ✓ Saved: combined_demographic_maps.png\n")

# Panel 3: Vote vs Demographics (side by side)
vote_demo_panel <- (m1 | m4 | m7) / (m2 | m5 | m6) +
  plot_annotation(
    title = "Electoral Support vs Ethnic Geography - AD34",
    subtitle = "Top: Vote Share & Hispanic | Bottom: Winner & Chinese/Bangladeshi",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("outputs/maps/combined_vote_vs_demographics.png", vote_demo_panel,
       width = 18, height = 10, dpi = 300)
cat("  ✓ Saved: combined_vote_vs_demographics.png\n")

# ============================================================
# SUMMARY STATISTICS BY GEOGRAPHY
# ============================================================

cat("\n📊 GEOGRAPHIC SUMMARY STATISTICS\n")
cat("=================================\n\n")

# Calculate quartiles of Mamdani support
map_data_stats <- map_data %>%
  st_drop_geometry() %>%
  mutate(
    support_quartile = ntile(Mamdani_pct, 4),
    support_label = case_when(
      support_quartile == 4 ~ "Top 25% (Strongest)",
      support_quartile == 3 ~ "3rd Quartile",
      support_quartile == 2 ~ "2nd Quartile",
      support_quartile == 1 ~ "Bottom 25% (Weakest)"
    )
  )

cat("MAMDANI SUPPORT BY QUARTILE:\n\n")

quartile_summary <- map_data_stats %>%
  group_by(support_label) %>%
  summarize(
    n_eds = n(),
    avg_mamdani = mean(Mamdani_pct, na.rm = TRUE),
    avg_hispanic = mean(pct_hispanic, na.rm = TRUE),
    avg_asian = mean(pct_asian, na.rm = TRUE),
    avg_white = mean(pct_white, na.rm = TRUE),
    avg_chinese = mean(pct_china, na.rm = TRUE),
    avg_bangladeshi = mean(pct_bangladesh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mamdani))

for (i in 1:nrow(quartile_summary)) {
  cat(sprintf("\n%s:\n", quartile_summary$support_label[i]))
  cat(sprintf("  EDs: %d\n", quartile_summary$n_eds[i]))
  cat(sprintf("  Avg Mamdani: %.1f%%\n", quartile_summary$avg_mamdani[i]))
  cat(sprintf("  Avg Hispanic: %.1f%%\n", quartile_summary$avg_hispanic[i]))
  cat(sprintf("  Avg Asian: %.1f%%\n", quartile_summary$avg_asian[i]))
  cat(sprintf("  Avg Chinese: %.1f%%\n", quartile_summary$avg_chinese[i]))
  cat(sprintf("  Avg Bangladeshi: %.1f%%\n", quartile_summary$avg_bangladeshi[i]))
}

cat("\n")

# Top and Bottom EDs
cat("TOP 10 STRONGEST MAMDANI EDs:\n")
cat(sprintf("%-8s %10s %10s %10s %10s\n", "ED", "Mamdani%", "Hispanic%", "Asian%", "Chinese%"))
cat(rep("-", 55), "\n")

top_eds <- map_data_stats %>%
  arrange(desc(Mamdani_pct)) %>%
  head(10)

for (i in 1:nrow(top_eds)) {
  cat(sprintf("%-8s %10.1f %10.1f %10.1f %10.1f\n",
              top_eds$ED[i],
              top_eds$Mamdani_pct[i],
              top_eds$pct_hispanic[i],
              top_eds$pct_asian[i],
              top_eds$pct_china[i]))
}

cat("\n")

cat("TOP 10 WEAKEST MAMDANI EDs:\n")
cat(sprintf("%-8s %10s %10s %10s %10s\n", "ED", "Mamdani%", "Hispanic%", "Asian%", "Chinese%"))
cat(rep("-", 55), "\n")

bottom_eds <- map_data_stats %>%
  arrange(Mamdani_pct) %>%
  head(10)

for (i in 1:nrow(bottom_eds)) {
  cat(sprintf("%-8s %10.1f %10.1f %10.1f %10.1f\n",
              bottom_eds$ED[i],
              bottom_eds$Mamdani_pct[i],
              bottom_eds$pct_hispanic[i],
              bottom_eds$pct_asian[i],
              bottom_eds$pct_china[i]))
}

cat("\n")

# Save geographic summary
write_csv(map_data_stats, "outputs/analysis/geographic_summary_by_ed.csv")
cat("✅ Saved: geographic_summary_by_ed.csv\n")

cat("\n✅ All maps created successfully!\n")
cat("   📁 Location: outputs/maps/\n")
