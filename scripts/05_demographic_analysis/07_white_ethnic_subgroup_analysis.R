# ============================================================
# White Ethnic Subgroup Analysis - Ancestry + Mamdani Support
# ============================================================
# Analyze which specific White ethnic/ancestry groups support Mamdani

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🔍 WHITE ETHNIC/ANCESTRY SUBGROUP ANALYSIS\n")
cat("===========================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading data...\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# Get detailed ancestry data from B04001-B04006
# B04001 = First Ancestry Reported
# We'll focus on European ancestries

ancestry_vars <- c(
  total_ancestry = "B04006_001",
  # Western European
  british = "B04006_003",
  irish = "B04006_018",
  scottish = "B04006_054",
  welsh = "B04006_063",
  # Southern European
  italian = "B04006_019",
  greek = "B04006_015",
  portuguese = "B04006_046",
  spanish = "B04006_057",
  # Eastern European
  polish = "B04006_045",
  russian = "B04006_051",
  ukrainian = "B04006_061",
  czech = "B04006_007",
  hungarian = "B04006_017",
  romanian = "B04006_050",
  # Germanic
  german = "B04006_014",
  austrian = "B04006_002",
  dutch = "B04006_009",
  # Scandinavian
  norwegian = "B04006_038",
  swedish = "B04006_058",
  danish = "B04006_008",
  # Other European
  french = "B04006_012",
  yugoslavian = "B04006_066",
  albanian = "B04006_001E"  # This might not exist, we'll filter out NAs
)

# Remove problematic variable
ancestry_vars <- ancestry_vars[names(ancestry_vars) != "albanian"]

cat("  Downloading ancestry data (B04006)...\n")
queens_ancestry <- get_acs(
  geography = "tract",
  variables = ancestry_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_ancestry))

# Spatial join for ancestry data
cat("  Performing spatial joins...\n")
queens_ancestry <- queens_ancestry %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

ed_ancestry <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_ancestry
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# Define ethnic groups to analyze
ethnic_groups <- c("italian", "irish", "greek", "polish", "russian", "german",
                   "british", "french", "ukrainian", "portuguese", "spanish",
                   "scottish", "norwegian", "swedish", "hungarian", "romanian",
                   "czech", "dutch", "danish", "welsh", "austrian", "yugoslavian")

# Aggregate to ED level
ed_ancestry_agg <- ed_ancestry %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(ethnic_groups), ~sum(.x * area_weight, na.rm = TRUE)),
    total_ancestry = sum(total_ancestry * area_weight, na.rm = TRUE),
    .groups = "drop"
  )

# Load election results and demographics
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Merge everything
merged <- election_results %>%
  left_join(ed_ancestry_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0) %>%
  mutate(
    turnout_per_1000 = 1000 * total_votes / total_pop,
    # Calculate percentages for each group
    across(all_of(ethnic_groups), ~100 * .x / total_pop, .names = "pct_{.col}")
  ) %>%
  filter(is.finite(turnout_per_1000))

cat("✓ Merged data for", nrow(merged), "EDs\n\n")

# ============================================================
# CORRELATION ANALYSIS
# ============================================================

cat("📊 CORRELATION ANALYSIS\n")
cat("========================\n\n")

# Calculate correlations for each ethnic group
results <- tibble()

for (group in ethnic_groups) {
  raw_var <- group

  # Check if variable exists in merged data
  if (!raw_var %in% names(merged)) next

  # Skip if no variance
  if (sd(merged[[raw_var]], na.rm = TRUE) == 0) next

  # Count non-zero EDs
  n_nonzero <- sum(merged[[raw_var]] > 0, na.rm = TRUE)

  # Skip if too few observations
  if (n_nonzero < 3) next

  # Correlations (all EDs)
  cor_mamdani <- cor(merged[[raw_var]], merged$Mamdani_pct, use = "complete.obs")
  cor_turnout <- cor(merged[[raw_var]], merged$turnout_per_1000, use = "complete.obs")

  # Correlations (non-zero only)
  nonzero_data <- merged %>% filter(.data[[raw_var]] > 0)
  cor_mamdani_nz <- NA
  cor_turnout_nz <- NA

  if (nrow(nonzero_data) >= 3) {
    cor_mamdani_nz <- cor(nonzero_data[[raw_var]], nonzero_data$Mamdani_pct, use = "complete.obs")
    cor_turnout_nz <- cor(nonzero_data[[raw_var]], nonzero_data$turnout_per_1000, use = "complete.obs")
  }

  # Average population
  avg_pop <- mean(merged[[raw_var]], na.rm = TRUE)

  results <- bind_rows(results, tibble(
    group = group,
    n_eds_nonzero = n_nonzero,
    avg_population = avg_pop,
    cor_mamdani_all = cor_mamdani,
    cor_turnout_all = cor_turnout,
    cor_mamdani_nonzero = cor_mamdani_nz,
    cor_turnout_nonzero = cor_turnout_nz
  ))
}

# Clean up names and categorize
results <- results %>%
  mutate(
    group_clean = str_replace_all(group, "_", " ") %>% str_to_title(),
    # Categorize by region
    category = case_when(
      group %in% c("italian", "greek", "portuguese", "spanish") ~ "Southern European",
      group %in% c("polish", "russian", "ukrainian", "czech", "hungarian", "romanian", "yugoslavian") ~ "Eastern European",
      group %in% c("german", "austrian", "dutch") ~ "Germanic",
      group %in% c("irish", "british", "scottish", "welsh") ~ "British Isles",
      group %in% c("norwegian", "swedish", "danish") ~ "Scandinavian",
      TRUE ~ "Other"
    ),
    # Mobilization score
    mobilization_score = cor_mamdani_nonzero + cor_turnout_nonzero
  ) %>%
  arrange(desc(mobilization_score))

cat("🎯 WHITE ETHNIC SUBGROUPS RANKED BY MAMDANI SUPPORT\n")
cat("   (Non-zero EDs only)\n\n")
cat(sprintf("%-30s %8s %12s %12s %12s\n",
            "Group", "N(>0)", "Mamdani r", "Turnout r", "Combined"))
cat(rep("-", 80), "\n")

for (i in 1:nrow(results)) {
  if (!is.na(results$mobilization_score[i])) {
    cat(sprintf("%-30s %8d %12.3f %12.3f %12.3f\n",
                results$group_clean[i],
                results$n_eds_nonzero[i],
                results$cor_mamdani_nonzero[i],
                results$cor_turnout_nonzero[i],
                results$mobilization_score[i]))
  }
}

cat("\n")

# ============================================================
# KEY FINDINGS
# ============================================================

cat("🎯 KEY FINDINGS\n")
cat("================\n\n")

cat("Groups with POSITIVE Mamdani correlation (r > 0.1):\n")
positive_mamdani <- results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero > 0.1) %>%
  arrange(desc(cor_mamdani_nonzero))

if (nrow(positive_mamdani) > 0) {
  for (i in 1:nrow(positive_mamdani)) {
    cat(sprintf("  ✓ %s: r=%.3f (n=%d EDs, avg pop=%.0f)\n",
                positive_mamdani$group_clean[i],
                positive_mamdani$cor_mamdani_nonzero[i],
                positive_mamdani$n_eds_nonzero[i],
                positive_mamdani$avg_population[i]))
  }
} else {
  cat("  (None found with r > 0.1)\n")
}

cat("\nGroups with NEGATIVE Mamdani correlation (r < -0.1):\n")
negative_mamdani <- results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero < -0.1) %>%
  arrange(cor_mamdani_nonzero)

if (nrow(negative_mamdani) > 0) {
  for (i in 1:nrow(negative_mamdani)) {
    cat(sprintf("  ✗ %s: r=%.3f (n=%d EDs, avg pop=%.0f)\n",
                negative_mamdani$group_clean[i],
                negative_mamdani$cor_mamdani_nonzero[i],
                negative_mamdani$n_eds_nonzero[i],
                negative_mamdani$avg_population[i]))
  }
} else {
  cat("  (None found with r < -0.1)\n")
}

cat("\n")

# ============================================================
# VISUALIZATION
# ============================================================

cat("📊 Creating visualizations...\n")

# Filter to groups with enough data
plot_data <- results %>%
  filter(n_eds_nonzero >= 5, !is.na(cor_mamdani_nonzero), !is.na(cor_turnout_nonzero))

if (nrow(plot_data) > 0) {
  # Quadrant plot
  p1 <- ggplot(plot_data, aes(x = cor_mamdani_nonzero, y = cor_turnout_nonzero)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = n_eds_nonzero, color = category), alpha = 0.7) +
    geom_text(aes(label = group_clean), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
    scale_size_continuous(name = "N EDs (>0)", range = c(2, 10)) +
    scale_color_brewer(palette = "Set1", name = "Region") +
    labs(
      title = "White Ethnic/Ancestry Group Electoral Patterns",
      subtitle = "Correlations with Mamdani support vs turnout (non-zero EDs only)",
      x = "Correlation with Mamdani Support",
      y = "Correlation with Turnout",
      caption = "Upper right = Mamdani's mobilized base\nLower right = Mamdani supporters, low turnout\nUpper left = High turnout, not Mamdani supporters"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  # Bar chart of Mamdani correlations
  p2 <- ggplot(results %>% filter(n_eds_nonzero >= 5),
               aes(x = reorder(group_clean, cor_mamdani_nonzero),
                   y = cor_mamdani_nonzero,
                   fill = category)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    coord_flip() +
    scale_fill_brewer(palette = "Set1", name = "Region") +
    labs(
      title = "Mamdani Support by White Ethnic Group",
      subtitle = "Correlation in EDs with population present (n≥5)",
      x = NULL,
      y = "Correlation with Mamdani %"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  combined <- p1 / p2 + plot_layout(heights = c(2, 1))

  dir.create("outputs/analysis", showWarnings = FALSE, recursive = TRUE)
  ggsave("outputs/analysis/white_ethnic_subgroup_analysis.png",
         combined, width = 14, height = 16, dpi = 300)

  cat("  ✓ Saved: outputs/analysis/white_ethnic_subgroup_analysis.png\n\n")
}

# Save results
write_csv(results, "outputs/analysis/white_ethnic_subgroup_correlations.csv")
cat("  ✓ Saved: outputs/analysis/white_ethnic_subgroup_correlations.csv\n\n")

cat("✅ White ethnic subgroup analysis complete!\n")
