# ============================================================
# Detailed Subgroup Analysis - Ethnicity + Turnout
# ============================================================
# Which specific ethnic subgroups show BOTH:
# 1. High correlation with turnout (mobilization)
# 2. High correlation with Mamdani support (his base)

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🔍 DETAILED ASIAN SUBGROUP + TURNOUT ANALYSIS\n")
cat("==============================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading data...\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# Get detailed Asian ancestry data
asian_vars <- c(
  total_asian = "B02015_001",
  # South Asian
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  pakistani = "B02015_006",
  sri_lankan = "B02015_007",
  # East Asian
  chinese = "B02015_009",
  taiwanese = "B02015_008",
  japanese = "B02015_013",
  korean = "B02015_014",
  # Southeast Asian
  filipino = "B02015_010",
  vietnamese = "B02015_018",
  thai = "B02015_016",
  indonesian = "B02015_011",
  burmese = "B02015_004",
  cambodian = "B02015_005"
)

cat("  Downloading Asian ethnicity data (B02015)...\n")
queens_asian <- get_acs(
  geography = "tract",
  variables = asian_vars,
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
  st_transform(st_crs(queens_asian))

# Spatial join for ethnicity data
cat("  Performing spatial joins...\n")
queens_asian <- queens_asian %>%
  mutate(bg_area = as.numeric(st_area(geometry)))

ed_asian <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_asian
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / bg_area
  ) %>%
  st_drop_geometry()

# Aggregate to ED level
ethnic_groups <- c("asian_indian", "bangladeshi", "pakistani", "sri_lankan",
                   "chinese", "taiwanese", "japanese", "korean",
                   "filipino", "vietnamese", "thai", "indonesian", "burmese", "cambodian")

ed_asian_agg <- ed_asian %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(ethnic_groups), ~sum(.x * area_weight, na.rm = TRUE)),
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
  left_join(ed_asian_agg, by = c("ED", "ED_num")) %>%
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
# CORRELATION MATRIX: Each subgroup with Mamdani & Turnout
# ============================================================

cat("📊 CORRELATION ANALYSIS\n")
cat("========================\n\n")

# Calculate correlations for each ethnic group
results <- tibble()

for (group in ethnic_groups) {
  raw_var <- group
  pct_var <- paste0("pct_", group)

  # Skip if no variance
  if (sd(merged[[raw_var]], na.rm = TRUE) == 0) next

  # Count non-zero EDs
  n_nonzero <- sum(merged[[raw_var]] > 0, na.rm = TRUE)

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

# Clean up names
results <- results %>%
  mutate(
    group_clean = str_replace_all(group, "_", " ") %>% str_to_title(),
    # Categorize groups
    category = case_when(
      group %in% c("asian_indian", "bangladeshi", "pakistani", "sri_lankan") ~ "South Asian",
      group %in% c("chinese", "taiwanese", "japanese", "korean") ~ "East Asian",
      group %in% c("filipino", "vietnamese", "thai", "indonesian", "burmese", "cambodian") ~ "Southeast Asian",
      TRUE ~ "Other"
    ),
    # Mobilization score: high turnout correlation + high Mamdani correlation
    mobilization_score = cor_mamdani_nonzero + cor_turnout_nonzero
  ) %>%
  arrange(desc(mobilization_score))

cat("🎯 SUBGROUPS RANKED BY MOBILIZATION POTENTIAL\n")
cat("   (Mamdani support + Turnout correlation, non-zero EDs only)\n\n")
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
# IDENTIFY THE MOBILIZATION PROFILE
# ============================================================

cat("🎯 WHICH GROUPS SHOW MAMDANI'S BASE?\n")
cat("====================================\n\n")

cat("Groups with POSITIVE Mamdani correlation (his supporters):\n")
positive_mamdani <- results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero > 0.1) %>%
  arrange(desc(cor_mamdani_nonzero))

if (nrow(positive_mamdani) > 0) {
  for (i in 1:nrow(positive_mamdani)) {
    cat(sprintf("  ✓ %s: r=%.3f (n=%d EDs)\n",
                positive_mamdani$group_clean[i],
                positive_mamdani$cor_mamdani_nonzero[i],
                positive_mamdani$n_eds_nonzero[i]))
  }
} else {
  cat("  (None found with r > 0.1)\n")
}

cat("\nGroups with NEGATIVE Mamdani correlation (not his supporters):\n")
negative_mamdani <- results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero < -0.1) %>%
  arrange(cor_mamdani_nonzero)

if (nrow(negative_mamdani) > 0) {
  for (i in 1:nrow(negative_mamdani)) {
    cat(sprintf("  ✗ %s: r=%.3f (n=%d EDs)\n",
                negative_mamdani$group_clean[i],
                negative_mamdani$cor_mamdani_nonzero[i],
                negative_mamdani$n_eds_nonzero[i]))
  }
} else {
  cat("  (None found with r < -0.1)\n")
}

cat("\n")

cat("Groups with HIGH turnout correlation (mobilized):\n")
high_turnout <- results %>%
  filter(!is.na(cor_turnout_nonzero), cor_turnout_nonzero > 0.2) %>%
  arrange(desc(cor_turnout_nonzero))

if (nrow(high_turnout) > 0) {
  for (i in 1:nrow(high_turnout)) {
    cat(sprintf("  ↑ %s: r=%.3f (n=%d EDs)\n",
                high_turnout$group_clean[i],
                high_turnout$cor_turnout_nonzero[i],
                high_turnout$n_eds_nonzero[i]))
  }
} else {
  cat("  (None found with r > 0.2)\n")
}

cat("\n")

# ============================================================
# VISUALIZATION: 2D PLOT
# ============================================================

cat("📊 Creating visualizations...\n")

# Filter to groups with enough data
plot_data <- results %>%
  filter(n_eds_nonzero >= 5, !is.na(cor_mamdani_nonzero), !is.na(cor_turnout_nonzero))

# Quadrant plot
p1 <- ggplot(plot_data, aes(x = cor_mamdani_nonzero, y = cor_turnout_nonzero)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(size = n_eds_nonzero, color = category), alpha = 0.7) +
  geom_text(aes(label = group_clean), hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_size_continuous(name = "N EDs (>0)", range = c(2, 10)) +
  scale_color_brewer(palette = "Set2", name = "Region") +
  labs(
    title = "Asian Subgroup Electoral Patterns",
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

# Bar chart comparing all vs non-zero correlations
comparison_long <- results %>%
  filter(n_eds_nonzero >= 5) %>%
  select(group_clean, category, cor_mamdani_all, cor_mamdani_nonzero) %>%
  pivot_longer(cols = starts_with("cor_"), names_to = "type", values_to = "correlation") %>%
  mutate(type = if_else(type == "cor_mamdani_all", "All EDs", "Non-zero only"))

p2 <- ggplot(comparison_long, aes(x = reorder(group_clean, correlation), y = correlation, fill = type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("All EDs" = "#3182bd", "Non-zero only" = "#e6550d"), name = NULL) +
  labs(
    title = "Mamdani Support by Ethnic Group",
    subtitle = "Comparing all EDs vs EDs with population present",
    x = NULL,
    y = "Correlation with Mamdani %"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

combined <- p1 / p2 + plot_layout(heights = c(2, 1))

dir.create("outputs/analysis", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/analysis/subgroup_detailed_analysis.png",
       combined, width = 14, height = 16, dpi = 300)

cat("  ✓ Saved: outputs/analysis/subgroup_detailed_analysis.png\n\n")

# Save results
write_csv(results, "outputs/analysis/subgroup_correlations_detailed.csv")
cat("  ✓ Saved: outputs/analysis/subgroup_correlations_detailed.csv\n\n")

cat("✅ Detailed subgroup analysis complete!\n")
