# ============================================================
# South Asian Detailed Subgroup Analysis
# ============================================================
# Deep dive into South Asian ethnicities and Mamdani support
# (Indian, Bangladeshi, Pakistani, Sri Lankan, Nepali, etc.)

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🇮🇳 SOUTH ASIAN DETAILED SUBGROUP ANALYSIS\n")
cat("===========================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading data...\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# Get detailed Asian ancestry data (B02015) - South Asian focus
south_asian_vars <- c(
  total_asian = "B02015_001",
  # South Asian detailed
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  bhutanese = "B02015_020",  # Sometimes grouped with Nepali
  nepali = "B02015_021",      # May be in "Other Asian"
  pakistani = "B02015_006",
  sri_lankan = "B02015_007"
)

cat("  Downloading South Asian ethnicity data (B02015)...\n")
queens_south_asian <- get_acs(
  geography = "tract",
  variables = south_asian_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Also get broader "Asian alone" by detailed group (B02001 derivatives)
# This captures more detail
cat("  Downloading additional South Asian data...\n")

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_south_asian))

# Spatial join for ethnicity data
cat("  Performing spatial joins...\n")
queens_south_asian <- queens_south_asian %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

ed_south_asian <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_south_asian
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# Define specific ethnic groups to analyze
ethnic_groups <- c("asian_indian", "bangladeshi", "pakistani", "sri_lankan", "bhutanese", "nepali")

# Aggregate to ED level
ed_south_asian_agg <- ed_south_asian %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(ethnic_groups), ~sum(.x * area_weight, na.rm = TRUE)),
    total_asian = sum(total_asian * area_weight, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total South Asian
ed_south_asian_agg <- ed_south_asian_agg %>%
  mutate(
    total_south_asian = asian_indian + bangladeshi + pakistani + sri_lankan + bhutanese + nepali
  )

# Load election results and demographics
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, Sliwa_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Merge everything
merged <- election_results %>%
  left_join(ed_south_asian_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0) %>%
  mutate(
    turnout_per_1000 = 1000 * total_votes / total_pop,
    # Calculate percentages for each group
    across(all_of(c(ethnic_groups, "total_south_asian")),
           ~100 * .x / total_pop, .names = "pct_{.col}")
  ) %>%
  filter(is.finite(turnout_per_1000))

cat("✓ Merged data for", nrow(merged), "EDs\n\n")

# ============================================================
# DESCRIPTIVE STATISTICS
# ============================================================

cat("📊 SOUTH ASIAN POPULATION IN AD34\n")
cat("===================================\n\n")

pop_summary <- merged %>%
  summarize(
    across(all_of(ethnic_groups),
           list(
             total = ~sum(.x, na.rm = TRUE),
             n_eds = ~sum(.x > 0, na.rm = TRUE),
             mean = ~mean(.x, na.rm = TRUE),
             max = ~max(.x, na.rm = TRUE)
           ))
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("group", "stat"), sep = "_(?=total|n_eds|mean|max)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(desc(total))

cat(sprintf("%-20s %12s %10s %12s %12s\n",
            "Group", "Total Pop", "N EDs", "Mean/ED", "Max/ED"))
cat(rep("-", 70), "\n")
for (i in 1:nrow(pop_summary)) {
  cat(sprintf("%-20s %12.0f %10.0f %12.1f %12.0f\n",
              str_to_title(str_replace_all(pop_summary$group[i], "_", " ")),
              pop_summary$total[i],
              pop_summary$n_eds[i],
              pop_summary$mean[i],
              pop_summary$max[i]))
}

cat("\n")
cat("Total South Asian:", round(sum(merged$total_south_asian)), "\n")
cat("As % of total AD34 population:",
    round(100 * sum(merged$total_south_asian) / sum(merged$total_pop), 1), "%\n\n")

# ============================================================
# CORRELATION ANALYSIS
# ============================================================

cat("📊 CORRELATION WITH MAMDANI SUPPORT\n")
cat("====================================\n\n")

# Calculate correlations for each ethnic group
results <- tibble()

for (group in c(ethnic_groups, "total_south_asian")) {
  raw_var <- group

  # Skip if no variance
  if (sd(merged[[raw_var]], na.rm = TRUE) == 0) next

  # Count non-zero EDs
  n_nonzero <- sum(merged[[raw_var]] > 0, na.rm = TRUE)

  # Total population
  total_pop <- sum(merged[[raw_var]], na.rm = TRUE)

  # Correlations (all EDs)
  cor_mamdani <- cor(merged[[raw_var]], merged$Mamdani_pct, use = "complete.obs")
  cor_cuomo <- cor(merged[[raw_var]], merged$Cuomo_pct, use = "complete.obs")
  cor_sliwa <- cor(merged[[raw_var]], merged$Sliwa_pct, use = "complete.obs")
  cor_turnout <- cor(merged[[raw_var]], merged$turnout_per_1000, use = "complete.obs")

  # Correlations (non-zero only)
  if (n_nonzero >= 3) {
    nonzero_data <- merged %>% filter(.data[[raw_var]] > 0)
    cor_mamdani_nz <- cor(nonzero_data[[raw_var]], nonzero_data$Mamdani_pct, use = "complete.obs")
    cor_cuomo_nz <- cor(nonzero_data[[raw_var]], nonzero_data$Cuomo_pct, use = "complete.obs")
    cor_sliwa_nz <- cor(nonzero_data[[raw_var]], nonzero_data$Sliwa_pct, use = "complete.obs")
    cor_turnout_nz <- cor(nonzero_data[[raw_var]], nonzero_data$turnout_per_1000, use = "complete.obs")
  } else {
    cor_mamdani_nz <- NA
    cor_cuomo_nz <- NA
    cor_sliwa_nz <- NA
    cor_turnout_nz <- NA
  }

  # Average population
  avg_pop <- mean(merged[[raw_var]], na.rm = TRUE)

  results <- bind_rows(results, tibble(
    group = group,
    n_eds_nonzero = n_nonzero,
    total_population = total_pop,
    avg_population = avg_pop,
    cor_mamdani_all = cor_mamdani,
    cor_mamdani_nonzero = cor_mamdani_nz,
    cor_cuomo_all = cor_cuomo,
    cor_cuomo_nonzero = cor_cuomo_nz,
    cor_sliwa_all = cor_sliwa,
    cor_sliwa_nonzero = cor_sliwa_nz,
    cor_turnout_all = cor_turnout,
    cor_turnout_nonzero = cor_turnout_nz
  ))
}

# Clean up names
results <- results %>%
  mutate(
    group_clean = str_replace_all(group, "_", " ") %>% str_to_title(),
    mobilization_score = cor_mamdani_nonzero + cor_turnout_nonzero
  ) %>%
  arrange(desc(cor_mamdani_nonzero))

cat("🎯 CORRELATIONS WITH ELECTORAL OUTCOMES (All EDs)\n\n")
cat(sprintf("%-25s %8s %10s %10s %10s %10s\n",
            "Group", "N(>0)", "Mamdani", "Cuomo", "Sliwa", "Turnout"))
cat(rep("-", 80), "\n")

for (i in 1:nrow(results)) {
  cat(sprintf("%-25s %8d %10.3f %10.3f %10.3f %10.3f\n",
              results$group_clean[i],
              results$n_eds_nonzero[i],
              results$cor_mamdani_all[i],
              results$cor_cuomo_all[i],
              results$cor_sliwa_all[i],
              results$cor_turnout_all[i]))
}

cat("\n")
cat("🎯 CORRELATIONS (Non-zero EDs only)\n\n")
cat(sprintf("%-25s %8s %10s %10s %10s %10s\n",
            "Group", "N(>0)", "Mamdani", "Cuomo", "Sliwa", "Turnout"))
cat(rep("-", 80), "\n")

for (i in 1:nrow(results)) {
  if (!is.na(results$cor_mamdani_nonzero[i])) {
    cat(sprintf("%-25s %8d %10.3f %10.3f %10.3f %10.3f\n",
                results$group_clean[i],
                results$n_eds_nonzero[i],
                results$cor_mamdani_nonzero[i],
                results$cor_cuomo_nonzero[i],
                results$cor_sliwa_nonzero[i],
                results$cor_turnout_nonzero[i]))
  }
}

cat("\n")

# ============================================================
# KEY FINDINGS
# ============================================================

cat("🎯 KEY FINDINGS\n")
cat("================\n\n")

cat("MAMDANI SUPPORT:\n")
positive <- results %>% filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero > 0)
negative <- results %>% filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero < 0)

if (nrow(positive) > 0) {
  cat("  Groups with POSITIVE correlation:\n")
  for (i in 1:nrow(positive)) {
    cat(sprintf("    ✓ %s: r=%.3f (n=%d EDs, pop=%.0f)\n",
                positive$group_clean[i],
                positive$cor_mamdani_nonzero[i],
                positive$n_eds_nonzero[i],
                positive$total_population[i]))
  }
} else {
  cat("  (None)\n")
}

if (nrow(negative) > 0) {
  cat("\n  Groups with NEGATIVE correlation:\n")
  for (i in 1:nrow(negative)) {
    cat(sprintf("    ✗ %s: r=%.3f (n=%d EDs, pop=%.0f)\n",
                negative$group_clean[i],
                negative$cor_mamdani_nonzero[i],
                negative$n_eds_nonzero[i],
                negative$total_population[i]))
  }
}

cat("\n")
cat("CUOMO SUPPORT (Opponent):\n")
cuomo_positive <- results %>%
  filter(!is.na(cor_cuomo_nonzero), cor_cuomo_nonzero > 0.1) %>%
  arrange(desc(cor_cuomo_nonzero))

if (nrow(cuomo_positive) > 0) {
  for (i in 1:nrow(cuomo_positive)) {
    cat(sprintf("  ✓ %s: r=%.3f (Cuomo supporter)\n",
                cuomo_positive$group_clean[i],
                cuomo_positive$cor_cuomo_nonzero[i]))
  }
} else {
  cat("  (No strong Cuomo correlations)\n")
}

cat("\n")

# ============================================================
# DETAILED ED-LEVEL ANALYSIS
# ============================================================

cat("📍 EDS WITH HIGHEST SOUTH ASIAN POPULATIONS\n")
cat("============================================\n\n")

top_eds <- merged %>%
  arrange(desc(total_south_asian)) %>%
  head(10) %>%
  select(ED, ED_num, total_pop, total_south_asian, asian_indian, bangladeshi,
         pakistani, Mamdani_pct, Cuomo_pct, turnout_per_1000)

cat(sprintf("%-6s %8s %10s %10s %10s %10s\n",
            "ED", "Total", "S.Asian", "Indian", "Bengali", "Mamdani%"))
cat(rep("-", 60), "\n")
for (i in 1:nrow(top_eds)) {
  cat(sprintf("%-6s %8.0f %10.0f %10.0f %10.0f %10.1f\n",
              top_eds$ED[i],
              top_eds$total_pop[i],
              top_eds$total_south_asian[i],
              top_eds$asian_indian[i],
              top_eds$bangladeshi[i],
              top_eds$Mamdani_pct[i]))
}

cat("\n")

# ============================================================
# VISUALIZATIONS
# ============================================================

cat("📊 Creating visualizations...\n")

# Filter for plotting (groups with at least some presence)
plot_data <- results %>%
  filter(group != "total_south_asian", n_eds_nonzero >= 3, !is.na(cor_mamdani_nonzero))

if (nrow(plot_data) > 0) {

  # 1. Bar chart of all candidates
  candidates_long <- plot_data %>%
    select(group_clean, n_eds_nonzero, cor_mamdani_nonzero, cor_cuomo_nonzero, cor_sliwa_nonzero) %>%
    pivot_longer(cols = starts_with("cor_"), names_to = "candidate", values_to = "correlation") %>%
    mutate(candidate = case_when(
      candidate == "cor_mamdani_nonzero" ~ "Mamdani",
      candidate == "cor_cuomo_nonzero" ~ "Cuomo",
      candidate == "cor_sliwa_nonzero" ~ "Sliwa"
    ))

  p1 <- ggplot(candidates_long, aes(x = reorder(group_clean, correlation),
                                     y = correlation,
                                     fill = candidate)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    coord_flip() +
    scale_fill_manual(values = c("Mamdani" = "#e41a1c",
                                  "Cuomo" = "#377eb8",
                                  "Sliwa" = "#4daf4a"),
                      name = "Candidate") +
    labs(
      title = "South Asian Support by Candidate",
      subtitle = "Correlations in EDs with South Asian population present",
      x = NULL,
      y = "Correlation"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  # 2. Mobilization plot
  p2 <- ggplot(plot_data, aes(x = cor_mamdani_nonzero, y = cor_turnout_nonzero)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = total_population), alpha = 0.7, color = "#e41a1c") +
    geom_text(aes(label = group_clean), hjust = -0.1, vjust = -0.5, size = 4) +
    scale_size_continuous(name = "Total Population", range = c(3, 12)) +
    labs(
      title = "South Asian Electoral Mobilization",
      subtitle = "Mamdani support vs turnout correlation",
      x = "Correlation with Mamdani Support",
      y = "Correlation with Turnout"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  # 3. Population vs support scatter
  scatter_data <- merged %>%
    filter(total_south_asian > 0) %>%
    mutate(pct_south_asian = 100 * total_south_asian / total_pop)

  p3 <- ggplot(scatter_data, aes(x = pct_south_asian, y = Mamdani_pct)) +
    geom_point(aes(size = total_votes), alpha = 0.6, color = "#e41a1c") +
    geom_smooth(method = "lm", se = TRUE, color = "#377eb8", linewidth = 1.5) +
    labs(
      title = "South Asian Population % vs Mamdani Support",
      subtitle = paste0("Correlation: r=",
                       round(cor(scatter_data$pct_south_asian,
                                scatter_data$Mamdani_pct, use = "complete.obs"), 3)),
      x = "South Asian % of ED Population",
      y = "Mamdani Vote %",
      size = "Total Votes"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  # Combine plots
  combined <- (p1 | p2) / p3 +
    plot_annotation(
      title = "South Asian Electoral Analysis - AD34",
      subtitle = "2025 Mayoral Election",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )

  dir.create("outputs/analysis", showWarnings = FALSE, recursive = TRUE)
  ggsave("outputs/analysis/south_asian_detailed_analysis.png",
         combined, width = 16, height = 14, dpi = 300)

  cat("  ✓ Saved: outputs/analysis/south_asian_detailed_analysis.png\n\n")
}

# Save results
write_csv(results, "outputs/analysis/south_asian_correlations_detailed.csv")
cat("  ✓ Saved: outputs/analysis/south_asian_correlations_detailed.csv\n\n")

# Save ED-level data
ed_level <- merged %>%
  select(ED, ED_num, total_pop, total_south_asian, asian_indian, bangladeshi,
         pakistani, sri_lankan, Mamdani_pct, Cuomo_pct, Sliwa_pct,
         turnout_per_1000) %>%
  arrange(desc(total_south_asian))

write_csv(ed_level, "outputs/analysis/south_asian_by_ed.csv")
cat("  ✓ Saved: outputs/analysis/south_asian_by_ed.csv\n\n")

cat("✅ South Asian detailed analysis complete!\n")
