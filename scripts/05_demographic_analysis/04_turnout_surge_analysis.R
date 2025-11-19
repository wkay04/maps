# ============================================================
# Turnout Surge Analysis - Testing New Voter Mobilization Hypothesis
# ============================================================
# Does Mamdani's lower correlation with South Asian % reflect
# NEW VOTER mobilization in those communities?

library(tidyverse)
library(tidycensus)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n🗳️  TURNOUT SURGE ANALYSIS\n")
cat("=========================\n\n")

cat("HYPOTHESIS: Mamdani mobilized NEW voters in South Asian neighborhoods,\n")
cat("            which explains the paradox of negative correlation despite\n")
cat("            South Asians being his base of support.\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading data...\n")

# Election results with Mamdani support
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, total_votes)

# Demographics
demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Get Asian ethnicity data (we'll reload this)
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

asian_vars <- c(
  total_asian = "B02015_001",
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  pakistani = "B02015_006",
  sri_lankan = "B02015_007"
)

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

# Load ED shapefile for spatial join
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_asian))

# Spatial join
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

ed_asian_agg <- ed_asian %>%
  group_by(ED, ED_num) %>%
  summarize(
    south_asian = sum((asian_indian + bangladeshi + pakistani + sri_lankan) * area_weight, na.rm = TRUE),
    .groups = "drop"
  )

# Merge all data
merged <- election_results %>%
  left_join(ed_asian_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct)) %>%
  filter(total_pop > 0) %>%  # Filter out EDs with no population
  mutate(
    pct_south_asian = 100 * south_asian / total_pop,
    turnout_per_1000 = 1000 * total_votes / total_pop
  ) %>%
  filter(is.finite(turnout_per_1000))  # Remove any Inf values

cat("✓ Loaded data for", nrow(merged), "EDs\n\n")

# ============================================================
# CALCULATE TURNOUT METRICS
# ============================================================

cat("📊 TURNOUT ANALYSIS\n")
cat("====================\n\n")

# Compare EDs by South Asian percentage
merged <- merged %>%
  mutate(
    south_asian_tercile = case_when(
      pct_south_asian < quantile(pct_south_asian, 0.33, na.rm = TRUE) ~ "Low",
      pct_south_asian < quantile(pct_south_asian, 0.67, na.rm = TRUE) ~ "Medium",
      TRUE ~ "High"
    )
  )

turnout_by_group <- merged %>%
  group_by(south_asian_tercile) %>%
  summarize(
    n_eds = n(),
    avg_south_asian_pct = mean(pct_south_asian, na.rm = TRUE),
    avg_mamdani_pct = mean(Mamdani_pct, na.rm = TRUE),
    avg_turnout_per_1000 = mean(turnout_per_1000, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(south_asian_tercile, c("Low", "Medium", "High")))

cat("Turnout by South Asian Population Level:\n\n")
cat(sprintf("%-12s %8s %12s %12s %15s\n",
            "SA Level", "N EDs", "Avg SA %", "Mamdani %", "Turnout/1000"))
cat(rep("-", 65), "\n")

for (i in 1:nrow(turnout_by_group)) {
  cat(sprintf("%-12s %8d %11.1f%% %11.1f%% %15.1f\n",
              turnout_by_group$south_asian_tercile[i],
              turnout_by_group$n_eds[i],
              turnout_by_group$avg_south_asian_pct[i],
              turnout_by_group$avg_mamdani_pct[i],
              turnout_by_group$avg_turnout_per_1000[i]))
}

cat("\n")

# ============================================================
# TEST: DOES TURNOUT MODERATE THE CORRELATION?
# ============================================================

cat("🔬 TESTING HYPOTHESIS: Does turnout explain the paradox?\n")
cat("==========================================================\n\n")

# Correlation: South Asian % with Mamdani support (original finding)
cor_simple <- cor(merged$pct_south_asian, merged$Mamdani_pct, use = "complete.obs")

cat(sprintf("Simple correlation (SA %% → Mamdani %%): %.3f\n", cor_simple))
cat("  ⚠️  Negative! But we think SA is his base...\n\n")

# Correlation: South Asian % with turnout
cor_turnout <- cor(merged$pct_south_asian, merged$turnout_per_1000, use = "complete.obs")

cat(sprintf("Correlation (SA %% → Turnout): %.3f\n", cor_turnout))

if (!is.na(cor_turnout) && cor_turnout > 0.1) {
  cat("  ✓ Positive! South Asian neighborhoods HAD higher turnout\n\n")
} else if (!is.na(cor_turnout) && cor_turnout < -0.1) {
  cat("  ✗ Negative! South Asian neighborhoods had LOWER turnout\n\n")
} else {
  cat("  → Weak/no relationship\n\n")
}

# Correlation: Turnout with Mamdani support
cor_turnout_mamdani <- cor(merged$turnout_per_1000, merged$Mamdani_pct, use = "complete.obs")

cat(sprintf("Correlation (Turnout → Mamdani %%): %.3f\n", cor_turnout_mamdani))

if (!is.na(cor_turnout_mamdani) && cor_turnout_mamdani > 0.1) {
  cat("  ✓ Positive! Higher turnout = more Mamdani support\n\n")
} else {
  cat("  → Weak/no relationship\n\n")
}

# Partial correlation (controlling for turnout)
# If the negative SA correlation goes away when we control for turnout,
# it suggests turnout is a key factor

# Simple approach: residualize
model_mamdani_turnout <- lm(Mamdani_pct ~ turnout_per_1000, data = merged)
model_sa_turnout <- lm(pct_south_asian ~ turnout_per_1000, data = merged)

merged$mamdani_resid <- residuals(model_mamdani_turnout)
merged$sa_resid <- residuals(model_sa_turnout)

cor_partial <- cor(merged$sa_resid, merged$mamdani_resid, use = "complete.obs")

cat(sprintf("Partial correlation (SA %% → Mamdani %%, controlling for turnout): %.3f\n", cor_partial))

if (abs(cor_partial) < abs(cor_simple)) {
  cat("  ✓ SUPPORTS HYPOTHESIS! The negative correlation WEAKENS when\n")
  cat("    we account for turnout differences.\n\n")
} else {
  cat("  ✗ Hypothesis not supported by turnout alone.\n\n")
}

# ============================================================
# VISUALIZATION: TURNOUT AS MEDIATOR
# ============================================================

cat("📊 Creating visualizations...\n")

library(ggplot2)
library(patchwork)

# Plot 1: South Asian % vs Mamdani %
p1 <- ggplot(merged, aes(x = pct_south_asian, y = Mamdani_pct)) +
  geom_point(aes(size = total_votes, color = turnout_per_1000), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  scale_color_viridis_c(option = "plasma", name = "Turnout\nper 1000") +
  labs(
    title = "South Asian % vs Mamdani Support",
    subtitle = sprintf("r = %.3f (negative!)", cor_simple),
    x = "South Asian Population %",
    y = "Mamdani Vote %",
    size = "Total Votes"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Plot 2: South Asian % vs Turnout
p2 <- ggplot(merged, aes(x = pct_south_asian, y = turnout_per_1000)) +
  geom_point(aes(size = total_votes, color = Mamdani_pct), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 1) +
  scale_color_viridis_c(option = "mako", name = "Mamdani %") +
  labs(
    title = "South Asian % vs Turnout",
    subtitle = sprintf("r = %.3f", cor_turnout),
    x = "South Asian Population %",
    y = "Turnout per 1000 residents",
    size = "Total Votes"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Plot 3: Turnout vs Mamdani
p3 <- ggplot(merged, aes(x = turnout_per_1000, y = Mamdani_pct)) +
  geom_point(aes(size = total_votes, color = pct_south_asian), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1) +
  scale_color_viridis_c(option = "cividis", name = "South\nAsian %") +
  labs(
    title = "Turnout vs Mamdani Support",
    subtitle = sprintf("r = %.3f", cor_turnout_mamdani),
    x = "Turnout per 1000 residents",
    y = "Mamdani Vote %",
    size = "Total Votes"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

combined <- (p1 / p2 / p3) +
  plot_annotation(
    title = "Testing New Voter Mobilization Hypothesis",
    subtitle = "Do South Asian neighborhoods show unusual turnout patterns?",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

dir.create("outputs/analysis", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/analysis/turnout_mediation_analysis.png",
       combined, width = 10, height = 14, dpi = 300)

cat("  ✓ Saved: outputs/analysis/turnout_mediation_analysis.png\n\n")

# Save results
results <- data.frame(
  metric = c("SA% → Mamdani%", "SA% → Turnout", "Turnout → Mamdani%",
             "SA% → Mamdani% (partial)"),
  correlation = c(cor_simple, cor_turnout, cor_turnout_mamdani, cor_partial)
)

write_csv(results, "outputs/analysis/turnout_mediation_correlations.csv")
write_csv(turnout_by_group, "outputs/analysis/turnout_by_south_asian_level.csv")

cat("✅ Analysis complete!\n\n")

# ============================================================
# INTERPRETATION
# ============================================================

cat("💡 INTERPRETATION\n")
cat("==================\n\n")

if (!is.na(cor_turnout) && !is.na(cor_turnout_mamdani) && !is.na(cor_partial) &&
    cor_turnout > 0.1 && cor_turnout_mamdani > 0.1 && abs(cor_partial) < abs(cor_simple)) {
  cat("✓ EVIDENCE SUPPORTS NEW VOTER MOBILIZATION HYPOTHESIS:\n\n")
  cat("  1. South Asian neighborhoods had HIGHER turnout\n")
  cat("  2. Higher turnout correlated with MORE Mamdani support\n")
  cat("  3. The negative SA-Mamdani correlation weakens when controlling for turnout\n\n")
  cat("  CONCLUSION: Mamdani likely mobilized NEW voters in South Asian\n")
  cat("              communities, masking his true base of support.\n\n")
} else if (cor_turnout < -0.1) {
  cat("⚠️  MIXED EVIDENCE:\n\n")
  cat("  South Asian neighborhoods actually had LOWER turnout.\n")
  cat("  This doesn't support simple new voter mobilization.\n\n")
  cat("  Alternative explanations:\n")
  cat("  - Mamdani's support is in SPECIFIC South Asian subgroups\n")
  cat("  - Other demographic factors are confounding\n")
  cat("  - Electoral dynamics beyond simple turnout\n\n")
} else {
  cat("→ INCONCLUSIVE:\n\n")
  cat("  Turnout patterns don't clearly explain the paradox.\n")
  cat("  Need individual voter file data to test properly.\n\n")
}

cat("📌 NEXT STEPS:\n")
cat("  1. Request NYC BOE voter file (FOIL request)\n")
cat("  2. Identify first-time voters in 2025\n")
cat("  3. Analyze their geographic distribution\n")
cat("  4. Compare to established voter patterns\n\n")
