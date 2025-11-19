# ============================================================
# Asian Ethnic Correlations - Comparing Zero vs Non-Zero Analysis
# ============================================================

library(tidyverse)

setwd("~/Mayoral Results AD34")

cat("\n📊 Comparing Correlation Methods: With vs Without Zeros\n")
cat("=========================================================\n\n")

# Load the merged data (saved from previous script)
# We'll recreate it from the intermediate files
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Load Asian ethnicity data from the previous run
# We need to re-run the spatial join
library(tidycensus)
library(sf)

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

asian_vars <- c(
  total_asian = "B02015_001",
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  pakistani = "B02015_006",
  sri_lankan = "B02015_007",
  chinese = "B02015_009",
  japanese = "B02015_013",
  korean = "B02015_014",
  filipino = "B02015_010",
  vietnamese = "B02015_018",
  thai = "B02015_016",
  other_asian = "B02015_019"
)

cat("📥 Loading census data...\n")
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

cat("📥 Loading election districts...\n")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_asian))

cat("🗺️  Performing spatial join...\n")
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
    total_asian_pop = sum(total_asian * area_weight, na.rm = TRUE),
    asian_indian = sum(asian_indian * area_weight, na.rm = TRUE),
    bangladeshi = sum(bangladeshi * area_weight, na.rm = TRUE),
    pakistani = sum(pakistani * area_weight, na.rm = TRUE),
    sri_lankan = sum(sri_lankan * area_weight, na.rm = TRUE),
    chinese = sum(chinese * area_weight, na.rm = TRUE),
    japanese = sum(japanese * area_weight, na.rm = TRUE),
    korean = sum(korean * area_weight, na.rm = TRUE),
    filipino = sum(filipino * area_weight, na.rm = TRUE),
    vietnamese = sum(vietnamese * area_weight, na.rm = TRUE),
    thai = sum(thai * area_weight, na.rm = TRUE),
    other_asian = sum(other_asian * area_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    south_asian = asian_indian + bangladeshi + pakistani + sri_lankan,
    east_asian = chinese + japanese + korean,
    southeast_asian = filipino + vietnamese + thai
  )

merged <- election_results %>%
  left_join(ed_asian_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct)) %>%
  mutate(
    pct_asian_indian = 100 * asian_indian / total_pop,
    pct_bangladeshi = 100 * bangladeshi / total_pop,
    pct_pakistani = 100 * pakistani / total_pop,
    pct_chinese = 100 * chinese / total_pop,
    pct_korean = 100 * korean / total_pop,
    pct_filipino = 100 * filipino / total_pop,
    pct_south_asian = 100 * south_asian / total_pop,
    pct_east_asian = 100 * east_asian / total_pop,
    pct_southeast_asian = 100 * southeast_asian / total_pop
  )

cat("✓ Data prepared for", nrow(merged), "EDs\n\n")

# ============================================================
# CORRELATION ANALYSIS - TWO METHODS
# ============================================================

cat("📊 METHOD 1: Including ALL EDs (with zeros)\n")
cat("============================================\n\n")

# Select ethnic groups
ethnic_vars <- c("south_asian", "asian_indian", "bangladeshi", "pakistani",
                 "east_asian", "chinese", "korean", "japanese",
                 "southeast_asian", "filipino", "vietnamese", "thai")

# Method 1: All EDs
cor_all <- merged %>%
  select(Mamdani_pct, all_of(ethnic_vars)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

cor_matrix_all <- cor(cor_all, use = "pairwise.complete.obs")

results_all <- cor_matrix_all[, "Mamdani_pct", drop = FALSE] %>%
  as.data.frame() %>%
  rownames_to_column("group") %>%
  rename(correlation = `Mamdani_pct`) %>%
  filter(group != "Mamdani_pct") %>%
  mutate(
    n_eds = nrow(merged),
    n_nonzero = map_int(group, ~sum(merged[[.x]] > 0, na.rm = TRUE))
  ) %>%
  arrange(desc(abs(correlation)))

cat("Results (all EDs):\n")
for (i in 1:nrow(results_all)) {
  cat(sprintf("  %s: r=%.3f (n=%d, %d with pop>0)\n",
              str_to_title(str_replace_all(results_all$group[i], "_", " ")),
              results_all$correlation[i],
              results_all$n_eds[i],
              results_all$n_nonzero[i]))
}

cat("\n📊 METHOD 2: Only EDs with NON-ZERO populations\n")
cat("================================================\n\n")

# Method 2: Only non-zero
results_nonzero <- data.frame()

for (var in ethnic_vars) {
  if (var %in% names(merged)) {
    # Filter to only EDs with this ethnic group present
    filtered <- merged %>%
      filter(.data[[var]] > 0)

    if (nrow(filtered) >= 3 && sd(filtered[[var]], na.rm = TRUE) > 0) {
      cor_val <- cor(filtered$Mamdani_pct, filtered[[var]], use = "pairwise.complete.obs")

      results_nonzero <- bind_rows(results_nonzero, data.frame(
        group = var,
        correlation = cor_val,
        n_eds = nrow(filtered)
      ))
    }
  }
}

results_nonzero <- results_nonzero %>%
  arrange(desc(abs(correlation)))

cat("Results (non-zero EDs only):\n")
for (i in 1:nrow(results_nonzero)) {
  cat(sprintf("  %s: r=%.3f (n=%d EDs with pop>0)\n",
              str_to_title(str_replace_all(results_nonzero$group[i], "_", " ")),
              results_nonzero$correlation[i],
              results_nonzero$n_eds[i]))
}

# ============================================================
# COMPARISON TABLE
# ============================================================

cat("\n📊 SIDE-BY-SIDE COMPARISON\n")
cat("============================\n\n")

comparison <- results_all %>%
  select(group, cor_all = correlation, n_all = n_eds, n_nonzero) %>%
  left_join(
    results_nonzero %>% select(group, cor_nonzero = correlation, n_nonzero_only = n_eds),
    by = "group"
  ) %>%
  mutate(
    difference = cor_nonzero - cor_all,
    group_clean = str_to_title(str_replace_all(group, "_", " "))
  ) %>%
  arrange(desc(abs(cor_all)))

cat(sprintf("%-20s %10s %10s %10s %8s\n", "Group", "All EDs", "Non-Zero", "Diff", "N(>0)"))
cat(rep("-", 70), "\n")

for (i in 1:nrow(comparison)) {
  cat(sprintf("%-20s %10.3f %10s %10s %8d\n",
              comparison$group_clean[i],
              comparison$cor_all[i],
              ifelse(is.na(comparison$cor_nonzero[i]), "N/A", sprintf("%.3f", comparison$cor_nonzero[i])),
              ifelse(is.na(comparison$difference[i]), "N/A", sprintf("%.3f", comparison$difference[i])),
              comparison$n_nonzero[i]))
}

# Save results
write_csv(comparison, "outputs/analysis/asian_correlations_comparison.csv")
cat("\n✅ Saved: outputs/analysis/asian_correlations_comparison.csv\n")

# ============================================================
# INTERPRETATION
# ============================================================

cat("\n💡 INTERPRETATION GUIDE\n")
cat("=======================\n\n")

cat("If correlations are SIMILAR between methods:\n")
cat("  → The relationship is consistent whether the group is present or absent\n")
cat("  → Population size matters proportionally across all EDs\n\n")

cat("If correlation is STRONGER when excluding zeros:\n")
cat("  → Among EDs with this population, size matters more\n")
cat("  → The relationship is dose-dependent\n\n")

cat("If correlation is WEAKER when excluding zeros:\n")
cat("  → The effect is mainly about presence/absence, not size\n")
cat("  → Binary effect: having the population vs not having it\n\n")

cat("If correlation CHANGES SIGN:\n")
cat("  → Very different dynamics in EDs with vs without this population\n")
cat("  → Interpret with caution - may indicate confounding\n\n")

cat("✅ Analysis complete!\n")
