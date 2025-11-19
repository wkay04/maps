# ============================================================
# CORRECTED COMPREHENSIVE ETHNIC ANALYSIS
# ============================================================
# Using the best Census table for each ethnic group:
# - B02015: Asian subgroups (race)
# - B03001: Hispanic subgroups (ethnicity)
# - B04006: White ethnic subgroups (ancestry)

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🔍 CORRECTED COMPREHENSIVE ETHNIC ANALYSIS\n")
cat("===========================================\n")
cat("Using optimal Census tables for each group\n\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# ============================================================
# PART 1: ASIAN SUBGROUPS (B02015)
# ============================================================

cat("📊 PART 1: ASIAN SUBGROUPS (B02015 - Race)\n")
cat("============================================\n\n")

asian_vars <- c(
  total_asian = "B02015_001",
  # East Asian
  chinese = "B02015_002",
  hmong = "B02015_003",
  japanese = "B02015_004",
  korean = "B02015_005",
  taiwanese = "B02015_008",
  # Southeast Asian
  burmese = "B02015_010",
  cambodian = "B02015_011",
  filipino = "B02015_012",
  indonesian = "B02015_013",
  laotian = "B02015_014",
  thai = "B02015_018",
  vietnamese = "B02015_019",
  # South Asian
  asian_indian = "B02015_021",
  bangladeshi = "B02015_022",
  bhutanese = "B02015_023",
  nepali = "B02015_024",
  pakistani = "B02015_025",
  sri_lankan = "B02015_027"
)

cat("  Downloading Asian data (B02015)...\n")
asian_data <- get_acs(
  geography = "tract",
  variables = asian_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

# ============================================================
# PART 2: HISPANIC SUBGROUPS (B03001)
# ============================================================

cat("\n📊 PART 2: HISPANIC SUBGROUPS (B03001 - Ethnicity)\n")
cat("===================================================\n\n")

hispanic_vars <- c(
  total_hispanic = "B03001_003",
  mexican = "B03001_004",
  puerto_rican = "B03001_005",
  cuban = "B03001_006",
  dominican = "B03001_007",
  guatemalan = "B03001_010",
  honduran = "B03001_011",
  nicaraguan = "B03001_012",
  salvadoran = "B03001_014",
  colombian = "B03001_020",
  ecuadorian = "B03001_021",
  peruvian = "B03001_023",
  venezuelan = "B03001_025",
  bolivian = "B03001_018"
)

cat("  Downloading Hispanic data (B03001)...\n")
hispanic_data <- get_acs(
  geography = "tract",
  variables = hispanic_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = FALSE,
  output = "wide"
)

# ============================================================
# PART 3: WHITE ETHNIC SUBGROUPS (B04006)
# ============================================================

cat("\n📊 PART 3: WHITE ETHNIC SUBGROUPS (B04006 - Ancestry)\n")
cat("======================================================\n\n")

white_vars <- c(
  italian = "B04006_043",
  irish = "B04006_044",
  german = "B04006_041",
  polish = "B04006_056",
  russian = "B04006_062",
  greek = "B04006_042",
  ukrainian = "B04006_079",
  czech = "B04006_031",
  norwegian = "B04006_054"
)

cat("  Downloading White ethnic data (B04006)...\n")
white_data <- get_acs(
  geography = "tract",
  variables = white_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = FALSE,
  output = "wide"
)

# ============================================================
# SPATIAL JOIN TO ELECTION DISTRICTS
# ============================================================

cat("\n📍 SPATIAL JOINING TO ELECTION DISTRICTS\n")
cat("=========================================\n\n")

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(asian_data))

# Function to aggregate tract data to ED level
aggregate_to_ed <- function(tract_data, ed_shape) {
  # Add tract area
  tract_data <- tract_data %>%
    mutate(tract_area = as.numeric(st_area(geometry)))

  # Spatial intersection
  intersection <- st_intersection(
    ed_shape %>% select(ED, ED_num, geometry),
    tract_data
  ) %>%
    mutate(
      intersection_area = as.numeric(st_area(geometry)),
      area_weight = intersection_area / tract_area
    ) %>%
    st_drop_geometry()

  # Get estimate columns
  estimate_cols <- names(intersection)[str_detect(names(intersection), "E$")]
  estimate_cols <- estimate_cols[sapply(intersection[estimate_cols], is.numeric)]

  # Aggregate
  ed_aggregated <- intersection %>%
    group_by(ED, ED_num) %>%
    summarize(
      across(all_of(estimate_cols), ~sum(.x * area_weight, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename_with(~str_remove(.x, "E$"), ends_with("E"))

  return(ed_aggregated)
}

cat("  Processing Asian data...\n")
asian_ed <- aggregate_to_ed(asian_data, ad34_shp)

cat("  Processing Hispanic data...\n")
# Hispanic data needs geometry first
hispanic_data_sf <- asian_data %>%
  select(GEOID, geometry) %>%
  left_join(hispanic_data %>% select(-NAME), by = "GEOID")
hispanic_ed <- aggregate_to_ed(hispanic_data_sf, ad34_shp)

cat("  Processing White ethnic data...\n")
white_data_sf <- asian_data %>%
  select(GEOID, geometry) %>%
  left_join(white_data %>% select(-NAME), by = "GEOID")
white_ed <- aggregate_to_ed(white_data_sf, ad34_shp)

cat("  ✓ Aggregated all data to", nrow(asian_ed), "EDs\n\n")

# ============================================================
# LOAD ELECTION RESULTS
# ============================================================

cat("📥 LOADING ELECTION RESULTS\n")
cat("============================\n\n")

election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, Sliwa_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# ============================================================
# MERGE AND CALCULATE CORRELATIONS
# ============================================================

cat("🔗 MERGING DATA AND CALCULATING CORRELATIONS\n")
cat("=============================================\n\n")

# Merge Asian
asian_merged <- election_results %>%
  left_join(asian_ed, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0)

# Merge Hispanic
hispanic_merged <- election_results %>%
  left_join(hispanic_ed, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0)

# Merge White
white_merged <- election_results %>%
  left_join(white_ed, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0)

# Function to calculate correlations
calc_correlations <- function(data, group_vars, category_name) {
  results <- tibble()

  for (var in group_vars) {
    if (!var %in% names(data)) next
    if (sd(data[[var]], na.rm = TRUE) == 0) next

    n_nonzero <- sum(data[[var]] > 0, na.rm = TRUE)
    if (n_nonzero < 3) next

    total_pop <- sum(data[[var]], na.rm = TRUE)

    # All EDs
    cor_mamdani_all <- cor(data[[var]], data$Mamdani_pct, use = "complete.obs")
    cor_cuomo_all <- cor(data[[var]], data$Cuomo_pct, use = "complete.obs")

    # Non-zero EDs
    nonzero <- data %>% filter(.data[[var]] > 0)
    cor_mamdani_nz <- NA
    cor_cuomo_nz <- NA
    if (nrow(nonzero) >= 3) {
      cor_mamdani_nz <- cor(nonzero[[var]], nonzero$Mamdani_pct, use = "complete.obs")
      cor_cuomo_nz <- cor(nonzero[[var]], nonzero$Cuomo_pct, use = "complete.obs")
    }

    results <- bind_rows(results, tibble(
      group = var,
      category = category_name,
      n_eds_nonzero = n_nonzero,
      total_population = total_pop,
      cor_mamdani_all = cor_mamdani_all,
      cor_mamdani_nonzero = cor_mamdani_nz,
      cor_cuomo_all = cor_cuomo_all,
      cor_cuomo_nonzero = cor_cuomo_nz
    ))
  }

  return(results)
}

# Calculate correlations for each category
asian_groups <- c("chinese", "japanese", "korean", "taiwanese", "filipino", "vietnamese",
                  "thai", "cambodian", "burmese", "hmong",
                  "asian_indian", "bangladeshi", "pakistani", "nepali", "sri_lankan")

hispanic_groups <- c("mexican", "puerto_rican", "cuban", "dominican",
                     "guatemalan", "honduran", "nicaraguan", "salvadoran",
                     "colombian", "ecuadorian", "peruvian", "venezuelan", "bolivian")

white_groups <- c("italian", "irish", "german", "polish", "russian", "greek",
                  "ukrainian", "czech", "norwegian")

cat("  Calculating Asian correlations...\n")
asian_cors <- calc_correlations(asian_merged, asian_groups, "Asian")

cat("  Calculating Hispanic correlations...\n")
hispanic_cors <- calc_correlations(hispanic_merged, hispanic_groups, "Hispanic/Latino")

cat("  Calculating White ethnic correlations...\n")
white_cors <- calc_correlations(white_merged, white_groups, "White Ethnic")

# Combine all
all_cors <- bind_rows(asian_cors, hispanic_cors, white_cors) %>%
  mutate(
    group_clean = str_replace_all(group, "_", " ") %>% str_to_title()
  ) %>%
  arrange(desc(cor_mamdani_nonzero))

cat("  ✓ Calculated correlations for", nrow(all_cors), "ethnic groups\n\n")

# ============================================================
# DISPLAY RESULTS
# ============================================================

cat("📊 RESULTS: TOP SUPPORTERS & OPPONENTS\n")
cat("========================================\n\n")

cat("TOP 15 MAMDANI SUPPORTERS:\n")
cat(sprintf("%-25s %-20s %10s %12s %12s\n",
            "Group", "Category", "Total Pop", "Mamdani r", "Cuomo r"))
cat(rep("-", 80), "\n")

top_supporters <- all_cors %>% head(15)
for (i in 1:nrow(top_supporters)) {
  cat(sprintf("%-25s %-20s %10.0f %12.3f %12.3f\n",
              top_supporters$group_clean[i],
              top_supporters$category[i],
              top_supporters$total_population[i],
              top_supporters$cor_mamdani_nonzero[i],
              top_supporters$cor_cuomo_nonzero[i]))
}

cat("\nTOP 15 MAMDANI OPPONENTS:\n")
cat(sprintf("%-25s %-20s %10s %12s %12s\n",
            "Group", "Category", "Total Pop", "Mamdani r", "Cuomo r"))
cat(rep("-", 80), "\n")

top_opponents <- all_cors %>% arrange(cor_mamdani_nonzero) %>% head(15)
for (i in 1:nrow(top_opponents)) {
  cat(sprintf("%-25s %-20s %10.0f %12.3f %12.3f\n",
              top_opponents$group_clean[i],
              top_opponents$category[i],
              top_opponents$total_population[i],
              top_opponents$cor_mamdani_nonzero[i],
              top_opponents$cor_cuomo_nonzero[i]))
}

cat("\n")

# ============================================================
# KEY FINDINGS BY CATEGORY
# ============================================================

cat("📊 SUMMARY BY ETHNICITY CATEGORY\n")
cat("==================================\n\n")

summary_by_cat <- all_cors %>%
  group_by(category) %>%
  summarize(
    n_groups = n(),
    avg_mamdani_cor = mean(cor_mamdani_nonzero, na.rm = TRUE),
    median_mamdani_cor = median(cor_mamdani_nonzero, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mamdani_cor))

for (i in 1:nrow(summary_by_cat)) {
  cat(sprintf("\n%s:\n", summary_by_cat$category[i]))
  cat(sprintf("  Groups analyzed: %d\n", summary_by_cat$n_groups[i]))
  cat(sprintf("  Total population: %.0f\n", summary_by_cat$total_population[i]))
  cat(sprintf("  Avg Mamdani correlation: %.3f\n", summary_by_cat$avg_mamdani_cor[i]))
  cat(sprintf("  Median Mamdani correlation: %.3f\n", summary_by_cat$median_mamdani_cor[i]))
}

cat("\n")

# ============================================================
# BANGLADESHI SPECIFIC ANALYSIS
# ============================================================

cat("🇧🇩 BANGLADESHI DETAILED ANALYSIS\n")
cat("==================================\n\n")

bangladesh_data <- asian_merged %>%
  select(ED, ED_num, bangladeshi, Mamdani_pct, Cuomo_pct, total_pop) %>%
  arrange(desc(bangladeshi))

cat("TOP 10 EDs BY BANGLADESHI POPULATION:\n")
cat(sprintf("%-8s %12s %12s %12s\n", "ED", "Bangladeshi", "Mamdani %", "Total Pop"))
cat(rep("-", 50), "\n")
for (i in 1:min(10, nrow(bangladesh_data))) {
  cat(sprintf("%-8s %12.0f %12.1f %12.0f\n",
              bangladesh_data$ED[i],
              bangladesh_data$bangladeshi[i],
              bangladesh_data$Mamdani_pct[i],
              bangladesh_data$total_pop[i]))
}

cat("\nTotal Bangladeshi in AD34:", round(sum(bangladesh_data$bangladeshi, na.rm = TRUE)), "\n")
cat("Correlation with Mamdani:",
    round(cor(bangladesh_data$bangladeshi, bangladesh_data$Mamdani_pct, use = "complete.obs"), 3), "\n\n")

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 SAVING RESULTS\n")
cat("==================\n\n")

dir.create("outputs/analysis/corrected", showWarnings = FALSE, recursive = TRUE)

write_csv(all_cors, "outputs/analysis/corrected/all_ethnic_correlations_final.csv")
cat("  ✓ Saved: all_ethnic_correlations_final.csv\n")

write_csv(asian_merged, "outputs/analysis/corrected/asian_by_ed_b02015.csv")
cat("  ✓ Saved: asian_by_ed_b02015.csv\n")

write_csv(hispanic_merged, "outputs/analysis/corrected/hispanic_by_ed_b03001.csv")
cat("  ✓ Saved: hispanic_by_ed_b03001.csv\n")

write_csv(white_merged, "outputs/analysis/corrected/white_by_ed_b04006.csv")
cat("  ✓ Saved: white_by_ed_b04006.csv\n")

cat("\n✅ CORRECTED ETHNIC ANALYSIS COMPLETE!\n")
cat("   Using optimal Census tables for each group\n")
cat("   Data quality significantly improved\n")
