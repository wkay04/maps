# ============================================================
# Comprehensive Asian Subgroup Breakdown with Multiple Tables
# ============================================================
# Deep dive using multiple Census tables to capture all Asian groups

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🌏 COMPREHENSIVE ASIAN SUBGROUP ANALYSIS\n")
cat("=========================================\n\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# ============================================================
# EXPLORE AVAILABLE VARIABLES
# ============================================================

cat("📋 Checking available Asian variables in ACS...\n\n")

# Load all ACS variables to see what's available
# acs_vars <- load_variables(2022, "acs5", cache = TRUE)
# asian_vars <- acs_vars %>% filter(str_detect(label, regex("asian", ignore_case = TRUE)))

# ============================================================
# LOAD MULTIPLE TABLES FOR COMPREHENSIVE COVERAGE
# ============================================================

cat("📥 Loading Asian ethnicity data from multiple sources...\n")

# TABLE 1: B02015 - Asian Alone by Selected Groups (most detailed)
asian_detailed_vars <- c(
  total_asian = "B02015_001",
  # South Asian
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  bhutanese = "B02015_020",
  burmese = "B02015_004",
  cambodian = "B02015_005",
  pakistani = "B02015_006",
  sri_lankan = "B02015_007",
  nepali = "B02015_021",
  # East Asian
  chinese_except_taiwanese = "B02015_009",
  taiwanese = "B02015_008",
  japanese = "B02015_013",
  korean = "B02015_014",
  # Southeast Asian
  filipino = "B02015_010",
  hmong = "B02015_012",
  indonesian = "B02015_011",
  laotian = "B02015_015",
  malaysian = "B02015_016",
  thai = "B02015_016",
  vietnamese = "B02015_018",
  # Other
  other_asian = "B02015_019"
)

cat("  Downloading B02015 (Asian Alone by Selected Groups)...\n")
queens_asian_b02015 <- get_acs(
  geography = "tract",
  variables = asian_detailed_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

# TABLE 2: C02003 - Detailed Asian Alone (alternative table)
cat("  Downloading C02003 (Detailed Asian)...\n")
asian_c02003_vars <- c(
  total_asian_c = "C02003_001",
  asian_indian_c = "C02003_002",
  bangladeshi_c = "C02003_003",
  cambodian_c = "C02003_004",
  chinese_c = "C02003_005",
  filipino_c = "C02003_006",
  hmong_c = "C02003_007",
  indonesian_c = "C02003_008",
  japanese_c = "C02003_009",
  korean_c = "C02003_010",
  laotian_c = "C02003_011",
  malaysian_c = "C02003_012",
  pakistani_c = "C02003_013",
  sri_lankan_c = "C02003_014",
  taiwanese_c = "C02003_015",
  thai_c = "C02003_016",
  vietnamese_c = "C02003_017",
  other_asian_c = "C02003_018"
)

queens_asian_c02003 <- get_acs(
  geography = "tract",
  variables = asian_c02003_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = FALSE,
  output = "wide"
)

# Combine the datasets
queens_asian_combined <- queens_asian_b02015 %>%
  left_join(
    queens_asian_c02003 %>% select(-ends_with("M")),
    by = "GEOID"
  )

cat("  ✓ Loaded data for", nrow(queens_asian_combined), "Census tracts\n\n")

# ============================================================
# SPATIAL JOIN TO ELECTION DISTRICTS
# ============================================================

cat("📍 Performing spatial join to Election Districts...\n")

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_asian_combined))

# Spatial join
queens_asian_combined <- queens_asian_combined %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

ed_asian <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_asian_combined
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# List all Asian ethnic variables (using estimate columns, removing MOE)
asian_ethnic_vars <- c(
  # From B02015
  "asian_indian", "bangladeshi", "bhutanese", "burmese", "cambodian",
  "pakistani", "sri_lankan", "nepali",
  "chinese_except_taiwanese", "taiwanese", "japanese", "korean",
  "filipino", "hmong", "indonesian", "laotian", "malaysian", "thai", "vietnamese",
  # From C02003 (alternative measures)
  "asian_indian_c", "bangladeshi_c", "cambodian_c", "chinese_c",
  "filipino_c", "hmong_c", "indonesian_c", "japanese_c", "korean_c",
  "laotian_c", "malaysian_c", "pakistani_c", "sri_lankan_c",
  "taiwanese_c", "thai_c", "vietnamese_c"
)

# Get only estimate columns
estimate_cols <- names(ed_asian)[str_detect(names(ed_asian), "E$")]
asian_estimate_cols <- estimate_cols[estimate_cols %in% paste0(asian_ethnic_vars, "E")]

# Aggregate to ED level
ed_asian_agg <- ed_asian %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(asian_estimate_cols),
           ~sum(.x * area_weight, na.rm = TRUE)),
    total_asian = sum(total_asianE * area_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up column names (remove E suffix)
  rename_with(~str_remove(.x, "E$"), ends_with("E"))

cat("  ✓ Aggregated to", nrow(ed_asian_agg), "Election Districts\n\n")

# ============================================================
# POPULATION SUMMARY
# ============================================================

cat("📊 ASIAN POPULATION BREAKDOWN IN AD34\n")
cat("======================================\n\n")

# Create summary comparing both data sources
groups_to_compare <- tibble(
  group = c("asian_indian", "bangladeshi", "pakistani", "sri_lankan", "nepali",
            "chinese_except_taiwanese", "chinese_c", "taiwanese", "japanese", "korean",
            "filipino", "vietnamese", "thai", "cambodian", "burmese",
            "hmong", "indonesian", "laotian", "malaysian"),
  display_name = c("Asian Indian", "Bangladeshi (B02015)", "Pakistani (B02015)",
                   "Sri Lankan (B02015)", "Nepali",
                   "Chinese (B02015, excl TW)", "Chinese (C02003)", "Taiwanese",
                   "Japanese", "Korean",
                   "Filipino", "Vietnamese", "Thai", "Cambodian", "Burmese",
                   "Hmong", "Indonesian", "Laotian", "Malaysian"),
  region = c("South Asian", "South Asian", "South Asian", "South Asian", "South Asian",
             "East Asian", "East Asian", "East Asian", "East Asian", "East Asian",
             "Southeast Asian", "Southeast Asian", "Southeast Asian",
             "Southeast Asian", "Southeast Asian",
             "Southeast Asian", "Southeast Asian", "Southeast Asian", "Southeast Asian")
)

# Calculate population statistics
pop_stats <- tibble()

for (i in 1:nrow(groups_to_compare)) {
  grp <- groups_to_compare$group[i]

  if (grp %in% names(ed_asian_agg)) {
    total_pop <- sum(ed_asian_agg[[grp]], na.rm = TRUE)
    n_eds <- sum(ed_asian_agg[[grp]] > 0, na.rm = TRUE)
    mean_pop <- mean(ed_asian_agg[[grp]], na.rm = TRUE)
    max_pop <- max(ed_asian_agg[[grp]], na.rm = TRUE)

    pop_stats <- bind_rows(pop_stats, tibble(
      group = grp,
      display_name = groups_to_compare$display_name[i],
      region = groups_to_compare$region[i],
      total_population = total_pop,
      n_eds_present = n_eds,
      mean_per_ed = mean_pop,
      max_in_ed = max_pop
    ))
  }
}

# Sort by total population
pop_stats <- pop_stats %>% arrange(desc(total_population))

cat("SOUTH ASIAN GROUPS:\n")
cat(sprintf("%-35s %10s %8s %10s %10s\n",
            "Group", "Total Pop", "N EDs", "Mean/ED", "Max/ED"))
cat(rep("-", 75), "\n")

south_asian <- pop_stats %>% filter(region == "South Asian")
for (i in 1:nrow(south_asian)) {
  cat(sprintf("%-35s %10.0f %8d %10.1f %10.0f\n",
              south_asian$display_name[i],
              south_asian$total_population[i],
              south_asian$n_eds_present[i],
              south_asian$mean_per_ed[i],
              south_asian$max_in_ed[i]))
}

cat("\nEAST ASIAN GROUPS:\n")
cat(sprintf("%-35s %10s %8s %10s %10s\n",
            "Group", "Total Pop", "N EDs", "Mean/ED", "Max/ED"))
cat(rep("-", 75), "\n")

east_asian <- pop_stats %>% filter(region == "East Asian")
for (i in 1:nrow(east_asian)) {
  cat(sprintf("%-35s %10.0f %8d %10.1f %10.0f\n",
              east_asian$display_name[i],
              east_asian$total_population[i],
              east_asian$n_eds_present[i],
              east_asian$mean_per_ed[i],
              east_asian$max_in_ed[i]))
}

cat("\nSOUTHEAST ASIAN GROUPS:\n")
cat(sprintf("%-35s %10s %8s %10s %10s\n",
            "Group", "Total Pop", "N EDs", "Mean/ED", "Max/ED"))
cat(rep("-", 75), "\n")

southeast_asian <- pop_stats %>% filter(region == "Southeast Asian")
for (i in 1:nrow(southeast_asian)) {
  cat(sprintf("%-35s %10.0f %8d %10.1f %10.0f\n",
              southeast_asian$display_name[i],
              southeast_asian$total_population[i],
              southeast_asian$n_eds_present[i],
              southeast_asian$mean_per_ed[i],
              southeast_asian$max_in_ed[i]))
}

cat("\n")
cat("TOTAL ASIAN:", round(sum(ed_asian_agg$total_asian)), "\n\n")

# ============================================================
# CORRELATIONS WITH ELECTION RESULTS
# ============================================================

cat("📊 CORRELATIONS WITH MAMDANI SUPPORT\n")
cat("=====================================\n\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Merge
merged <- election_results %>%
  left_join(ed_asian_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0)

# Calculate correlations for each group
correlation_results <- tibble()

for (i in 1:nrow(pop_stats)) {
  grp <- pop_stats$group[i]

  if (grp %in% names(merged) && pop_stats$n_eds_present[i] >= 3) {

    # All EDs correlation
    cor_mamdani_all <- cor(merged[[grp]], merged$Mamdani_pct, use = "complete.obs")
    cor_cuomo_all <- cor(merged[[grp]], merged$Cuomo_pct, use = "complete.obs")

    # Non-zero EDs
    nonzero <- merged %>% filter(.data[[grp]] > 0)
    if (nrow(nonzero) >= 3) {
      cor_mamdani_nz <- cor(nonzero[[grp]], nonzero$Mamdani_pct, use = "complete.obs")
      cor_cuomo_nz <- cor(nonzero[[grp]], nonzero$Cuomo_pct, use = "complete.obs")
    } else {
      cor_mamdani_nz <- NA
      cor_cuomo_nz <- NA
    }

    correlation_results <- bind_rows(correlation_results, tibble(
      group = grp,
      display_name = pop_stats$display_name[i],
      region = pop_stats$region[i],
      n_eds = pop_stats$n_eds_present[i],
      total_pop = pop_stats$total_population[i],
      cor_mamdani_all = cor_mamdani_all,
      cor_mamdani_nonzero = cor_mamdani_nz,
      cor_cuomo_all = cor_cuomo_all,
      cor_cuomo_nonzero = cor_cuomo_nz
    ))
  }
}

# Display by region
cat("SOUTH ASIAN CORRELATIONS (Non-zero EDs):\n")
cat(sprintf("%-35s %8s %10s %10s %10s\n",
            "Group", "N EDs", "Total Pop", "Mamdani r", "Cuomo r"))
cat(rep("-", 75), "\n")

sa_cors <- correlation_results %>%
  filter(region == "South Asian") %>%
  arrange(desc(cor_mamdani_nonzero))

for (i in 1:nrow(sa_cors)) {
  cat(sprintf("%-35s %8d %10.0f %10.3f %10.3f\n",
              sa_cors$display_name[i],
              sa_cors$n_eds[i],
              sa_cors$total_pop[i],
              ifelse(is.na(sa_cors$cor_mamdani_nonzero[i]), 0, sa_cors$cor_mamdani_nonzero[i]),
              ifelse(is.na(sa_cors$cor_cuomo_nonzero[i]), 0, sa_cors$cor_cuomo_nonzero[i])))
}

cat("\nEAST ASIAN CORRELATIONS (Non-zero EDs):\n")
cat(sprintf("%-35s %8s %10s %10s %10s\n",
            "Group", "N EDs", "Total Pop", "Mamdani r", "Cuomo r"))
cat(rep("-", 75), "\n")

ea_cors <- correlation_results %>%
  filter(region == "East Asian") %>%
  arrange(desc(cor_mamdani_nonzero))

for (i in 1:nrow(ea_cors)) {
  cat(sprintf("%-35s %8d %10.0f %10.3f %10.3f\n",
              ea_cors$display_name[i],
              ea_cors$n_eds[i],
              ea_cors$total_pop[i],
              ifelse(is.na(ea_cors$cor_mamdani_nonzero[i]), 0, ea_cors$cor_mamdani_nonzero[i]),
              ifelse(is.na(ea_cors$cor_cuomo_nonzero[i]), 0, ea_cors$cor_cuomo_nonzero[i])))
}

cat("\nSOUTHEAST ASIAN CORRELATIONS (Non-zero EDs):\n")
cat(sprintf("%-35s %8s %10s %10s %10s\n",
            "Group", "N EDs", "Total Pop", "Mamdani r", "Cuomo r"))
cat(rep("-", 75), "\n")

sea_cors <- correlation_results %>%
  filter(region == "Southeast Asian") %>%
  arrange(desc(cor_mamdani_nonzero))

for (i in 1:nrow(sea_cors)) {
  cat(sprintf("%-35s %8d %10.0f %10.3f %10.3f\n",
              sea_cors$display_name[i],
              sea_cors$n_eds[i],
              sea_cors$total_pop[i],
              ifelse(is.na(sea_cors$cor_mamdani_nonzero[i]), 0, sea_cors$cor_mamdani_nonzero[i]),
              ifelse(is.na(sea_cors$cor_cuomo_nonzero[i]), 0, sea_cors$cor_cuomo_nonzero[i])))
}

cat("\n")

# Save results
write_csv(pop_stats, "outputs/analysis/asian_population_detailed_breakdown.csv")
write_csv(correlation_results, "outputs/analysis/asian_correlations_comprehensive.csv")

cat("✅ Comprehensive Asian breakdown complete!\n")
cat("  ✓ Saved: asian_population_detailed_breakdown.csv\n")
cat("  ✓ Saved: asian_correlations_comprehensive.csv\n")
