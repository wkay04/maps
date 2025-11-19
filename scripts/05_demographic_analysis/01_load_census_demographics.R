# ============================================================
# Load Census Demographics for AD34
# ============================================================
# Cross-reference with election results to find demographic
# correlates of Zohran Mamdani support

library(tidyverse)
library(tidycensus)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n📊 Census Demographic Analysis - AD34\n")
cat("======================================\n\n")

# Census API key should already be set in .Renviron
# If not, run: census_api_key("YOUR_KEY_HERE", install = TRUE)

# ============================================================
# DEFINE CENSUS VARIABLES TO PULL
# ============================================================

# ACS 5-year estimates (2022) - most detailed data
census_vars <- c(
  # Total population
  total_pop = "B01003_001",

  # Race/Ethnicity
  white_alone = "B03002_003",
  black_alone = "B03002_004",
  asian_alone = "B03002_006",
  hispanic = "B03002_012",

  # Age
  median_age = "B01002_001",
  under_18 = "B09001_001",
  over_65 = "B09020_001",

  # Income
  median_income = "B19013_001",
  below_poverty = "B17001_002",
  total_poverty_universe = "B17001_001",

  # Education (25+)
  bachelors_plus = "B15003_022",  # Bachelor's
  graduate_degree = "B15003_023", # Master's
  total_edu_universe = "B15003_001",

  # Housing
  renter_occupied = "B25003_003",
  total_occupied = "B25003_001",
  median_rent = "B25064_001",

  # Nativity
  foreign_born = "B05002_013",
  total_nativity = "B05002_001",

  # Language at home
  spanish_home = "B16001_003",
  asian_lang_home = "B16001_005",
  total_language = "B16001_001"
)

cat("📥 Downloading Census data for Queens County...\n")
cat("   Using ACS 5-Year 2022 estimates at block group level\n\n")

# Get data for Queens County at block group level
# Queens County FIPS: 36081
queens_demographics <- get_acs(
  geography = "block group",
  variables = census_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(-moe) %>%  # Remove margin of error for now
  pivot_wider(names_from = variable, values_from = estimate)

cat("  ✓ Downloaded", nrow(queens_demographics), "block groups\n\n")

# Calculate percentages
queens_demographics <- queens_demographics %>%
  mutate(
    pct_white = 100 * white_alone / total_pop,
    pct_black = 100 * black_alone / total_pop,
    pct_asian = 100 * asian_alone / total_pop,
    pct_hispanic = 100 * hispanic / total_pop,
    pct_poverty = 100 * below_poverty / total_poverty_universe,
    pct_bachelors_plus = 100 * (bachelors_plus + graduate_degree) / total_edu_universe,
    pct_renter = 100 * renter_occupied / total_occupied,
    pct_foreign_born = 100 * foreign_born / total_nativity,
    pct_spanish_home = 100 * spanish_home / total_language,
    pct_asian_lang_home = 100 * asian_lang_home / total_language
  )

# ============================================================
# SPATIAL JOIN WITH ELECTION DISTRICTS
# ============================================================

cat("🗺️  Loading Election District boundaries...\n")

shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

# Filter for AD34
ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_demographics))  # Match CRS

cat("  ✓ Loaded", nrow(ad34_shp), "EDs in AD34\n\n")

cat("🔗 Performing spatial join between block groups and EDs...\n")
cat("   (This may take a minute...)\n\n")

# Spatial intersection: find which block groups overlap each ED
# Use st_intersection to get area-weighted demographics
ed_demographics <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_demographics
) %>%
  mutate(
    # Calculate area of intersection
    intersection_area = as.numeric(st_area(geometry))
  ) %>%
  st_drop_geometry()

# Weight demographics by intersection area and aggregate to ED level
ed_demographics_agg <- ed_demographics %>%
  group_by(ED, ED_num) %>%
  summarize(
    # Population-weighted averages for percentages
    total_pop = sum(total_pop * intersection_area) / sum(intersection_area),
    pct_white = sum(pct_white * intersection_area) / sum(intersection_area),
    pct_black = sum(pct_black * intersection_area) / sum(intersection_area),
    pct_asian = sum(pct_asian * intersection_area) / sum(intersection_area),
    pct_hispanic = sum(pct_hispanic * intersection_area) / sum(intersection_area),
    median_age = sum(median_age * intersection_area, na.rm = TRUE) / sum(intersection_area),
    median_income = sum(median_income * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_poverty = sum(pct_poverty * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_bachelors_plus = sum(pct_bachelors_plus * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_renter = sum(pct_renter * intersection_area, na.rm = TRUE) / sum(intersection_area),
    median_rent = sum(median_rent * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_foreign_born = sum(pct_foreign_born * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_spanish_home = sum(pct_spanish_home * intersection_area, na.rm = TRUE) / sum(intersection_area),
    pct_asian_lang_home = sum(pct_asian_lang_home * intersection_area, na.rm = TRUE) / sum(intersection_area),
    .groups = "drop"
  )

cat("  ✓ Aggregated demographics for", nrow(ed_demographics_agg), "EDs\n\n")

# Save
write_csv(ed_demographics_agg, "data/intermediate/ad34_ed_demographics.csv")
cat("✅ Saved: data/intermediate/ad34_ed_demographics.csv\n\n")

# Show summary
cat("📊 DEMOGRAPHIC SUMMARY:\n")
cat(sprintf("  Median Income: $%s\n", format(round(median(ed_demographics_agg$median_income, na.rm=TRUE)), big.mark=",")))
cat(sprintf("  Avg %% Hispanic: %.1f%%\n", mean(ed_demographics_agg$pct_hispanic, na.rm=TRUE)))
cat(sprintf("  Avg %% Asian: %.1f%%\n", mean(ed_demographics_agg$pct_asian, na.rm=TRUE)))
cat(sprintf("  Avg %% White: %.1f%%\n", mean(ed_demographics_agg$pct_white, na.rm=TRUE)))
cat(sprintf("  Avg %% Foreign-Born: %.1f%%\n", mean(ed_demographics_agg$pct_foreign_born, na.rm=TRUE)))
cat(sprintf("  Avg %% Renters: %.1f%%\n", mean(ed_demographics_agg$pct_renter, na.rm=TRUE)))
