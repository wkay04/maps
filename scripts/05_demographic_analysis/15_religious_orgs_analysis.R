# ============================================================
# RELIGIOUS ORGANIZATIONS ANALYSIS
# ============================================================
# Using NaNDA data on religious institutions by census tract
# to analyze relationship with Mamdani vote share

library(tidyverse)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n🕌 RELIGIOUS ORGANIZATIONS ANALYSIS\n")
cat("===================================\n\n")

# ============================================================
# LOAD NANDA DATA
# ============================================================

cat("📥 Loading NaNDA religious organizations data...\n")

nanda_path <- "207966-V1/nanda_relcivsoc_1990-2021_CSV/nanda_relcivsoc_tract20_1990-2021_01P.csv"

# Read data - filter to Queens (FIPS 36081) and recent year (2021)
nanda_raw <- read_csv(nanda_path,
                      col_types = cols(tract_fips20 = col_character()),
                      show_col_types = FALSE)

cat("  Total rows in dataset:", nrow(nanda_raw), "\n")
cat("  Years available:", min(nanda_raw$year), "to", max(nanda_raw$year), "\n")
cat("  Columns:", ncol(nanda_raw), "\n\n")

# Filter to Queens (36081) and year 2021
queens_data <- nanda_raw %>%
  filter(str_starts(tract_fips20, "36081"), year == 2021) %>%
  select(tract_fips20, year, totpop, aland20,
         count_religiousorgs, emps_religiousorgs,
         den_religiousorgs, aden_religiousorgs)

cat("  Filtered to Queens 2021:", nrow(queens_data), "tracts\n")
cat("  Total religious orgs in Queens:", sum(queens_data$count_religiousorgs, na.rm = TRUE), "\n\n")

# ============================================================
# LOAD CENSUS TRACT GEOMETRIES
# ============================================================

cat("📍 Loading Census tract geometries...\n")

# Get tract geometries from tidycensus (2020 tracts)
library(tidycensus)
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

queens_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",  # Total population
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  select(GEOID, geometry)

cat("  Loaded", nrow(queens_tracts), "tract geometries\n\n")

# Merge with NaNDA data
queens_geo <- queens_tracts %>%
  left_join(queens_data, by = c("GEOID" = "tract_fips20"))

# ============================================================
# LOAD AD34 BOUNDARY AND FILTER
# ============================================================

cat("📍 Loading AD34 boundary...\n")

library(tigris)
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>%
  filter(NAMELSAD == "Assembly District 34") %>%
  st_transform(3857)

# Filter tracts to AD34 using centroid method
centroids <- suppressWarnings(st_centroid(queens_geo))
queens_geo$in_ad34 <- st_within(centroids, ad34, sparse = FALSE)[,1]
ad34_tracts <- queens_geo %>% filter(in_ad34)

cat("  AD34 contains", nrow(ad34_tracts), "census tracts\n")
cat("  Religious orgs in AD34:", sum(ad34_tracts$count_religiousorgs, na.rm = TRUE), "\n\n")

# ============================================================
# AGGREGATE TO ELECTION DISTRICTS
# ============================================================

cat("📊 Aggregating to Election District level...\n")

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_ed_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(3857)

# Spatial intersection with area weighting
ad34_tracts_data <- ad34_tracts %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

intersection <- st_intersection(
  ad34_ed_shp %>% select(ED, ED_num, geometry),
  ad34_tracts_data
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# Aggregate to ED level
religious_by_ed <- intersection %>%
  group_by(ED, ED_num) %>%
  summarize(
    count_religious = sum(count_religiousorgs * area_weight, na.rm = TRUE),
    emps_religious = sum(emps_religiousorgs * area_weight, na.rm = TRUE),
    den_religious = weighted.mean(den_religiousorgs, area_weight, na.rm = TRUE),
    aden_religious = weighted.mean(aden_religiousorgs, area_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(count_religious = round(count_religious, 1))

cat("  Aggregated to", nrow(religious_by_ed), "Election Districts\n\n")

# ============================================================
# MERGE WITH ELECTION RESULTS
# ============================================================

cat("🗳️  Merging with election results...\n")

election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, Sliwa_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

analysis_data <- religious_by_ed %>%
  left_join(election_results, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), !is.na(count_religious))

cat("  Merged data for", nrow(analysis_data), "EDs\n\n")

# ============================================================
# CALCULATE CORRELATIONS
# ============================================================

cat("📊 CORRELATION ANALYSIS\n")
cat("========================\n\n")

cor_count <- cor(analysis_data$count_religious,
                 analysis_data$Mamdani_pct,
                 use = "complete.obs")

cor_density <- cor(analysis_data$den_religious,
                   analysis_data$Mamdani_pct,
                   use = "complete.obs")

cor_area_density <- cor(analysis_data$aden_religious,
                        analysis_data$Mamdani_pct,
                        use = "complete.obs")

cat("Religious Org Count vs Mamdani %:          r =", round(cor_count, 3), "\n")
cat("Religious Org Density (per capita) vs Mamdani %: r =", round(cor_density, 3), "\n")
cat("Religious Org Density (per sq mi) vs Mamdani %:  r =", round(cor_area_density, 3), "\n\n")

# ============================================================
# TOP/BOTTOM EDs BY RELIGIOUS ORG DENSITY
# ============================================================

cat("🕌 EDs WITH MOST RELIGIOUS ORGANIZATIONS:\n")
cat(sprintf("%-8s %12s %12s %12s\n", "ED", "Count", "Density", "Mamdani %"))
cat(rep("-", 50), "\n")

top_orgs <- analysis_data %>%
  arrange(desc(count_religious)) %>%
  head(10)

for (i in 1:nrow(top_orgs)) {
  cat(sprintf("%-8s %12.1f %12.3f %12.1f\n",
              top_orgs$ED[i],
              top_orgs$count_religious[i],
              top_orgs$den_religious[i],
              top_orgs$Mamdani_pct[i]))
}

cat("\n🕌 EDs WITH HIGHEST RELIGIOUS ORG DENSITY (per capita):\n")
cat(sprintf("%-8s %12s %12s %12s\n", "ED", "Count", "Density", "Mamdani %"))
cat(rep("-", 50), "\n")

top_density <- analysis_data %>%
  arrange(desc(den_religious)) %>%
  head(10)

for (i in 1:nrow(top_density)) {
  cat(sprintf("%-8s %12.1f %12.3f %12.1f\n",
              top_density$ED[i],
              top_density$count_religious[i],
              top_density$den_religious[i],
              top_density$Mamdani_pct[i]))
}

cat("\n")

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 SAVING RESULTS\n")
cat("==================\n\n")

dir.create("outputs/analysis/corrected", showWarnings = FALSE, recursive = TRUE)

write_csv(analysis_data, "outputs/analysis/corrected/religious_orgs_by_ed.csv")
cat("  ✓ Saved: religious_orgs_by_ed.csv\n")

# Create summary stats
summary_stats <- tibble(
  metric = c("Correlation: Count vs Mamdani",
             "Correlation: Density vs Mamdani",
             "Total Religious Orgs in AD34",
             "Mean per ED",
             "Median per ED"),
  value = c(cor_count,
            cor_density,
            sum(analysis_data$count_religious, na.rm = TRUE),
            mean(analysis_data$count_religious, na.rm = TRUE),
            median(analysis_data$count_religious, na.rm = TRUE))
)

write_csv(summary_stats, "outputs/analysis/corrected/religious_orgs_summary.csv")
cat("  ✓ Saved: religious_orgs_summary.csv\n")

cat("\n✅ RELIGIOUS ORGANIZATIONS ANALYSIS COMPLETE!\n")
cat("   Note: Data shows aggregate religious organizations\n")
cat("   Does not distinguish between mosques, churches, synagogues, temples\n")
