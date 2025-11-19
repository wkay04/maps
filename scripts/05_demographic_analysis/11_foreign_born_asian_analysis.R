# ============================================================
# Foreign-Born Place of Birth Analysis - Asian Countries
# ============================================================
# Using B05006 (Place of Birth) for more accurate ethnic counts

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🌍 FOREIGN-BORN ASIAN ANALYSIS (Place of Birth)\n")
cat("================================================\n\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# ============================================================
# LOAD PLACE OF BIRTH DATA (B05006)
# ============================================================

cat("📥 Loading foreign-born place of birth data...\n")

# B05006 - Place of Birth for the Foreign-Born Population
# This has detailed country-level breakdowns

foreign_born_vars <- c(
  total_pop = "B05006_001",
  total_foreign = "B05006_002",

  # Asia total
  asia_total = "B05006_047",

  # Eastern Asia
  china = "B05006_048",
  hong_kong = "B05006_049",
  taiwan = "B05006_050",
  japan = "B05006_051",
  korea = "B05006_052",
  other_east_asia = "B05006_053",

  # South Central Asia
  south_central_asia_total = "B05006_091",
  afghanistan = "B05006_092",
  bangladesh = "B05006_093",
  india = "B05006_094",
  iran = "B05006_095",
  kazakhstan = "B05006_096",
  nepal = "B05006_097",
  pakistan = "B05006_098",
  sri_lanka = "B05006_099",
  uzbekistan = "B05006_100",
  other_south_central_asia = "B05006_101",

  # Southeast Asia
  southeast_asia_total = "B05006_102",
  burma = "B05006_103",
  cambodia = "B05006_104",
  indonesia = "B05006_105",
  laos = "B05006_106",
  malaysia = "B05006_107",
  philippines = "B05006_108",
  singapore = "B05006_109",
  thailand = "B05006_110",
  vietnam = "B05006_111",
  other_southeast_asia = "B05006_112",

  # Western Asia (Middle East)
  western_asia_total = "B05006_113"
)

cat("  Downloading B05006 (Place of Birth)...\n")
queens_foreign_born <- get_acs(
  geography = "tract",
  variables = foreign_born_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

cat("  ✓ Loaded data for", nrow(queens_foreign_born), "Census tracts\n\n")

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
  st_transform(st_crs(queens_foreign_born))

# Spatial join
queens_foreign_born <- queens_foreign_born %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

ed_foreign <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_foreign_born
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# Get all estimate columns (numeric only)
estimate_cols <- names(ed_foreign)[str_detect(names(ed_foreign), "E$")]
# Filter to only numeric columns
numeric_estimate_cols <- estimate_cols[sapply(ed_foreign[estimate_cols], is.numeric)]

# Aggregate to ED level
ed_foreign_agg <- ed_foreign %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(numeric_estimate_cols),
           ~sum(.x * area_weight, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Clean up column names (remove E suffix)
  rename_with(~str_remove(.x, "E$"), ends_with("E"))

cat("  ✓ Aggregated to", nrow(ed_foreign_agg), "Election Districts\n\n")

# ============================================================
# POPULATION SUMMARY
# ============================================================

cat("📊 FOREIGN-BORN ASIAN POPULATION IN AD34\n")
cat("=========================================\n\n")

# Define groups with display names and regions
country_groups <- tibble(
  variable = c(
    # South Central Asia
    "india", "bangladesh", "pakistan", "nepal", "sri_lanka", "afghanistan",
    "iran", "kazakhstan", "uzbekistan",
    # East Asia
    "china", "hong_kong", "taiwan", "korea", "japan",
    # Southeast Asia
    "philippines", "vietnam", "burma", "cambodia", "laos", "thailand",
    "indonesia", "malaysia", "singapore"
  ),
  display_name = c(
    # South Central Asia
    "India", "Bangladesh", "Pakistan", "Nepal", "Sri Lanka", "Afghanistan",
    "Iran", "Kazakhstan", "Uzbekistan",
    # East Asia
    "China", "Hong Kong", "Taiwan", "Korea (South)", "Japan",
    # Southeast Asia
    "Philippines", "Vietnam", "Burma/Myanmar", "Cambodia", "Laos", "Thailand",
    "Indonesia", "Malaysia", "Singapore"
  ),
  region = c(
    # South Central Asia
    rep("South/Central Asian", 9),
    # East Asia
    rep("East Asian", 5),
    # Southeast Asia
    rep("Southeast Asian", 9)
  )
)

# Calculate population statistics
pop_stats <- tibble()

for (i in 1:nrow(country_groups)) {
  var <- country_groups$variable[i]

  if (var %in% names(ed_foreign_agg)) {
    total_pop <- sum(ed_foreign_agg[[var]], na.rm = TRUE)
    n_eds <- sum(ed_foreign_agg[[var]] > 0, na.rm = TRUE)
    mean_pop <- mean(ed_foreign_agg[[var]], na.rm = TRUE)
    max_pop <- max(ed_foreign_agg[[var]], na.rm = TRUE)

    pop_stats <- bind_rows(pop_stats, tibble(
      variable = var,
      display_name = country_groups$display_name[i],
      region = country_groups$region[i],
      total_population = total_pop,
      n_eds_present = n_eds,
      mean_per_ed = mean_pop,
      max_in_ed = max_pop
    ))
  }
}

# Sort by total population
pop_stats <- pop_stats %>% arrange(desc(total_population))

# Display by region
for (reg in c("South/Central Asian", "East Asian", "Southeast Asian")) {
  cat(toupper(reg), "COUNTRIES:\n")
  cat(sprintf("%-25s %12s %8s %12s %12s\n",
              "Country", "Total Pop", "N EDs", "Mean/ED", "Max/ED"))
  cat(rep("-", 75), "\n")

  region_data <- pop_stats %>% filter(region == reg) %>% arrange(desc(total_population))

  for (i in 1:nrow(region_data)) {
    cat(sprintf("%-25s %12.0f %8d %12.1f %12.0f\n",
                region_data$display_name[i],
                region_data$total_population[i],
                region_data$n_eds_present[i],
                region_data$mean_per_ed[i],
                region_data$max_in_ed[i]))
  }
  cat("\n")
}

# Regional totals
cat("REGIONAL TOTALS:\n")
cat(sprintf("%-25s %12.0f\n", "Total South/Central Asian",
            sum(ed_foreign_agg$south_central_asia_total, na.rm = TRUE)))
cat(sprintf("%-25s %12.0f\n", "Total East Asian",
            sum(ed_foreign_agg$china, na.rm = TRUE) +
            sum(ed_foreign_agg$hong_kong, na.rm = TRUE) +
            sum(ed_foreign_agg$taiwan, na.rm = TRUE) +
            sum(ed_foreign_agg$korea, na.rm = TRUE) +
            sum(ed_foreign_agg$japan, na.rm = TRUE) +
            sum(ed_foreign_agg$other_east_asia, na.rm = TRUE)))
cat(sprintf("%-25s %12.0f\n", "Total Southeast Asian",
            sum(ed_foreign_agg$southeast_asia_total, na.rm = TRUE)))
cat(sprintf("%-25s %12.0f\n", "Total Asia",
            sum(ed_foreign_agg$asia_total, na.rm = TRUE)))
cat("\n")

# ============================================================
# CORRELATIONS WITH ELECTION RESULTS
# ============================================================

cat("📊 CORRELATIONS WITH MAMDANI SUPPORT\n")
cat("=====================================\n\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, Cuomo_pct, Sliwa_pct, total_votes)

demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

# Merge
merged <- election_results %>%
  left_join(ed_foreign_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), total_pop > 0)

# Calculate correlations
correlation_results <- tibble()

for (i in 1:nrow(pop_stats)) {
  var <- pop_stats$variable[i]

  if (var %in% names(merged) && pop_stats$n_eds_present[i] >= 3) {

    # All EDs correlation
    cor_mamdani_all <- cor(merged[[var]], merged$Mamdani_pct, use = "complete.obs")
    cor_cuomo_all <- cor(merged[[var]], merged$Cuomo_pct, use = "complete.obs")

    # Non-zero EDs
    nonzero <- merged %>% filter(.data[[var]] > 0)
    if (nrow(nonzero) >= 3) {
      cor_mamdani_nz <- cor(nonzero[[var]], nonzero$Mamdani_pct, use = "complete.obs")
      cor_cuomo_nz <- cor(nonzero[[var]], nonzero$Cuomo_pct, use = "complete.obs")
    } else {
      cor_mamdani_nz <- NA
      cor_cuomo_nz <- NA
    }

    correlation_results <- bind_rows(correlation_results, tibble(
      variable = var,
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

# Display correlations by region
for (reg in c("South/Central Asian", "East Asian", "Southeast Asian")) {
  cat(toupper(reg), "CORRELATIONS (Non-zero EDs):\n")
  cat(sprintf("%-25s %8s %10s %12s %12s\n",
              "Country", "N EDs", "Total Pop", "Mamdani r", "Cuomo r"))
  cat(rep("-", 75), "\n")

  region_cors <- correlation_results %>%
    filter(region == reg) %>%
    arrange(desc(cor_mamdani_nonzero))

  for (i in 1:nrow(region_cors)) {
    cat(sprintf("%-25s %8d %10.0f %12.3f %12.3f\n",
                region_cors$display_name[i],
                region_cors$n_eds[i],
                region_cors$total_pop[i],
                ifelse(is.na(region_cors$cor_mamdani_nonzero[i]), 0, region_cors$cor_mamdani_nonzero[i]),
                ifelse(is.na(region_cors$cor_cuomo_nonzero[i]), 0, region_cors$cor_cuomo_nonzero[i])))
  }
  cat("\n")
}

# ============================================================
# KEY FINDINGS SUMMARY
# ============================================================

cat("🎯 KEY FINDINGS\n")
cat("================\n\n")

cat("TOP 10 LARGEST FOREIGN-BORN ASIAN GROUPS:\n")
top_10 <- pop_stats %>% arrange(desc(total_population)) %>% head(10)
for (i in 1:nrow(top_10)) {
  cat(sprintf("%2d. %-20s %8.0f people\n",
              i, top_10$display_name[i], top_10$total_population[i]))
}

cat("\n")

cat("STRONGEST MAMDANI SUPPORTERS (r > 0.2):\n")
supporters <- correlation_results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero > 0.2) %>%
  arrange(desc(cor_mamdani_nonzero))

if (nrow(supporters) > 0) {
  for (i in 1:nrow(supporters)) {
    cat(sprintf("  ✓ %-20s r=%6.3f (n=%d EDs, pop=%6.0f)\n",
                supporters$display_name[i],
                supporters$cor_mamdani_nonzero[i],
                supporters$n_eds[i],
                supporters$total_pop[i]))
  }
} else {
  cat("  (None with r > 0.2)\n")
}

cat("\n")

cat("STRONGEST MAMDANI OPPONENTS (r < -0.1):\n")
opponents <- correlation_results %>%
  filter(!is.na(cor_mamdani_nonzero), cor_mamdani_nonzero < -0.1) %>%
  arrange(cor_mamdani_nonzero)

if (nrow(opponents) > 0) {
  for (i in 1:nrow(opponents)) {
    cat(sprintf("  ✗ %-20s r=%6.3f (n=%d EDs, pop=%6.0f)\n",
                opponents$display_name[i],
                opponents$cor_mamdani_nonzero[i],
                opponents$n_eds[i],
                opponents$total_pop[i]))
  }
} else {
  cat("  (None with r < -0.1)\n")
}

cat("\n")

# ============================================================
# SAVE RESULTS
# ============================================================

write_csv(pop_stats, "outputs/analysis/foreign_born_asian_population.csv")
write_csv(correlation_results, "outputs/analysis/foreign_born_asian_correlations.csv")

# Save ED-level data for top groups
ed_detail <- merged %>%
  select(ED, ED_num,
         india, bangladesh, pakistan, nepal, china, korea, philippines, vietnam,
         Mamdani_pct, Cuomo_pct, Sliwa_pct) %>%
  arrange(desc(india + bangladesh + pakistan + nepal))

write_csv(ed_detail, "outputs/analysis/foreign_born_asian_by_ed.csv")

cat("✅ Foreign-born Asian analysis complete!\n")
cat("  ✓ Saved: foreign_born_asian_population.csv\n")
cat("  ✓ Saved: foreign_born_asian_correlations.csv\n")
cat("  ✓ Saved: foreign_born_asian_by_ed.csv\n")
