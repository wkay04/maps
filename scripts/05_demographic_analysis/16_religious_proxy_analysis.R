# ============================================================
# RELIGIOUS AFFILIATION PROXY ANALYSIS
# ============================================================
# ⚠️  MAJOR LIMITATIONS - TAKE WITH A GRAIN OF SALT ⚠️
#
# This uses Census proxy variables to ESTIMATE religious affiliation
# because the Census does NOT collect religion data.
#
# PROXIES ARE IMPERFECT:
# - Bangladesh/Pakistan birth → mostly Muslim (but not 100%)
# - India birth → Hindu, Muslim, Sikh, Christian, Jain (mixed!)
# - Arabic language → Muslim or Christian
# - Hebrew/Yiddish → Jewish (but misses Reform Jews who don't speak these)
# - These miss US-born descendants who don't speak heritage languages
# - Country of birth ≠ religious affiliation
#
# INTERPRET RESULTS CAUTIOUSLY - THESE ARE ROUGH ESTIMATES ONLY
# ============================================================

library(tidyverse)
library(tidycensus)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n🕌 RELIGIOUS AFFILIATION PROXY ANALYSIS\n")
cat("========================================\n")
cat("⚠️  WARNING: IMPERFECT PROXIES - USE CAUTION ⚠️\n\n")

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# ============================================================
# DEFINE PROXY VARIABLES
# ============================================================

cat("📋 Defining Census proxy variables...\n\n")

# MUSLIM PROXY: Bangladesh + Pakistan birth + Arabic language
muslim_proxy_vars <- c(
  bangladesh_born = "B05006_045",  # Born in Bangladesh
  pakistan_born = "B05006_046",    # Born in Pakistan
  arabic_spoken = "C16001_008"     # Speak Arabic at home
)

# HINDU PROXY: India birth (MIXED with Muslims, Sikhs, Christians!)
hindu_proxy_vars <- c(
  india_born = "B05006_044"        # Born in India (MIXED RELIGIONS!)
)

# JEWISH PROXY: Israel birth + Hebrew + Yiddish
jewish_proxy_vars <- c(
  israel_born = "B05006_077",      # Born in Israel
  hebrew_spoken = "C16001_011",    # Speak Hebrew
  yiddish_spoken = "C16001_026"    # Speak Yiddish
)

# EAST ASIAN (Buddhist/Taoist/Nonreligious/Christian - MIXED)
eastasian_proxy_vars <- c(
  china_born = "B05006_039",       # Born in China
  korea_born = "B05006_040",       # Born in Korea
  japan_born = "B05006_041"        # Born in Japan
)

all_proxy_vars <- c(muslim_proxy_vars, hindu_proxy_vars,
                    jewish_proxy_vars, eastasian_proxy_vars)

cat("  Muslim proxies: Bangladesh birth, Pakistan birth, Arabic language\n")
cat("  Hindu proxies: India birth (⚠️  INCLUDES Muslims, Sikhs, Christians!)\n")
cat("  Jewish proxies: Israel birth, Hebrew, Yiddish\n")
cat("  East Asian proxies: China, Korea, Japan birth (⚠️  MIXED religions!)\n\n")

# ============================================================
# DOWNLOAD CENSUS DATA
# ============================================================

cat("📥 Downloading Census proxy data...\n")

proxy_data <- get_acs(
  geography = "tract",
  variables = all_proxy_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
) %>%
  st_transform(3857)

cat("  Downloaded data for", nrow(proxy_data), "tracts\n\n")

# ============================================================
# LOAD AD34 BOUNDARY AND FILTER
# ============================================================

cat("📍 Filtering to AD34...\n")

library(tigris)
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>%
  filter(NAMELSAD == "Assembly District 34") %>%
  st_transform(3857)

centroids <- suppressWarnings(st_centroid(proxy_data))
proxy_data$in_ad34 <- st_within(centroids, ad34, sparse = FALSE)[,1]
ad34_proxy <- proxy_data %>% filter(in_ad34)

cat("  Filtered to", nrow(ad34_proxy), "tracts in AD34\n\n")

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
ad34_proxy_area <- ad34_proxy %>%
  mutate(tract_area = as.numeric(st_area(geometry)))

intersection <- st_intersection(
  ad34_ed_shp %>% select(ED, ED_num, geometry),
  ad34_proxy_area
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    area_weight = intersection_area / tract_area
  ) %>%
  st_drop_geometry()

# Get estimate columns
estimate_cols <- names(intersection)[str_detect(names(intersection), "E$")]
estimate_cols <- estimate_cols[sapply(intersection[estimate_cols], is.numeric)]

# Aggregate to ED
proxy_by_ed <- intersection %>%
  group_by(ED, ED_num) %>%
  summarize(
    across(all_of(estimate_cols), ~sum(.x * area_weight, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename_with(~str_remove(.x, "E$"), ends_with("E"))

# Create composite proxy estimates
proxy_by_ed <- proxy_by_ed %>%
  mutate(
    # MUSLIM PROXY (sum of Bangladesh + Pakistan birth + Arabic speakers)
    # ⚠️  CAVEAT: Counts people multiple times if they're e.g. Pakistani AND speak Arabic
    muslim_proxy = bangladesh_born + pakistan_born + arabic_spoken,

    # HINDU PROXY (India birth - but INCLUDES Muslims, Sikhs, Christians!)
    hindu_proxy = india_born,

    # JEWISH PROXY (Israel birth + Hebrew + Yiddish speakers)
    jewish_proxy = israel_born + hebrew_spoken + yiddish_spoken,

    # EAST ASIAN PROXY (China + Korea + Japan - MIXED religions!)
    eastasian_proxy = china_born + korea_born + japan_born
  )

cat("  Aggregated to", nrow(proxy_by_ed), "EDs\n\n")

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

analysis_data <- proxy_by_ed %>%
  left_join(election_results, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct), !is.na(total_pop))

cat("  Merged data for", nrow(analysis_data), "EDs\n\n")

# ============================================================
# CALCULATE CORRELATIONS
# ============================================================

cat("📊 CORRELATION ANALYSIS (⚠️  PROXIES ONLY!)\n")
cat("============================================\n\n")

proxies <- c("muslim_proxy", "hindu_proxy", "jewish_proxy", "eastasian_proxy")
proxy_labels <- c("Muslim proxy", "Hindu proxy", "Jewish proxy", "East Asian proxy")

correlation_results <- tibble()

for (i in seq_along(proxies)) {
  proxy_var <- proxies[i]
  label <- proxy_labels[i]

  # Calculate per capita percentage
  analysis_data <- analysis_data %>%
    mutate(!!paste0(proxy_var, "_pct") := 100 * .data[[proxy_var]] / total_pop)

  pct_var <- paste0(proxy_var, "_pct")

  # Correlation with Mamdani
  cor_mamdani <- cor(analysis_data[[proxy_var]],
                     analysis_data$Mamdani_pct,
                     use = "complete.obs")

  cor_mamdani_pct <- cor(analysis_data[[pct_var]],
                         analysis_data$Mamdani_pct,
                         use = "complete.obs")

  # Non-zero EDs only
  nonzero <- analysis_data %>% filter(.data[[proxy_var]] > 10)
  cor_mamdani_nz <- NA
  if (nrow(nonzero) >= 3) {
    cor_mamdani_nz <- cor(nonzero[[proxy_var]],
                          nonzero$Mamdani_pct,
                          use = "complete.obs")
  }

  total_count <- sum(analysis_data[[proxy_var]], na.rm = TRUE)

  cat(sprintf("%-20s r = %6.3f  |  r_pct = %6.3f  |  r_nonzero = %6.3f  |  Total: %.0f\n",
              label, cor_mamdani, cor_mamdani_pct, cor_mamdani_nz, total_count))

  correlation_results <- bind_rows(correlation_results, tibble(
    proxy = label,
    correlation_count = cor_mamdani,
    correlation_pct = cor_mamdani_pct,
    correlation_nonzero = cor_mamdani_nz,
    total_count = total_count,
    n_eds_nonzero = sum(analysis_data[[proxy_var]] > 10)
  ))
}

cat("\n")

# ============================================================
# COMPONENT BREAKDOWN
# ============================================================

cat("📋 COMPONENT BREAKDOWN\n")
cat("======================\n\n")

cat("MUSLIM PROXY COMPONENTS:\n")
cat(sprintf("  Bangladesh-born:     %6.0f  (r = %.3f)\n",
            sum(analysis_data$bangladesh_born),
            cor(analysis_data$bangladesh_born, analysis_data$Mamdani_pct, use = "complete.obs")))
cat(sprintf("  Pakistan-born:       %6.0f  (r = %.3f)\n",
            sum(analysis_data$pakistan_born),
            cor(analysis_data$pakistan_born, analysis_data$Mamdani_pct, use = "complete.obs")))
cat(sprintf("  Arabic speakers:     %6.0f  (r = %.3f)\n",
            sum(analysis_data$arabic_spoken),
            cor(analysis_data$arabic_spoken, analysis_data$Mamdani_pct, use = "complete.obs")))

cat("\nHINDU PROXY COMPONENTS:\n")
cat(sprintf("  India-born:          %6.0f  (r = %.3f)\n",
            sum(analysis_data$india_born),
            cor(analysis_data$india_born, analysis_data$Mamdani_pct, use = "complete.obs")))
cat("  ⚠️  India includes Hindus, Muslims, Sikhs, Christians, Jains!\n")

cat("\nJEWISH PROXY COMPONENTS:\n")
cat(sprintf("  Israel-born:         %6.0f  (r = %.3f)\n",
            sum(analysis_data$israel_born),
            cor(analysis_data$israel_born, analysis_data$Mamdani_pct, use = "complete.obs")))
cat(sprintf("  Hebrew speakers:     %6.0f  (r = %.3f)\n",
            sum(analysis_data$hebrew_spoken),
            cor(analysis_data$hebrew_spoken, analysis_data$Mamdani_pct, use = "complete.obs")))
cat(sprintf("  Yiddish speakers:    %6.0f  (r = %.3f)\n",
            sum(analysis_data$yiddish_spoken),
            cor(analysis_data$yiddish_spoken, analysis_data$Mamdani_pct, use = "complete.obs")))

cat("\n")

# ============================================================
# TOP EDs BY PROXY
# ============================================================

cat("🗳️  TOP EDs BY PROXY AFFILIATION\n")
cat("=================================\n\n")

for (i in seq_along(proxies)) {
  proxy_var <- proxies[i]
  label <- proxy_labels[i]
  pct_var <- paste0(proxy_var, "_pct")

  cat(sprintf("%-20s\n", toupper(label)))
  cat(sprintf("%-8s %12s %10s %12s\n", "ED", "Count", "% of Pop", "Mamdani %"))
  cat(rep("-", 50), "\n")

  top_eds <- analysis_data %>%
    arrange(desc(.data[[proxy_var]])) %>%
    head(5)

  for (j in 1:nrow(top_eds)) {
    cat(sprintf("%-8s %12.0f %10.1f %12.1f\n",
                top_eds$ED[j],
                top_eds[[proxy_var]][j],
                top_eds[[pct_var]][j],
                top_eds$Mamdani_pct[j]))
  }
  cat("\n")
}

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 SAVING RESULTS\n")
cat("==================\n\n")

dir.create("outputs/analysis/corrected", showWarnings = FALSE, recursive = TRUE)

write_csv(analysis_data, "outputs/analysis/corrected/religious_proxy_by_ed.csv")
cat("  ✓ Saved: religious_proxy_by_ed.csv\n")

write_csv(correlation_results, "outputs/analysis/corrected/religious_proxy_correlations.csv")
cat("  ✓ Saved: religious_proxy_correlations.csv\n")

cat("\n")
cat("⚠️  ============================================= ⚠️\n")
cat("⚠️  IMPORTANT CAVEATS - READ CAREFULLY          ⚠️\n")
cat("⚠️  ============================================= ⚠️\n\n")
cat("These are PROXY ESTIMATES based on country of birth\n")
cat("and language, NOT actual religious affiliation data.\n\n")
cat("LIMITATIONS:\n")
cat("  • India-born includes Hindus, Muslims, Sikhs, Christians, Jains\n")
cat("  • Bangladesh/Pakistan-born are mostly but not all Muslim\n")
cat("  • Arabic speakers include both Muslims and Christians\n")
cat("  • Language proxies miss US-born descendants\n")
cat("  • Jewish proxy misses Reform/secular Jews who don't speak Hebrew/Yiddish\n")
cat("  • Muslim proxy may double-count (e.g., Pakistani who speaks Arabic)\n")
cat("  • East Asian proxy includes Buddhist, Taoist, Christian, nonreligious\n\n")
cat("USE THESE RESULTS WITH EXTREME CAUTION.\n")
cat("They are suggestive at best, not definitive.\n\n")

cat("✅ RELIGIOUS PROXY ANALYSIS COMPLETE\n")
