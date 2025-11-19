# Debug script to figure out why Asian ethnicity data is all zeros

library(tidyverse)
library(tidycensus)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n🔍 DEBUGGING ASIAN ETHNICITY DATA\n")
cat("==================================\n\n")

# Load Asian data
asian_vars <- c(
  total_asian = "B02015_001",
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  chinese = "B02015_009"
)

queens_asian <- get_acs(
  geography = "block group",
  variables = asian_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

cat("✓ Downloaded", nrow(queens_asian), "block groups\n")
cat("  CRS:", st_crs(queens_asian)$input, "\n\n")

# Check data summary
cat("📊 Data Summary (first few non-zero block groups):\n")
nonzero <- queens_asian %>%
  filter(total_asian > 0) %>%
  arrange(desc(total_asian)) %>%
  head(5) %>%
  st_drop_geometry()

print(nonzero)

cat("\n📍 Overall statistics:\n")
cat(sprintf("  Total Asian range: %.0f to %.0f\n",
            min(queens_asian$total_asian, na.rm=TRUE),
            max(queens_asian$total_asian, na.rm=TRUE)))
cat(sprintf("  Bangladeshi range: %.0f to %.0f\n",
            min(queens_asian$bangladeshi, na.rm=TRUE),
            max(queens_asian$bangladeshi, na.rm=TRUE)))
cat(sprintf("  Block groups with Asian pop > 0: %d (%.1f%%)\n\n",
            sum(queens_asian$total_asian > 0, na.rm=TRUE),
            100 * sum(queens_asian$total_asian > 0, na.rm=TRUE) / nrow(queens_asian)))

# Load ED shapefile
cat("🗺️  Loading Election Districts...\n")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(ED = Election_D) %>%
  st_transform(st_crs(queens_asian))

cat("✓ Loaded", nrow(ad34_shp), "EDs\n")
cat("  CRS:", st_crs(ad34_shp)$input, "\n\n")

# Test intersection with one ED
cat("🔬 Testing intersection with ED 34001...\n")
test_ed <- ad34_shp %>% filter(Election_D == "34001")

# Find block groups that intersect
test_intersect <- st_intersection(
  queens_asian %>% mutate(bg_id = row_number()),
  test_ed %>% select(ED, geometry)
)

cat("✓ Found", nrow(test_intersect), "block groups intersecting ED 34001\n\n")

if (nrow(test_intersect) > 0) {
  cat("📊 Asian population in these block groups:\n")
  summary_data <- test_intersect %>%
    st_drop_geometry() %>%
    select(GEOID, total_asian, bangladeshi, chinese)

  print(summary_data)

  cat("\n💡 Aggregated totals for ED 34001:\n")
  cat(sprintf("  Total Asian: %.0f\n", sum(test_intersect$total_asian, na.rm=TRUE)))
  cat(sprintf("  Bangladeshi: %.0f\n", sum(test_intersect$bangladeshi, na.rm=TRUE)))
  cat(sprintf("  Chinese: %.0f\n", sum(test_intersect$chinese, na.rm=TRUE)))
} else {
  cat("⚠️  No block groups found! Spatial join failed.\n")
}

cat("\n✅ Debug complete\n")
