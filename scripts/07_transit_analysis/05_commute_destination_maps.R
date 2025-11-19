#!/usr/bin/env Rscript
# ==============================================================================
# COMPREHENSIVE COMMUTE DESTINATION MAPS
# ==============================================================================
# Creates detailed maps showing where AD34 residents work
# Includes NYC-wide view, borough details, and neighborhood identification

library(tidyverse)
library(sf)
library(tigris)
library(viridis)
library(ggplot2)
library(patchwork)
library(scales)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

setwd("~/Mayoral Results AD34")
dir.create("outputs/transit/commute_flows/maps", showWarnings = FALSE, recursive = TRUE)

cat("\n🗺️  COMMUTE DESTINATION MAPPING\n")
cat("==============================\n\n")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("📂 Loading commute data...\n")

# Load OD data
dest_counties <- read_csv("outputs/transit/commute_flows/destination_counties.csv",
                          show_col_types = FALSE)
dest_tracts <- read_csv("outputs/transit/commute_flows/all_destination_tracts.csv",
                        show_col_types = FALSE)

total_workers <- sum(dest_counties$workers)

cat(sprintf("✓ Total workers: %s\n", format(total_workers, big.mark = ",")))

# Load AD34 boundary
cat("  Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_4326 <- st_transform(ad34, 4326)
ad34_3857 <- st_transform(ad34, 3857)

# ==============================================================================
# 1. NYC-WIDE EMPLOYMENT DENSITY MAP
# ==============================================================================

cat("\n🗺️  Creating NYC-wide employment density map...\n")

# Get all NYC boroughs
nyc_counties <- c("Queens" = "081", "Kings" = "047", "New York" = "061",
                  "Bronx" = "005", "Richmond" = "085")

# Load all NYC tracts
all_nyc_tracts <- map_dfr(names(nyc_counties), function(county) {
  tracts(state = "NY", county = county, year = 2021) %>%
    mutate(borough = county)
})

# Join with employment data
all_nyc_tracts <- all_nyc_tracts %>%
  st_transform(4326) %>%
  left_join(
    dest_tracts %>% mutate(GEOID = as.character(dest_tract)),
    by = "GEOID"
  )

# Create NYC-wide map
p_nyc_wide <- ggplot() +
  geom_sf(data = all_nyc_tracts, aes(fill = workers), color = "white", linewidth = 0.05) +
  geom_sf(data = ad34_4326, fill = NA, color = "red", linewidth = 1.5) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Workers\nfrom AD34",
    trans = "log1p",
    breaks = c(0, 10, 50, 100, 200),
    na.value = "gray95",
    labels = comma
  ) +
  labs(
    title = "Where Do AD34 Residents Work?",
    subtitle = sprintf("Employment destinations across NYC • %s total workers",
                      format(total_workers, big.mark = ",")),
    caption = "Source: Census LODES 2021 | Red outline = AD34 (home)"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/nyc_employment_density.png",
       p_nyc_wide, width = 14, height = 10, dpi = 300, bg = "white")

cat("✓ Saved: nyc_employment_density.png\n")

# ==============================================================================
# 2. QUEENS EMPLOYMENT CENTERS (DETAILED)
# ==============================================================================

cat("\n🗺️  Creating detailed Queens employment map...\n")

# Get Queens tracts
queens_tracts <- all_nyc_tracts %>%
  filter(borough == "Queens")

# Identify top Queens employment tracts
queens_top <- queens_tracts %>%
  filter(!is.na(workers), workers >= 50) %>%
  arrange(desc(workers))

# Create Queens detail map
p_queens_detail <- ggplot() +
  geom_sf(data = queens_tracts, fill = "gray95", color = "white", linewidth = 0.1) +
  geom_sf(data = queens_tracts %>% filter(!is.na(workers)),
          aes(fill = workers), color = "white", linewidth = 0.15) +
  geom_sf(data = ad34_4326, fill = NA, color = "red", linewidth = 1.5) +
  # Label top 5 tracts
  geom_sf_text(
    data = queens_top %>% head(5),
    aes(label = sprintf("%d", workers)),
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Workers",
    na.value = "transparent",
    labels = comma
  ) +
  labs(
    title = "Queens: Major Employment Centers for AD34 Residents",
    subtitle = "10,730 workers (38.5%) work in Queens • Top concentrations labeled",
    caption = "Source: Census LODES 2021 | Red outline = AD34"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/queens_employment_detail.png",
       p_queens_detail, width = 12, height = 10, dpi = 300, bg = "white")

cat("✓ Saved: queens_employment_detail.png\n")

# ==============================================================================
# 3. MANHATTAN EMPLOYMENT
# ==============================================================================

cat("\n🗺️  Creating Manhattan employment map...\n")

manhattan_tracts <- all_nyc_tracts %>%
  filter(borough == "New York")

p_manhattan <- ggplot() +
  geom_sf(data = manhattan_tracts, fill = "gray95", color = "white", linewidth = 0.1) +
  geom_sf(data = manhattan_tracts %>% filter(!is.na(workers)),
          aes(fill = workers), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Workers",
    na.value = "transparent",
    labels = comma
  ) +
  labs(
    title = "Manhattan Employment",
    subtitle = "2,396 workers (8.6%) commute to Manhattan",
    caption = "Source: Census LODES 2021"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/manhattan_employment_detail.png",
       p_manhattan, width = 8, height = 12, dpi = 300, bg = "white")

cat("✓ Saved: manhattan_employment_detail.png\n")

# ==============================================================================
# 4. BROOKLYN EMPLOYMENT
# ==============================================================================

cat("\n🗺️  Creating Brooklyn employment map...\n")

brooklyn_tracts <- all_nyc_tracts %>%
  filter(borough == "Kings")

p_brooklyn <- ggplot() +
  geom_sf(data = brooklyn_tracts, fill = "gray95", color = "white", linewidth = 0.1) +
  geom_sf(data = brooklyn_tracts %>% filter(!is.na(workers)),
          aes(fill = workers), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Workers",
    na.value = "transparent",
    labels = comma
  ) +
  labs(
    title = "Brooklyn Employment",
    subtitle = "3,860 workers (13.8%) commute to Brooklyn",
    caption = "Source: Census LODES 2021"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/brooklyn_employment_detail.png",
       p_brooklyn, width = 10, height = 10, dpi = 300, bg = "white")

cat("✓ Saved: brooklyn_employment_detail.png\n")

# ==============================================================================
# 5. REGIONAL MAP (INCLUDING NJ)
# ==============================================================================

cat("\n🗺️  Creating tri-state regional map...\n")

# Get county-level data
county_employment <- dest_counties %>%
  filter(workers >= 200) %>%  # Only show counties with 200+ workers
  mutate(
    state_fips = substr(dest_county, 1, 2),
    county_fips = substr(dest_county, 3, 5)
  )

# Load county boundaries for NY, NJ, CT
cat("  Loading county boundaries...\n")
ny_counties <- counties("NY", year = 2021) %>% mutate(state = "NY")
nj_counties <- counties("NJ", year = 2021) %>% mutate(state = "NJ")
ct_counties <- counties("CT", year = 2021) %>% mutate(state = "CT")

all_counties <- bind_rows(ny_counties, nj_counties, ct_counties) %>%
  st_transform(4326)

# Join with employment data
all_counties <- all_counties %>%
  left_join(
    county_employment,
    by = c("GEOID" = "dest_county")
  )

# Create bounding box for NYC metro area
nyc_metro_bbox <- st_bbox(c(xmin = -74.5, xmax = -73.5, ymin = 40.4, ymax = 41.2),
                          crs = st_crs(4326))

p_regional <- ggplot() +
  geom_sf(data = all_counties, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_sf(data = all_counties %>% filter(!is.na(workers)),
          aes(fill = workers), color = "white", linewidth = 0.4) +
  geom_sf(data = ad34_4326, fill = NA, color = "red", linewidth = 2) +
  # Add state labels
  annotate("text", x = -74.3, y = 40.8, label = "NEW\nJERSEY",
           size = 5, color = "gray40", fontface = "bold") +
  annotate("text", x = -73.95, y = 40.7, label = "NYC",
           size = 6, color = "gray20", fontface = "bold") +
  coord_sf(xlim = c(-74.5, -73.5), ylim = c(40.4, 41.2)) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Workers\nfrom AD34",
    labels = comma,
    na.value = "transparent"
  ) +
  labs(
    title = "Tri-State Region: Where AD34 Residents Work",
    subtitle = "2,662 workers (9.5%) commute out of NY state • 2,093 work in NJ",
    caption = "Source: Census LODES 2021 | Red outline = AD34 (Jackson Heights/Corona)"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/regional_tristate_employment.png",
       p_regional, width = 14, height = 10, dpi = 300, bg = "white")

cat("✓ Saved: regional_tristate_employment.png\n")

# ==============================================================================
# 6. FOUR-PANEL COMPARISON MAP
# ==============================================================================

cat("\n🗺️  Creating four-panel comparison map...\n")

# Create simplified versions for panel
p1 <- ggplot() +
  geom_sf(data = queens_tracts, aes(fill = workers), color = "white", linewidth = 0.1) +
  geom_sf(data = ad34_4326, fill = NA, color = "red", linewidth = 1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "gray95",
                       trans = "log1p", guide = "none") +
  labs(title = "Queens (38.5%)\n10,730 workers") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

p2 <- ggplot() +
  geom_sf(data = brooklyn_tracts, aes(fill = workers), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "gray95",
                       trans = "log1p", guide = "none") +
  labs(title = "Brooklyn (13.8%)\n3,860 workers") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

p3 <- ggplot() +
  geom_sf(data = manhattan_tracts, aes(fill = workers), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "gray95",
                       trans = "log1p", guide = "none") +
  labs(title = "Manhattan (8.6%)\n2,396 workers") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# NJ counties for panel 4
nj_employment <- all_counties %>%
  filter(state == "NJ", !is.na(workers))

p4 <- ggplot() +
  geom_sf(data = nj_counties, fill = "gray95", color = "white", linewidth = 0.3) +
  geom_sf(data = nj_employment, aes(fill = workers), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "transparent",
                       guide = "none") +
  labs(title = "New Jersey (7.5%)\n2,093 workers") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# Combine panels
p_combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "AD34 Commute Destinations: Four Major Employment Areas",
    subtitle = sprintf("Where %s workers live in AD34 and work across the region",
                      format(total_workers, big.mark = ",")),
    caption = "Source: Census LODES 2021",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    )
  )

ggsave("outputs/transit/commute_flows/maps/four_panel_comparison.png",
       p_combined, width = 16, height = 14, dpi = 300, bg = "white")

cat("✓ Saved: four_panel_comparison.png\n")

# ==============================================================================
# 7. TOP 10 EMPLOYMENT CENTERS (ZOOMED)
# ==============================================================================

cat("\n🗺️  Creating top 10 employment centers map...\n")

# Get top 10 tracts
top10_tracts <- dest_tracts %>%
  head(10) %>%
  mutate(GEOID = as.character(dest_tract))

# Get geometries for top 10
top10_with_geo <- queens_tracts %>%
  filter(GEOID %in% top10_tracts$GEOID) %>%
  left_join(top10_tracts %>% select(GEOID, workers), by = "GEOID") %>%
  mutate(rank = rank(-workers.y))

# Create buffer around top 10 for context
top10_bbox <- st_bbox(st_buffer(st_union(top10_with_geo), 2000))

p_top10 <- ggplot() +
  geom_sf(data = queens_tracts, fill = "gray95", color = "white", linewidth = 0.1) +
  geom_sf(data = ad34_4326, fill = "lightblue", alpha = 0.3, color = "blue", linewidth = 1) +
  geom_sf(data = top10_with_geo, aes(fill = workers.y), color = "black", linewidth = 0.5) +
  geom_sf_text(data = top10_with_geo, aes(label = sprintf("#%d\n%d", rank, workers.y)),
               size = 3, fontface = "bold", color = "white") +
  coord_sf(xlim = top10_bbox[c(1,3)], ylim = top10_bbox[c(2,4)]) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Workers") +
  labs(
    title = "Top 10 Workplace Organizing Targets",
    subtitle = "Highest concentrations of AD34 workers • All in Queens",
    caption = "Source: Census LODES 2021 | Blue = AD34 home | Numbers = workplace rank & worker count"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/maps/top10_targets_zoomed.png",
       p_top10, width = 14, height = 10, dpi = 300, bg = "white")

cat("✓ Saved: top10_targets_zoomed.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("✅ COMMUTE DESTINATION MAPS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("📊 Maps Created (outputs/transit/commute_flows/maps/):\n\n")
cat("  1. nyc_employment_density.png          - NYC-wide employment heatmap\n")
cat("  2. queens_employment_detail.png        - Detailed Queens employment centers\n")
cat("  3. manhattan_employment_detail.png     - Manhattan employment distribution\n")
cat("  4. brooklyn_employment_detail.png      - Brooklyn employment distribution\n")
cat("  5. regional_tristate_employment.png    - Tri-state region (NY, NJ, CT)\n")
cat("  6. four_panel_comparison.png           - Side-by-side borough comparison\n")
cat("  7. top10_targets_zoomed.png            - Zoomed view of top 10 workplace targets\n\n")

cat("🎯 KEY INSIGHTS:\n\n")
cat("  • Most employment is LOCAL in Queens (38.5%)\n")
cat("  • Brooklyn is 2nd largest destination (13.8%)\n")
cat("  • Manhattan is only 8.6% (lower than expected)\n")
cat("  • 9.5% commute OUT OF STATE (mostly NJ)\n")
cat("  • Top 10 workplace targets all within Queens\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
