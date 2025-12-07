library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(ggplot2)
library(ggspatial)
library(viridis)
library(patchwork)

setwd("~/Mayoral Results AD34")

# Set Census API key
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

cat("========================================\n")
cat("NY-8 Congressional District Demographics\n")
cat("========================================\n\n")

# Load NY-8 Congressional District boundary
cat("Loading NY-8 boundary...\n")
cd <- congressional_districts(state = "NY", year = 2022)
ny8 <- cd %>% filter(CD118FP == "08") %>% st_transform(3857)

cat("NY-8 loaded successfully\n\n")

# Get tract-level demographic data
cat("Pulling demographic data from Census...\n")

# Variables to pull
vars <- c(
  total_pop = "B01003_001",
  hispanic = "B03002_012",
  white_nh = "B03002_003",
  black_nh = "B03002_004",
  asian_nh = "B03002_006",
  foreign_born = "B05002_013",
  median_income = "B19013_001",
  bachelor_plus = "B15003_022",
  total_25plus = "B15003_001",
  median_age = "B01002_001",
  renter_occupied = "B25003_003",
  total_occupied = "B25003_001"
)

demo_data <- get_acs(
  geography = "tract",
  state = "NY",
  county = c("New York", "Kings", "Queens"),  # Manhattan, Brooklyn, Queens
  variables = vars,
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  st_make_valid()

cat(sprintf("Loaded %d tracts\n", nrow(demo_data)))

# Filter to NY-8
cat("Filtering to NY-8 boundary...\n")
centroids <- suppressWarnings(st_centroid(demo_data))
demo_data$in_ny8 <- st_within(centroids, ny8, sparse = FALSE)[,1]
demo_ny8 <- demo_data %>% filter(in_ny8)

cat(sprintf("Filtered to %d tracts in NY-8\n\n", nrow(demo_ny8)))

# Calculate percentages
demo_ny8 <- demo_ny8 %>%
  mutate(
    pct_hispanic = (hispanicE / total_popE) * 100,
    pct_white = (white_nhE / total_popE) * 100,
    pct_black = (black_nhE / total_popE) * 100,
    pct_asian = (asian_nhE / total_popE) * 100,
    pct_foreign_born = (foreign_bornE / total_popE) * 100,
    pct_bachelor = (bachelor_plusE / total_25plusE) * 100,
    pct_renter = (renter_occupiedE / total_occupiedE) * 100
  )

# Summary stats
cat("NY-8 Summary Statistics:\n")
cat("========================================\n")
cat(sprintf("Total Population: %s\n", format(sum(demo_ny8$total_popE, na.rm = TRUE), big.mark = ",")))
cat(sprintf("Median Income: $%s\n", format(round(median(demo_ny8$median_incomeE, na.rm = TRUE)), big.mark = ",")))
cat(sprintf("Median Age: %.1f years\n", median(demo_ny8$median_ageE, na.rm = TRUE)))
cat(sprintf("Hispanic: %.1f%%\n", weighted.mean(demo_ny8$pct_hispanic, demo_ny8$total_popE, na.rm = TRUE)))
cat(sprintf("White (NH): %.1f%%\n", weighted.mean(demo_ny8$pct_white, demo_ny8$total_popE, na.rm = TRUE)))
cat(sprintf("Black (NH): %.1f%%\n", weighted.mean(demo_ny8$pct_black, demo_ny8$total_popE, na.rm = TRUE)))
cat(sprintf("Asian (NH): %.1f%%\n", weighted.mean(demo_ny8$pct_asian, demo_ny8$total_popE, na.rm = TRUE)))
cat(sprintf("Foreign Born: %.1f%%\n", weighted.mean(demo_ny8$pct_foreign_born, demo_ny8$total_popE, na.rm = TRUE)))
cat(sprintf("Bachelor's+: %.1f%%\n", weighted.mean(demo_ny8$pct_bachelor, demo_ny8$total_25plusE, na.rm = TRUE)))
cat(sprintf("Renters: %.1f%%\n\n", weighted.mean(demo_ny8$pct_renter, demo_ny8$total_occupiedE, na.rm = TRUE)))

# Create maps
cat("Creating demographic maps...\n")

# Transform for mapping
demo_map <- st_transform(demo_ny8, 4326)
ny8_map <- st_transform(ny8, 4326)

# Helper function for maps
make_map <- function(data, var, title, palette = "viridis") {
  ggplot() +
    geom_sf(data = data, aes(fill = .data[[var]]), color = NA) +
    geom_sf(data = ny8_map, fill = NA, color = "red", linewidth = 1) +
    scale_fill_viridis_c(option = palette, name = "%", na.value = "gray90") +
    labs(title = title, caption = "ACS 2019-2023 5-year | Red boundary: NY-8") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.caption = element_text(size = 8),
      legend.position = "right"
    )
}

# Create individual maps
map_hispanic <- make_map(demo_map, "pct_hispanic", "Hispanic Population", "plasma")
map_white <- make_map(demo_map, "pct_white", "White (Non-Hispanic)", "mako")
map_black <- make_map(demo_map, "pct_black", "Black (Non-Hispanic)", "cividis")
map_asian <- make_map(demo_map, "pct_asian", "Asian (Non-Hispanic)", "viridis")
map_foreign <- make_map(demo_map, "pct_foreign_born", "Foreign Born", "magma")
map_bachelor <- make_map(demo_map, "pct_bachelor", "Bachelor's Degree or Higher", "plasma")

# Income map (different scale)
map_income <- ggplot() +
  geom_sf(data = demo_map, aes(fill = median_incomeE), color = NA) +
  geom_sf(data = ny8_map, fill = NA, color = "red", linewidth = 1) +
  scale_fill_viridis_c(option = "rocket", name = "$",
                       labels = scales::dollar_format(),
                       na.value = "gray90") +
  labs(title = "Median Household Income",
       caption = "ACS 2019-2023 5-year | Red boundary: NY-8") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

map_renter <- make_map(demo_map, "pct_renter", "Renter-Occupied Housing", "turbo")

# Combine into layouts
cat("Combining maps...\n")

# Layout 1: Race/Ethnicity
race_maps <- (map_hispanic | map_white) / (map_black | map_asian) +
  plot_annotation(
    title = "NY-8 Congressional District: Race and Ethnicity",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Layout 2: Socioeconomic
socio_maps <- (map_foreign | map_bachelor) / (map_income | map_renter) +
  plot_annotation(
    title = "NY-8 Congressional District: Socioeconomic Characteristics",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Save maps
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

ggsave("outputs/maps/ny8_race_ethnicity.png", race_maps,
       width = 14, height = 10, dpi = 300)
cat("✓ Saved: outputs/maps/ny8_race_ethnicity.png\n")

ggsave("outputs/maps/ny8_socioeconomic.png", socio_maps,
       width = 14, height = 10, dpi = 300)
cat("✓ Saved: outputs/maps/ny8_socioeconomic.png\n")

# Save data
demo_ny8 %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, total_popE, pct_hispanic, pct_white, pct_black, pct_asian,
         pct_foreign_born, median_incomeE, pct_bachelor, median_ageE, pct_renter) %>%
  write.csv("outputs/tables/ny8_demographics.csv", row.names = FALSE)
cat("✓ Saved: outputs/tables/ny8_demographics.csv\n\n")

cat("========================================\n")
cat("COMPLETE!\n")
cat("========================================\n")
