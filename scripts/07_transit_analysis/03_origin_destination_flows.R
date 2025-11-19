# ============================================================
# ORIGIN-DESTINATION COMMUTE FLOW ANALYSIS
# ============================================================
# Uses Census LODES data to show where AD34 residents work
# Identifies major employment destinations for workplace organizing
# ============================================================

library(tidyverse)
library(sf)
library(tigris)
library(lehdr)  # For LODES data
library(viridis)
library(ggplot2)

setwd("~/Mayoral Results AD34")
dir.create("outputs/transit/commute_flows", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw/lodes", showWarnings = FALSE, recursive = TRUE)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

cat("\n🗺️  ORIGIN-DESTINATION COMMUTE FLOW ANALYSIS\n")
cat("==========================================\n\n")

# ============================================================
# INSTALL LEHDR IF NEEDED
# ============================================================

if (!require(lehdr, quietly = TRUE)) {
  cat("📦 Installing lehdr package for LODES data...\n")
  install.packages("lehdr", repos = "https://cloud.r-project.org")
  library(lehdr)
}

# ============================================================
# LOAD AD34 GEOGRAPHY
# ============================================================

cat("📐 Loading AD34 boundaries...\n")

sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_4326 <- st_transform(ad34, 4326)
ad34_3857 <- st_transform(ad34, 3857)

# Get census blocks in AD34
cat("  Loading census blocks for Queens...\n")
queens_blocks <- blocks(state = "NY", county = "Queens", year = 2020)
queens_blocks_3857 <- st_transform(queens_blocks, 3857)

# Filter to blocks in AD34 (centroid method)
block_centroids <- st_centroid(queens_blocks_3857)
ad34_blocks <- queens_blocks_3857[st_within(block_centroids, ad34_3857, sparse = FALSE)[,1], ]

cat("  ✓ Found", nrow(ad34_blocks), "census blocks in AD34\n\n")

# ============================================================
# DOWNLOAD LODES DATA
# ============================================================

cat("📥 Downloading LODES Origin-Destination data for New York...\n")
cat("  (This shows where people live and where they work)\n\n")

# Download LODES OD data for New York
# OD = Origin-Destination (home block -> work block)
# Using 2021 (most recent available)
# job_type = "JT00" = all jobs
# segment = "S000" = all workers

tryCatch({
  # Download MAIN OD data (in-state jobs)
  # This gets workers who live AND work in NY
  cat("  Downloading MAIN file (in-state jobs)...\n")
  lodes_od_main <- grab_lodes(
    state = "ny",
    year = 2021,
    lodes_type = "od",
    job_type = "JT00",
    segment = "S000",
    state_part = "main",
    agg_geo = "block"
  )
  cat("  ✓ Main file:", nrow(lodes_od_main), "OD pairs\n")

  # Download AUX OD data (out-of-state jobs)
  # This gets workers who live in NY but work in OTHER states (NJ, CT, etc.)
  cat("  Downloading AUX file (out-of-state jobs)...\n")
  lodes_od_aux <- grab_lodes(
    state = "ny",
    year = 2021,
    lodes_type = "od",
    job_type = "JT00",
    segment = "S000",
    state_part = "aux",
    agg_geo = "block"
  )
  cat("  ✓ Aux file:", nrow(lodes_od_aux), "OD pairs\n")

  # Combine both datasets
  lodes_od <- bind_rows(lodes_od_main, lodes_od_aux)
  cat("  ✓ Combined total:", nrow(lodes_od), "OD pairs (includes out-of-state commutes)\n\n")

}, error = function(e) {
  cat("  ⚠️  Error downloading LODES:", e$message, "\n")
  cat("  Trying alternative method...\n\n")

  # Alternative: download directly
  url_main <- "https://lehd.ces.census.gov/data/lodes/LODES8/ny/od/ny_od_main_JT00_2021.csv.gz"
  url_aux <- "https://lehd.ces.census.gov/data/lodes/LODES8/ny/od/ny_od_aux_JT00_2021.csv.gz"

  destfile_main <- "data/raw/lodes/ny_od_main_JT00_2021.csv.gz"
  destfile_aux <- "data/raw/lodes/ny_od_aux_JT00_2021.csv.gz"

  cat("  Downloading main file...\n")
  download.file(url_main, destfile_main, mode = "wb")
  lodes_od_main <- read_csv(destfile_main, show_col_types = FALSE)

  cat("  Downloading aux file...\n")
  download.file(url_aux, destfile_aux, mode = "wb")
  lodes_od_aux <- read_csv(destfile_aux, show_col_types = FALSE)

  lodes_od <- bind_rows(lodes_od_main, lodes_od_aux)
  cat("  ✓ Combined total:", nrow(lodes_od), "OD pairs\n\n")
})

# ============================================================
# FILTER TO AD34 RESIDENTS
# ============================================================

cat("🎯 Filtering to workers who LIVE in AD34...\n")

# Get GEOIDs of blocks in AD34
ad34_block_geoids <- ad34_blocks$GEOID20

# Filter LODES to only origin blocks in AD34
# w_geocode = home location (where worker lives)
# h_geocode = work location (where worker works)
ad34_commutes <- lodes_od %>%
  filter(w_geocode %in% ad34_block_geoids)

cat("  ✓ Found", nrow(ad34_commutes), "commute flows from AD34\n")
cat("  ✓ Total workers:", format(sum(ad34_commutes$S000), big.mark = ","), "\n\n")

# ============================================================
# AGGREGATE BY DESTINATION
# ============================================================

cat("📊 Analyzing where AD34 residents work...\n\n")

# Aggregate by destination census tract (first 11 digits of h_geocode)
ad34_commutes <- ad34_commutes %>%
  mutate(
    dest_tract = substr(h_geocode, 1, 11),
    dest_county = substr(h_geocode, 1, 5),
    dest_state = substr(h_geocode, 1, 2)
  )

# ALL destination tracts (for mapping)
all_dest_tracts <- ad34_commutes %>%
  group_by(dest_tract) %>%
  summarise(
    workers = sum(S000),
    .groups = "drop"
  ) %>%
  arrange(desc(workers))

# Top 20 destination tracts (for summary)
top_dest_tracts <- all_dest_tracts %>%
  head(20)

# Top destination counties
top_dest_counties <- ad34_commutes %>%
  group_by(dest_county) %>%
  summarise(
    workers = sum(S000),
    .groups = "drop"
  ) %>%
  left_join(
    tibble(
      dest_county = c(
        # NYC boroughs
        "36061", "36081", "36047", "36005", "36085",
        # NY suburban counties
        "36059", "36103", "36119", "36087", "36027",
        # NJ counties (major commuter counties)
        "34003", "34017", "34013", "34023", "34025", "34031", "34039", "34019",
        # CT counties
        "09001", "09009", "09013"
      ),
      county_name = c(
        # NYC
        "Manhattan", "Queens", "Kings (Brooklyn)", "Bronx", "Richmond (Staten Island)",
        # NY suburban
        "Nassau (NY)", "Suffolk (NY)", "Westchester (NY)", "Rockland (NY)", "Dutchess (NY)",
        # NJ
        "Bergen (NJ)", "Hudson (NJ)", "Essex (NJ)", "Morris (NJ)", "Monmouth (NJ)",
        "Passaic (NJ)", "Union (NJ)", "Hunterdon (NJ)",
        # CT
        "Fairfield (CT)", "New Haven (CT)", "New London (CT)"
      )
    ),
    by = "dest_county"
  ) %>%
  mutate(
    # For unmapped counties, show state and county code
    county_name = if_else(
      is.na(county_name),
      case_when(
        substr(dest_county, 1, 2) == "34" ~ paste0("NJ County (", dest_county, ")"),
        substr(dest_county, 1, 2) == "09" ~ paste0("CT County (", dest_county, ")"),
        substr(dest_county, 1, 2) == "36" ~ paste0("NY County (", dest_county, ")"),
        TRUE ~ paste0("Other State (", dest_county, ")")
      ),
      county_name
    )
  ) %>%
  arrange(desc(workers))

cat("🔴 TOP EMPLOYMENT DESTINATIONS (by county):\n\n")

for (i in 1:min(10, nrow(top_dest_counties))) {
  county <- top_dest_counties[i,]
  pct <- 100 * county$workers / sum(ad34_commutes$S000)
  cat(sprintf("%d. %s: %s workers (%.1f%%)\n",
              i,
              county$county_name,
              format(county$workers, big.mark = ","),
              pct))
}

# Within-Queens analysis
queens_commutes <- ad34_commutes %>%
  filter(dest_county == "36081")

cat("\n📍 WITHIN QUEENS COMMUTES:\n")
cat(sprintf("  %s workers (%.1f%%) work elsewhere in Queens\n",
            format(sum(queens_commutes$S000), big.mark = ","),
            100 * sum(queens_commutes$S000) / sum(ad34_commutes$S000)))

# Manhattan commutes (major target)
manhattan_commutes <- ad34_commutes %>%
  filter(dest_county == "36061")

cat("\n🏙️  MANHATTAN COMMUTES:\n")
cat(sprintf("  %s workers (%.1f%%) commute to Manhattan\n",
            format(sum(manhattan_commutes$S000), big.mark = ","),
            100 * sum(manhattan_commutes$S000) / sum(ad34_commutes$S000)))

# Out-of-state commutes
out_of_state <- ad34_commutes %>%
  filter(dest_state != "36") %>%
  group_by(dest_state) %>%
  summarise(workers = sum(S000), .groups = "drop") %>%
  arrange(desc(workers))

if (nrow(out_of_state) > 0) {
  total_out_of_state <- sum(out_of_state$workers)
  cat("\n🌎 OUT-OF-STATE COMMUTES:\n")
  cat(sprintf("  %s workers (%.1f%%) work outside New York State\n",
              format(total_out_of_state, big.mark = ","),
              100 * total_out_of_state / sum(ad34_commutes$S000)))

  # Show breakdown by state
  state_names <- c("09" = "Connecticut", "34" = "New Jersey", "42" = "Pennsylvania")
  for (i in 1:min(5, nrow(out_of_state))) {
    state_code <- out_of_state$dest_state[i]
    state_name <- if_else(!is.na(state_names[state_code]),
                          state_names[state_code],
                          paste0("State ", state_code))
    cat(sprintf("    • %s: %s workers\n",
                state_name,
                format(out_of_state$workers[i], big.mark = ",")))
  }
} else {
  cat("\n🌎 OUT-OF-STATE COMMUTES:\n")
  cat("  No out-of-state commuters found (or all work in NY)\n")
}

# ============================================================
# TOP SPECIFIC WORK LOCATIONS
# ============================================================

cat("\n\n🎯 TOP 20 SPECIFIC WORK LOCATIONS (census tracts):\n")
cat("==================================================\n\n")

# Get tract geometries for mapping
queens_tracts <- tracts(state = "NY", county = "Queens", year = 2020)
manhattan_tracts <- tracts(state = "NY", county = "New York", year = 2020)
brooklyn_tracts <- tracts(state = "NY", county = "Kings", year = 2020)

all_tracts <- bind_rows(
  queens_tracts,
  manhattan_tracts,
  brooklyn_tracts
)

# Join top destinations with tract geometries
top_dest_geo <- top_dest_tracts %>%
  left_join(
    all_tracts %>% select(GEOID, geometry) %>% st_transform(4326),
    by = c("dest_tract" = "GEOID")
  ) %>%
  st_as_sf()

for (i in 1:min(20, nrow(top_dest_tracts))) {
  dest <- top_dest_tracts[i,]
  pct <- 100 * dest$workers / sum(ad34_commutes$S000)

  # Try to identify location
  tract_geo <- all_tracts %>% filter(GEOID == dest$dest_tract)
  location <- if (nrow(tract_geo) > 0) {
    county_code <- substr(dest$dest_tract, 1, 5)
    county_name <- case_when(
      county_code == "36061" ~ "Manhattan",
      county_code == "36081" ~ "Queens",
      county_code == "36047" ~ "Brooklyn",
      county_code == "36005" ~ "Bronx",
      county_code == "36085" ~ "Staten Island",
      TRUE ~ "Other"
    )
    sprintf("%s, Tract %s", county_name, substr(dest$dest_tract, 6, 11))
  } else {
    dest$dest_tract
  }

  cat(sprintf("%2d. %s workers → %s (%.1f%%)\n",
              i,
              format(dest$workers, big.mark = ","),
              location,
              pct))
}

# ============================================================
# IDENTIFY STRATEGIC WORKPLACE TARGETS
# ============================================================

cat("\n\n🎯 STRATEGIC WORKPLACE ORGANIZING TARGETS:\n")
cat("=========================================\n\n")

# Identify high-concentration work locations
strategic_workplaces <- top_dest_tracts %>%
  head(10) %>%
  left_join(
    all_tracts %>% st_drop_geometry() %>% select(GEOID, NAME),
    by = c("dest_tract" = "GEOID")
  ) %>%
  mutate(
    county = case_when(
      substr(dest_tract, 1, 5) == "36061" ~ "Manhattan",
      substr(dest_tract, 1, 5) == "36081" ~ "Queens",
      substr(dest_tract, 1, 5) == "36047" ~ "Brooklyn",
      TRUE ~ "Other"
    ),
    organizing_value = workers,
    priority = row_number()
  )

cat("Top workplace targets for lunch-time tabling, workplace organizing:\n\n")

for (i in 1:nrow(strategic_workplaces)) {
  workplace <- strategic_workplaces[i,]
  cat(sprintf("Priority %d: %s, %s\n",
              workplace$priority,
              workplace$county,
              workplace$NAME))
  cat(sprintf("  - %s AD34 residents work here\n",
              format(workplace$workers, big.mark = ",")))
  cat(sprintf("  - %.1f%% of district's workforce\n\n",
              100 * workplace$workers / sum(ad34_commutes$S000)))
}

# ============================================================
# SAVE RESULTS
# ============================================================

write_csv(top_dest_counties, "outputs/transit/commute_flows/destination_counties.csv")
write_csv(top_dest_tracts, "outputs/transit/commute_flows/destination_tracts.csv")
write_csv(all_dest_tracts, "outputs/transit/commute_flows/all_destination_tracts.csv")
write_csv(strategic_workplaces, "outputs/transit/commute_flows/workplace_targets.csv")

cat("  ✓ Saved destination analysis CSVs\n\n")

# ============================================================
# CREATE FLOW MAP
# ============================================================

cat("🗺️  Creating commute flow visualization...\n")

# Aggregate to county-level flows for cleaner visualization
county_flows <- ad34_commutes %>%
  group_by(dest_county) %>%
  summarise(workers = sum(S000), .groups = "drop") %>%
  filter(workers >= 100) %>%  # Only show significant flows
  arrange(desc(workers))

# Get county geometries
nyc_counties <- counties(state = "NY", year = 2020) %>%
  filter(COUNTYFP %in% c("005", "047", "061", "081", "085")) %>%
  st_transform(4326)

# Create county centroids for flow lines
county_centroids <- st_centroid(nyc_counties)
ad34_centroid <- st_centroid(ad34_4326)

# Prepare flow lines
flow_lines <- county_flows %>%
  left_join(
    county_centroids %>%
      st_drop_geometry() %>%
      mutate(dest_county = paste0("36", COUNTYFP)) %>%
      select(dest_county),
    by = "dest_county"
  )

# Join geometries for destinations
flow_lines_sf <- flow_lines %>%
  left_join(
    county_centroids %>%
      mutate(dest_county = paste0("36", COUNTYFP)),
    by = "dest_county"
  ) %>%
  st_as_sf() %>%
  filter(!is.na(geometry))

# Create flow map
map_flows <- ggplot() +
  # Base: NYC counties
  geom_sf(data = nyc_counties, fill = "gray95", color = "gray70", linewidth = 0.3) +

  # Destination counties (sized by workers)
  geom_sf(data = flow_lines_sf,
          aes(size = workers, color = workers),
          alpha = 0.6) +
  scale_size_continuous(name = "Workers", range = c(3, 15),
                        labels = scales::comma) +
  scale_color_viridis_c(name = "Workers", option = "plasma",
                        labels = scales::comma) +

  # AD34 (origin)
  geom_sf(data = ad34_4326, fill = "#E74C3C", color = "darkred",
          linewidth = 1, alpha = 0.7) +

  # Labels
  geom_sf_text(data = nyc_counties, aes(label = NAME),
               size = 3, fontface = "bold") +

  labs(title = "Where Do AD34 Residents Work?",
       subtitle = "Commute destinations for workplace organizing",
       caption = "Source: Census LODES 2021 | Circle size = number of workers") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("outputs/transit/commute_flows/destination_map.png", map_flows,
       width = 12, height = 10, dpi = 300, bg = "white")

cat("  ✓ Saved destination_map.png\n")

# ============================================================
# CREATE DETAILED MANHATTAN MAP
# ============================================================

cat("  Creating detailed Manhattan employment map...\n")

# Get Manhattan tracts with AD34 commuters
manhattan_dest_tracts <- ad34_commutes %>%
  filter(dest_county == "36061") %>%
  group_by(dest_tract) %>%
  summarise(workers = sum(S000), .groups = "drop") %>%
  filter(workers >= 10)

manhattan_dest_geo <- manhattan_dest_tracts %>%
  left_join(
    manhattan_tracts %>% select(GEOID, geometry) %>% st_transform(4326),
    by = c("dest_tract" = "GEOID")
  ) %>%
  st_as_sf() %>%
  filter(!is.na(geometry))

map_manhattan <- ggplot() +
  geom_sf(data = st_transform(manhattan_tracts, 4326),
          fill = "gray95", color = "gray80", linewidth = 0.1) +
  geom_sf(data = manhattan_dest_geo,
          aes(fill = workers),
          color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Workers from AD34",
                       trans = "log10",
                       labels = scales::comma) +
  labs(title = "Manhattan: Where AD34 Residents Work",
       subtitle = "Major employment centers for workplace organizing",
       caption = "Source: Census LODES 2021") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/transit/commute_flows/manhattan_employment.png", map_manhattan,
       width = 8, height = 12, dpi = 300, bg = "white")

cat("  ✓ Saved manhattan_employment.png\n\n")

# ============================================================
# SUMMARY REPORT
# ============================================================

cat("📝 Creating summary report...\n")

report_lines <- c(
  "# COMMUTE FLOW ANALYSIS - AD34",
  "## Workplace Organizing Strategy",
  "",
  sprintf("**Analysis Date:** %s", Sys.Date()),
  sprintf("**Data Source:** Census LODES 2021"),
  sprintf("**Total Workers Analyzed:** %s",
          format(sum(ad34_commutes$S000), big.mark = ",")),
  "",
  "---",
  "",
  "## Where Do AD34 Residents Work?",
  ""
)

# Add county breakdown
for (i in 1:min(5, nrow(top_dest_counties))) {
  county <- top_dest_counties[i,]
  pct <- 100 * county$workers / sum(ad34_commutes$S000)
  report_lines <- c(report_lines,
    sprintf("**%s**: %s workers (%.1f%%)",
            county$county_name,
            format(county$workers, big.mark = ","),
            pct)
  )
}

report_lines <- c(report_lines,
  "",
  "---",
  "",
  "## Top 10 Workplace Organizing Targets",
  "",
  "### Lunch-Time Tabling & Workplace Outreach",
  ""
)

for (i in 1:min(10, nrow(strategic_workplaces))) {
  workplace <- strategic_workplaces[i,]
  report_lines <- c(report_lines,
    sprintf("### %d. %s", i, workplace$county),
    sprintf("- **%s workers** from AD34 (%.1f%% of district)",
            format(workplace$workers, big.mark = ","),
            100 * workplace$workers / sum(ad34_commutes$S000)),
    sprintf("- Tract: %s", workplace$NAME),
    ""
  )
}

report_lines <- c(report_lines,
  "",
  "---",
  "",
  "## Campaign Tactics",
  "",
  "### Workplace Organizing",
  "",
  "1. **Lunch-time tabling** at major employment centers",
  "2. **Workplace visits** to talk to AD34 residents at work",
  "3. **Industry-specific outreach** if data shows concentration",
  "4. **Commute-time messaging** on M60-SBS, Q29, Q33 serving these destinations",
  "",
  "### Timing",
  "",
  "- **Lunch hours:** 12-2 PM at top employment centers",
  "- **After work:** 5-7 PM at transit hubs serving these locations",
  "- **Focus days:** Tuesday-Thursday (highest workplace presence)",
  "",
  "---",
  "",
  "## Files Generated",
  "",
  "- `destination_counties.csv` - Workers by destination county",
  "- `destination_tracts.csv` - Top 20 destination census tracts",
  "- `workplace_targets.csv` - Strategic workplace organizing targets",
  "- `destination_map.png` - NYC-wide commute flow map",
  "- `manhattan_employment.png` - Detailed Manhattan employment map",
  ""
)

writeLines(report_lines, "outputs/transit/commute_flows/COMMUTE_FLOWS_SUMMARY.md")

cat("  ✓ Saved: COMMUTE_FLOWS_SUMMARY.md\n\n")

cat("✅ ORIGIN-DESTINATION ANALYSIS COMPLETE!\n\n")
cat("📂 Outputs saved to outputs/transit/commute_flows/\n\n")
cat("🎯 Key Finding: Target workplace organizing at top employment centers\n")
cat("   where AD34 residents are concentrated during work hours.\n\n")
