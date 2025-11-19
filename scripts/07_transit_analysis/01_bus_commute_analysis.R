# ============================================================
# BUS & COMMUTE ANALYSIS FOR AD34
# ============================================================
# Analyzes bus ridership patterns and commute modes to identify
# where "fast and free buses" campaign messaging will resonate most
# ============================================================

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(readr)
library(viridis)
library(ggplot2)

setwd("~/Mayoral Results AD34")
dir.create("outputs/transit", showWarnings = FALSE, recursive = TRUE)
dir.create("data/intermediate/transit", showWarnings = FALSE, recursive = TRUE)

census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

cat("\n🚌 BUS & COMMUTE ANALYSIS\n")
cat("========================\n\n")

# ============================================================
# LOAD GEOMETRY & HELPER FUNCTIONS
# ============================================================

cat("📐 Loading AD34 boundary and ED shapefile...\n")

# AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_mask <- st_union(st_transform(ad34, 3857)) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_eds <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(ED = Election_D) %>%
  st_transform(3857)

cat("  ✓ Loaded", nrow(ad34_eds), "EDs\n\n")

# ============================================================
# PULL DETAILED CENSUS COMMUTE DATA (B08301)
# ============================================================

cat("📊 Pulling detailed commute data from ACS...\n")

# B08301: Means of Transportation to Work
# Get detailed breakdown by mode

commute_vars <- c(
  total_workers = "B08301_001",          # Total workers
  drove_alone = "B08301_003",            # Car, truck, or van - alone
  carpool = "B08301_004",                # Car, truck, or van - carpool
  public_transit = "B08301_010",         # Public transportation (total)
  bus = "B08301_011",                    # Bus or trolley bus
  subway = "B08301_012",                 # Subway or elevated rail
  railroad = "B08301_013",               # Long-distance or commuter rail
  streetcar = "B08301_014",              # Light rail, streetcar, or trolley
  ferry = "B08301_015",                  # Ferryboat
  taxi = "B08301_016",                   # Taxicab, motorcycle, or other
  bicycle = "B08301_018",                # Bicycle
  walked = "B08301_019",                 # Walked
  other = "B08301_020",                  # Other means
  worked_home = "B08301_021"             # Worked from home
)

# Also get travel time variables
traveltime_vars <- c(
  total_tt_universe = "B08303_001",      # Total for travel time
  tt_under_10 = "B08303_002",            # Less than 10 minutes
  tt_10_14 = "B08303_003",               # 10 to 14 minutes
  tt_15_19 = "B08303_004",               # 15 to 19 minutes
  tt_20_24 = "B08303_005",               # 20 to 24 minutes
  tt_25_29 = "B08303_006",               # 25 to 29 minutes
  tt_30_34 = "B08303_007",               # 30 to 34 minutes
  tt_35_44 = "B08303_008",               # 35 to 44 minutes
  tt_45_59 = "B08303_009",               # 45 to 59 minutes
  tt_60_plus = "B08303_010"              # 60+ minutes
)

# Pull commute data for Queens tracts
commute_data <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = commute_vars,
  year = 2022,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  st_make_valid()

# Pull travel time data
traveltime_data <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = traveltime_vars,
  year = 2022,
  survey = "acs5",
  output = "wide",
  geometry = FALSE
)

cat("  ✓ Pulled data for", nrow(commute_data), "tracts\n\n")

# ============================================================
# SPATIAL INTERPOLATION: TRACT → ED
# ============================================================

cat("🗺️  Interpolating tract data to Election Districts...\n")

# Calculate area-weighted interpolation
interpolate_to_ed <- function(tract_data, ed_polygons) {

  # Ensure same CRS
  tract_data <- st_transform(tract_data, 3857)
  ed_polygons <- st_transform(ed_polygons, 3857)

  # Calculate intersection areas
  intersections <- st_intersection(tract_data, ed_polygons)
  intersections$area <- st_area(intersections) %>% as.numeric()

  # Calculate weights
  intersections <- intersections %>%
    group_by(GEOID) %>%
    mutate(tract_total_area = sum(area)) %>%
    ungroup() %>%
    mutate(weight = area / tract_total_area)

  # Weighted sum by ED
  ed_estimates <- intersections %>%
    st_drop_geometry() %>%
    group_by(ED) %>%
    summarise(
      total_workers = sum(total_workersE * weight, na.rm = TRUE),
      drove_alone = sum(drove_aloneE * weight, na.rm = TRUE),
      carpool = sum(carpoolE * weight, na.rm = TRUE),
      public_transit = sum(public_transitE * weight, na.rm = TRUE),
      bus = sum(busE * weight, na.rm = TRUE),
      subway = sum(subwayE * weight, na.rm = TRUE),
      railroad = sum(railroadE * weight, na.rm = TRUE),
      streetcar = sum(streetcarE * weight, na.rm = TRUE),
      ferry = sum(ferryE * weight, na.rm = TRUE),
      taxi = sum(taxiE * weight, na.rm = TRUE),
      bicycle = sum(bicycleE * weight, na.rm = TRUE),
      walked = sum(walkedE * weight, na.rm = TRUE),
      other = sum(otherE * weight, na.rm = TRUE),
      worked_home = sum(worked_homeE * weight, na.rm = TRUE)
    ) %>%
    ungroup()

  return(ed_estimates)
}

commute_by_ed <- interpolate_to_ed(commute_data, ad34_eds)

# Calculate percentages
commute_by_ed <- commute_by_ed %>%
  mutate(
    pct_bus = 100 * bus / total_workers,
    pct_subway = 100 * subway / total_workers,
    pct_public_transit = 100 * public_transit / total_workers,
    pct_drove = 100 * (drove_alone + carpool) / total_workers,
    pct_walked = 100 * walked / total_workers,
    pct_bicycle = 100 * bicycle / total_workers,
    pct_worked_home = 100 * worked_home / total_workers,

    # Bus dependency score: bus as % of all commuters (not just transit users)
    bus_dependency_score = pct_bus,

    # Transit reliance: public transit as % of all commuters
    transit_reliance = pct_public_transit,

    # Bus-to-subway ratio (among transit users)
    bus_subway_ratio = if_else(subway > 0, bus / subway, NA_real_)
  )

cat("  ✓ Interpolated to", nrow(commute_by_ed), "EDs\n\n")

# ============================================================
# INTERPOLATE TRAVEL TIME DATA
# ============================================================

cat("⏱️  Interpolating travel time data...\n")

traveltime_sf <- commute_data %>%
  select(GEOID, geometry) %>%
  left_join(traveltime_data, by = "GEOID")

interpolate_traveltime <- function(tract_data, ed_polygons) {

  tract_data <- st_transform(tract_data, 3857)
  ed_polygons <- st_transform(ed_polygons, 3857)

  intersections <- st_intersection(tract_data, ed_polygons)
  intersections$area <- st_area(intersections) %>% as.numeric()

  intersections <- intersections %>%
    group_by(GEOID) %>%
    mutate(tract_total_area = sum(area)) %>%
    ungroup() %>%
    mutate(weight = area / tract_total_area)

  ed_estimates <- intersections %>%
    st_drop_geometry() %>%
    group_by(ED) %>%
    summarise(
      total_tt_universe = sum(total_tt_universeE * weight, na.rm = TRUE),
      tt_under_10 = sum(tt_under_10E * weight, na.rm = TRUE),
      tt_10_14 = sum(tt_10_14E * weight, na.rm = TRUE),
      tt_15_19 = sum(tt_15_19E * weight, na.rm = TRUE),
      tt_20_24 = sum(tt_20_24E * weight, na.rm = TRUE),
      tt_25_29 = sum(tt_25_29E * weight, na.rm = TRUE),
      tt_30_34 = sum(tt_30_34E * weight, na.rm = TRUE),
      tt_35_44 = sum(tt_35_44E * weight, na.rm = TRUE),
      tt_45_59 = sum(tt_45_59E * weight, na.rm = TRUE),
      tt_60_plus = sum(tt_60_plusE * weight, na.rm = TRUE)
    ) %>%
    ungroup()

  return(ed_estimates)
}

traveltime_by_ed <- interpolate_traveltime(traveltime_sf, ad34_eds)

# Calculate average travel time (using midpoints)
traveltime_by_ed <- traveltime_by_ed %>%
  mutate(
    # Calculate weighted average using midpoints
    avg_commute_time = (
      tt_under_10 * 5 +
      tt_10_14 * 12 +
      tt_15_19 * 17 +
      tt_20_24 * 22 +
      tt_25_29 * 27 +
      tt_30_34 * 32 +
      tt_35_44 * 39.5 +
      tt_45_59 * 52 +
      tt_60_plus * 70  # Estimate 70 min for 60+
    ) / total_tt_universe,

    # % with long commutes (45+ minutes)
    pct_long_commute = 100 * (tt_45_59 + tt_60_plus) / total_tt_universe
  )

cat("  ✓ Travel time data ready\n\n")

# ============================================================
# MERGE WITH ELECTION & STRATEGIC DATA
# ============================================================

cat("🔗 Merging with election and strategic targeting data...\n")

# Load strategic dataset (contains election results too)
if (file.exists("outputs/analysis/actionable/master_strategic_dataset.csv")) {
  strategic_data <- read_csv("outputs/analysis/actionable/master_strategic_dataset.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE)

  # Merge everything
  master_transit <- commute_by_ed %>%
    left_join(traveltime_by_ed, by = "ED") %>%
    left_join(strategic_data, by = "ED")

} else {
  cat("  ⚠️  Strategic dataset not found - continuing without election/strategic data\n")
  master_transit <- commute_by_ed %>%
    left_join(traveltime_by_ed, by = "ED")
}

# Merge with ED geometry
master_transit_sf <- ad34_eds %>%
  select(ED, geometry) %>%
  left_join(master_transit, by = "ED")

cat("  ✓ Merged", nrow(master_transit), "EDs\n\n")

# ============================================================
# SAVE INTERMEDIATE DATA
# ============================================================

write_csv(master_transit, "data/intermediate/transit/commute_by_ed.csv")
st_write(master_transit_sf, "data/intermediate/transit/commute_by_ed.geojson",
         delete_dsn = TRUE, quiet = TRUE)

cat("  ✓ Saved: commute_by_ed.csv and .geojson\n\n")

# ============================================================
# ANALYSIS: HIGH BUS RIDERSHIP EDs
# ============================================================

cat("🔍 ANALYZING HIGH BUS RIDERSHIP...\n\n")

# Identify high bus ridership EDs
high_bus <- master_transit %>%
  arrange(desc(pct_bus)) %>%
  filter(!is.na(pct_bus)) %>%
  head(15)

cat("📊 TOP 15 EDs BY BUS RIDERSHIP:\n")
cat("===============================\n\n")

high_bus_display <- high_bus %>%
  select(ED, pct_bus, pct_subway, total_workers,
         avg_commute_time, Mamdani_pct) %>%
  mutate(
    ED_short = str_remove(ED, "^34"),
    across(c(pct_bus, pct_subway, Mamdani_pct), ~round(., 1)),
    avg_commute_time = round(avg_commute_time, 0),
    total_workers = round(total_workers, 0)
  ) %>%
  select(ED = ED_short, `Bus %` = pct_bus, `Subway %` = pct_subway,
         Workers = total_workers, `Avg Commute (min)` = avg_commute_time,
         `Mamdani %` = Mamdani_pct)

print(high_bus_display, n = 15)

# ============================================================
# STRATEGIC TARGETING FOR "FAST & FREE BUSES"
# ============================================================

cat("\n\n🎯 STRATEGIC TARGETING FOR 'FAST & FREE BUSES' CAMPAIGN\n")
cat("========================================================\n\n")

# Identify EDs where bus message will resonate:
# 1. High bus ridership (>20%)
# 2. Long commute times (>35 min avg)
# 3. High potential voters OR swing districts

bus_target_eds <- master_transit %>%
  filter(!is.na(pct_bus), !is.na(avg_commute_time)) %>%
  mutate(
    high_bus = pct_bus >= 20,
    long_commute = avg_commute_time >= 35,
    bus_dependent = pct_bus / pct_public_transit >= 0.3  # Bus is 30%+ of transit
  )

if ("strategic_type" %in% colnames(bus_target_eds)) {
  bus_target_eds <- bus_target_eds %>%
    mutate(
      strategic_priority = strategic_type %in%
        c("HIGH OPPORTUNITY", "MEDIUM OPPORTUNITY", "SWING - PERSUADE")
    )

  # High-value bus targets
  high_value_bus <- bus_target_eds %>%
    filter(high_bus, strategic_priority) %>%
    arrange(desc(potential_voters))

  cat("🔴 HIGH-VALUE BUS MESSAGING TARGETS:\n")
  cat("(High bus ridership + Strategic priority)\n\n")

  high_value_display <- high_value_bus %>%
    select(ED, pct_bus, avg_commute_time, strategic_type,
           potential_voters, Mamdani_pct) %>%
    mutate(
      ED_short = str_remove(ED, "^34"),
      across(c(pct_bus, Mamdani_pct), ~round(., 1)),
      avg_commute_time = round(avg_commute_time, 0),
      potential_voters = round(potential_voters, 0)
    ) %>%
    select(ED = ED_short, `Bus %` = pct_bus, `Commute` = avg_commute_time,
           Strategy = strategic_type, Potential = potential_voters,
           `Mamdani %` = Mamdani_pct)

  print(high_value_display, n = 20)

  # Calculate total potential impact
  total_bus_voters <- sum(high_value_bus$potential_voters, na.rm = TRUE)
  cat("\n📈 Total potential voters in high-bus strategic EDs:",
      format(round(total_bus_voters), big.mark = ","), "\n")

} else {
  # Without strategic data, just show high bus + long commute
  high_value_bus <- bus_target_eds %>%
    filter(high_bus, long_commute) %>%
    arrange(desc(total_workers))

  cat("🔴 HIGH BUS RIDERSHIP + LONG COMMUTE EDs:\n\n")

  high_value_display <- high_value_bus %>%
    select(ED, pct_bus, avg_commute_time, total_workers, Mamdani_pct) %>%
    mutate(
      ED_short = str_remove(ED, "^34"),
      across(c(pct_bus, Mamdani_pct), ~round(., 1)),
      avg_commute_time = round(avg_commute_time, 0),
      total_workers = round(total_workers, 0)
    ) %>%
    select(ED = ED_short, `Bus %` = pct_bus, `Commute` = avg_commute_time,
           Workers = total_workers, `Mamdani %` = Mamdani_pct)

  print(high_value_display, n = 20)
}

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("\n\n📊 AD34 COMMUTE SUMMARY STATISTICS\n")
cat("==================================\n\n")

summary_stats_data <- master_transit %>%
  filter(!is.na(total_workers), total_workers > 0,
         !is.na(pct_bus), !is.na(avg_commute_time))

summary_stats <- tibble(
  total_workers = sum(summary_stats_data$total_workers, na.rm = TRUE),
  bus_commuters = sum(summary_stats_data$bus, na.rm = TRUE),
  subway_commuters = sum(summary_stats_data$subway, na.rm = TRUE),
  transit_commuters = sum(summary_stats_data$public_transit, na.rm = TRUE),
  avg_bus_pct = weighted.mean(summary_stats_data$pct_bus, summary_stats_data$total_workers, na.rm = TRUE),
  avg_subway_pct = weighted.mean(summary_stats_data$pct_subway, summary_stats_data$total_workers, na.rm = TRUE),
  avg_transit_pct = weighted.mean(summary_stats_data$pct_public_transit, summary_stats_data$total_workers, na.rm = TRUE),
  avg_commute_time = weighted.mean(summary_stats_data$avg_commute_time, summary_stats_data$total_tt_universe, na.rm = TRUE)
)

cat("Total Workers:", format(round(summary_stats$total_workers), big.mark = ","), "\n")
cat("Bus Commuters:", format(round(summary_stats$bus_commuters), big.mark = ","),
    sprintf("(%.1f%%)\n", summary_stats$avg_bus_pct))
cat("Subway Commuters:", format(round(summary_stats$subway_commuters), big.mark = ","),
    sprintf("(%.1f%%)\n", summary_stats$avg_subway_pct))
cat("All Transit:", format(round(summary_stats$transit_commuters), big.mark = ","),
    sprintf("(%.1f%%)\n", summary_stats$avg_transit_pct))
cat("Average Commute Time:", round(summary_stats$avg_commute_time, 1), "minutes\n")

# ============================================================
# CREATE MAPS
# ============================================================

cat("\n\n🗺️  CREATING MAPS...\n")

# Map 1: Bus ridership percentage
map1 <- ggplot() +
  geom_sf(data = master_transit_sf,
          aes(fill = pct_bus),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Bus %",
                       na.value = "gray90") +
  geom_sf_text(data = master_transit_sf,
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "black", fontface = "bold") +
  labs(title = "Bus Commuters by Election District",
       subtitle = "% of workers who commute by bus",
       caption = "Source: ACS 2018-2022 5-year estimates (B08301)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/transit/bus_ridership_map.png", map1,
       width = 10, height = 10, dpi = 300, bg = "white")

# Map 2: Average commute time
map2 <- ggplot() +
  geom_sf(data = master_transit_sf,
          aes(fill = avg_commute_time),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "inferno", name = "Minutes",
                       na.value = "gray90") +
  geom_sf_text(data = master_transit_sf,
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "white", fontface = "bold") +
  labs(title = "Average Commute Time by Election District",
       subtitle = "Mean travel time to work (minutes)",
       caption = "Source: ACS 2018-2022 5-year estimates (B08303)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/transit/commute_time_map.png", map2,
       width = 10, height = 10, dpi = 300, bg = "white")

# Map 3: Bus dependency (high bus, low other options)
map3 <- ggplot() +
  geom_sf(data = master_transit_sf,
          aes(fill = bus_subway_ratio),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "Ratio",
                       na.value = "gray90",
                       trans = "log10") +
  geom_sf_text(data = master_transit_sf,
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "white", fontface = "bold") +
  labs(title = "Bus-to-Subway Ratio by Election District",
       subtitle = "Higher values = more bus-dependent (log scale)",
       caption = "Source: ACS 2018-2022 5-year estimates") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/transit/bus_dependency_map.png", map3,
       width = 10, height = 10, dpi = 300, bg = "white")

# Map 4: Strategic bus targeting (if strategic data available)
if ("strategic_type" %in% colnames(master_transit_sf)) {

  bus_target_sf <- master_transit_sf %>%
    mutate(
      bus_target_category = case_when(
        pct_bus >= 25 & strategic_type %in% c("HIGH OPPORTUNITY", "MEDIUM OPPORTUNITY") ~
          "🔴 High Bus + GOTV Target",
        pct_bus >= 25 & strategic_type == "SWING - PERSUADE" ~
          "🟡 High Bus + Swing",
        pct_bus >= 20 & strategic_type %in% c("HIGH OPPORTUNITY", "MEDIUM OPPORTUNITY", "SWING - PERSUADE") ~
          "🟠 Med Bus + Strategic",
        pct_bus >= 20 ~
          "🟢 Med Bus + Other",
        TRUE ~ "⚪ Lower Priority"
      )
    )

  target_colors <- c(
    "🔴 High Bus + GOTV Target" = "#E74C3C",
    "🟡 High Bus + Swing" = "#F39C12",
    "🟠 Med Bus + Strategic" = "#FF8C00",
    "🟢 Med Bus + Other" = "#27AE60",
    "⚪ Lower Priority" = "#BDC3C7"
  )

  map4 <- ggplot() +
    geom_sf(data = bus_target_sf,
            aes(fill = bus_target_category),
            color = "white", linewidth = 0.3) +
    scale_fill_manual(values = target_colors, name = NULL) +
    geom_sf_text(data = bus_target_sf,
                 aes(label = str_remove(ED, "^34")),
                 size = 2.5, color = "black", fontface = "bold") +
    labs(title = "Strategic Bus Campaign Targeting",
         subtitle = "'Fast & Free Buses' message priority by ED",
         caption = "High Bus = 25%+ bus ridership | Med Bus = 20-25%") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "bottom",
      legend.direction = "vertical"
    )

  ggsave("outputs/transit/strategic_bus_targeting.png", map4,
         width = 10, height = 12, dpi = 300, bg = "white")

  cat("  ✓ Created strategic bus targeting map\n")
}

cat("  ✓ Saved 3-4 maps to outputs/transit/\n\n")

# ============================================================
# SAVE SUMMARY REPORT
# ============================================================

cat("📝 Creating summary report...\n")

report_lines <- c(
  "# BUS & COMMUTE ANALYSIS - AD34",
  "## Campaign Resource Allocation Guide",
  "",
  sprintf("**Analysis Date:** %s", Sys.Date()),
  sprintf("**Total Workers Analyzed:** %s", format(round(summary_stats$total_workers), big.mark = ",")),
  "",
  "---",
  "",
  "## Key Findings",
  "",
  sprintf("- **%.1f%%** of AD34 workers commute by bus (%s people)",
          summary_stats$avg_bus_pct,
          format(round(summary_stats$bus_commuters), big.mark = ",")),
  sprintf("- **%.1f%%** use subway (%s people)",
          summary_stats$avg_subway_pct,
          format(round(summary_stats$subway_commuters), big.mark = ",")),
  sprintf("- **%.1f%%** use any public transit (%s people)",
          summary_stats$avg_transit_pct,
          format(round(summary_stats$transit_commuters), big.mark = ",")),
  sprintf("- Average commute time: **%.1f minutes**", summary_stats$avg_commute_time),
  ""
)

if ("strategic_type" %in% colnames(bus_target_eds)) {
  high_priority_count <- sum(bus_target_eds$high_bus & bus_target_eds$strategic_priority, na.rm = TRUE)
  report_lines <- c(report_lines,
    "",
    "## Strategic Targeting",
    "",
    sprintf("- **%d EDs** have high bus ridership (25%%+) AND strategic priority", high_priority_count),
    sprintf("- These EDs contain **%s potential voters**",
            format(round(total_bus_voters), big.mark = ",")),
    "- 'Fast & Free Buses' messaging should be PRIORITY in these districts",
    ""
  )
}

report_lines <- c(report_lines,
  "",
  "## Top Bus-Dependent EDs",
  "",
  "| ED | Bus % | Commute (min) | Mamdani % | Strategy |",
  "|---|---|---|---|---|"
)

top_eds <- head(high_bus, 10)
for (i in 1:nrow(top_eds)) {
  ed <- top_eds[i,]
  strat <- if ("strategic_type" %in% colnames(top_eds) && !is.na(ed$strategic_type)) ed$strategic_type else "N/A"
  report_lines <- c(report_lines,
    sprintf("| %s | %.1f%% | %.0f | %.1f%% | %s |",
            str_remove(ed$ED, "^34"),
            ed$pct_bus,
            ed$avg_commute_time,
            ed$Mamdani_pct,
            strat)
  )
}

report_lines <- c(report_lines,
  "",
  "## Recommendations",
  "",
  "### High-Priority Messaging EDs",
  ""
)

if ("strategic_type" %in% colnames(bus_target_eds)) {
  priority_eds <- bus_target_eds %>%
    filter(high_bus, strategic_priority) %>%
    arrange(desc(potential_voters)) %>%
    head(10)

  for (i in 1:nrow(priority_eds)) {
    ed <- priority_eds[i,]
    report_lines <- c(report_lines,
      sprintf("**ED %s**: %.1f%% bus ridership, %s, %s potential voters",
              str_remove(ed$ED, "^34"),
              ed$pct_bus,
              ed$strategic_type,
              format(round(ed$potential_voters), big.mark = ","))
    )
  }
}

report_lines <- c(report_lines,
  "",
  "### Campaign Tactics",
  "",
  "1. **Bus Stop Tabling**: Set up at high-traffic bus stops in priority EDs during rush hour",
  "2. **Bus Shelter Ads**: Place campaign materials at bus shelters in high-bus EDs",
  "3. **Commute Time Messaging**: Emphasize 'faster buses' in EDs with 40+ min commutes",
  "4. **Free Fare Messaging**: Lead with 'free buses' in lower-income, high-bus EDs",
  "5. **Door-Knocking Script**: Ask 'How do you get to work?' to identify bus riders",
  "",
  "---",
  "",
  "## Files Generated",
  "",
  "- `commute_by_ed.csv` - Full dataset with all commute variables by ED",
  "- `bus_ridership_map.png` - Map of bus ridership percentages",
  "- `commute_time_map.png` - Map of average commute times",
  "- `bus_dependency_map.png` - Map of bus-to-subway ratio",
  "- `strategic_bus_targeting.png` - Combined strategic targeting map",
  ""
)

writeLines(report_lines, "outputs/transit/BUS_ANALYSIS_SUMMARY.md")

cat("  ✓ Saved: BUS_ANALYSIS_SUMMARY.md\n\n")

cat("✅ BUS & COMMUTE ANALYSIS COMPLETE!\n\n")
cat("📂 Outputs saved to:\n")
cat("   - outputs/transit/\n")
cat("   - data/intermediate/transit/\n\n")
cat("🚌 Next steps:\n")
cat("   1. Download MTA GTFS data to map specific bus routes\n")
cat("   2. Identify which Q routes serve high-priority EDs\n")
cat("   3. Develop bus-stop-specific campaign materials\n\n")
