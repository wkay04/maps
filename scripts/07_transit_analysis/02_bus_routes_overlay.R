# ============================================================
# BUS ROUTES OVERLAY & ROUTE IDENTIFICATION
# ============================================================
# Downloads MTA GTFS data, overlays bus routes with AD34,
# identifies key routes serving bus-dependent EDs
# ============================================================

library(tidyverse)
library(sf)
library(tidytransit)  # For reading GTFS data
library(ggplot2)
library(viridis)

setwd("~/Mayoral Results AD34")
dir.create("data/raw/mta", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/transit/routes", showWarnings = FALSE, recursive = TRUE)

sf_use_s2(FALSE)

cat("\n🚌 BUS ROUTES OVERLAY ANALYSIS\n")
cat("==============================\n\n")

# ============================================================
# DOWNLOAD MTA GTFS DATA
# ============================================================

cat("📥 Downloading MTA GTFS data for Queens, Manhattan, and Bronx buses...\n")

# Install tidytransit if needed
if (!require(tidytransit, quietly = TRUE)) {
  cat("  📦 Installing tidytransit package...\n")
  install.packages("tidytransit", repos = "https://cloud.r-project.org", quiet = TRUE)
  library(tidytransit)
}

# Download borough-specific GTFS files
boroughs <- c("queens", "manhattan", "bronx", "brooklyn")
gtfs_list <- list()

for (boro in boroughs) {
  gtfs_url <- sprintf("http://web.mta.info/developers/data/nyct/bus/google_transit_%s.zip", boro)
  gtfs_zip <- sprintf("data/raw/mta/google_transit_%s.zip", boro)

  if (!file.exists(gtfs_zip)) {
    tryCatch({
      download.file(gtfs_url, gtfs_zip, mode = "wb", quiet = TRUE)
      cat("  ✓ Downloaded", boro, "GTFS\n")
    }, error = function(e) {
      cat("  ⚠️  Could not download", boro, "GTFS\n")
    })
  } else {
    cat("  ✓ Using cached", boro, "GTFS\n")
  }

  if (file.exists(gtfs_zip)) {
    gtfs_list[[boro]] <- read_gtfs(gtfs_zip)
  }
}

# Combine all GTFS data
cat("  📖 Combining GTFS data from all boroughs...\n")

gtfs <- list(
  routes = bind_rows(lapply(gtfs_list, function(x) x$routes)),
  stops = bind_rows(lapply(gtfs_list, function(x) x$stops)),
  stop_times = bind_rows(lapply(gtfs_list, function(x) x$stop_times)),
  trips = bind_rows(lapply(gtfs_list, function(x) x$trips)),
  shapes = bind_rows(lapply(gtfs_list, function(x) x$shapes))
) %>%
  # Remove duplicates that appear in multiple borough files
  lapply(function(df) {
    if ("route_id" %in% colnames(df)) {
      distinct(df, route_id, .keep_all = TRUE)
    } else if ("stop_id" %in% colnames(df)) {
      distinct(df, stop_id, .keep_all = TRUE)
    } else if ("trip_id" %in% colnames(df)) {
      distinct(df, trip_id, .keep_all = TRUE)
    } else if ("shape_id" %in% colnames(df) && "shape_pt_sequence" %in% colnames(df)) {
      distinct(df, shape_id, shape_pt_sequence, .keep_all = TRUE)
    } else {
      df
    }
  })

cat("  ✓ Loaded combined GTFS data\n")
cat("    Routes:", nrow(gtfs$routes), "\n")
cat("    Stops:", nrow(gtfs$stops), "\n")
cat("    Shapes:", length(unique(gtfs$shapes$shape_id)), "\n\n")

# ============================================================
# LOAD AD34 GEOGRAPHY & BUS DEPENDENCY DATA
# ============================================================

cat("📐 Loading AD34 boundaries and bus dependency data...\n")

# Load AD34 boundary
library(tigris)
options(tigris_use_cache = TRUE)
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_4326 <- st_transform(ad34, 4326)
ad34_3857 <- st_transform(ad34, 3857)

# Load bus dependency data
bus_data <- read_csv("data/intermediate/transit/commute_by_ed.csv",
                     col_types = cols(ED = col_character()),
                     show_col_types = FALSE)

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_eds <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(ED = Election_D) %>%
  st_transform(4326) %>%
  left_join(bus_data, by = "ED")

cat("  ✓ Loaded geography and bus data\n\n")

# ============================================================
# FILTER TO Q ROUTES & STOPS IN/NEAR AD34
# ============================================================

cat("🗺️  Filtering to all bus routes serving AD34...\n")

# Get all bus routes (filter out subway/rail)
# MTA uses route_type: 3 = bus, 1 = subway, 2 = rail
all_bus_routes <- gtfs$routes %>%
  filter(route_type == 3)

cat("  Found", nrow(all_bus_routes), "total bus routes\n")

# Convert stops to sf
stops_sf <- gtfs$stops %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Buffer AD34 by 2000m (2km) to catch all routes serving the district
ad34_buffer <- st_buffer(st_transform(ad34_4326, 3857), 2000) %>%
  st_transform(4326)

# Filter stops within/near AD34
stops_in_ad34 <- stops_sf %>%
  st_filter(ad34_buffer)

cat("  Found", nrow(stops_in_ad34), "bus stops in/near AD34\n")

# Get routes serving these stops
stop_times_filtered <- gtfs$stop_times %>%
  filter(stop_id %in% stops_in_ad34$stop_id)

trips_filtered <- gtfs$trips %>%
  filter(trip_id %in% stop_times_filtered$trip_id)

routes_in_ad34 <- all_bus_routes %>%
  filter(route_id %in% trips_filtered$route_id) %>%
  arrange(route_short_name)

cat("  ✓", nrow(routes_in_ad34), "bus routes serve AD34:\n")
cat("   ", paste(routes_in_ad34$route_short_name, collapse = ", "), "\n\n")

# ============================================================
# CREATE ROUTE SHAPES
# ============================================================

cat("🛤️  Creating route shape geometries...\n")

# Get shapes for routes in AD34
shapes_in_ad34 <- gtfs$shapes %>%
  filter(shape_id %in% trips_filtered$shape_id) %>%
  arrange(shape_id, shape_pt_sequence)

# Convert to sf linestrings
route_lines <- shapes_in_ad34 %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
  group_by(shape_id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

# Join route info
route_lines <- route_lines %>%
  left_join(
    trips_filtered %>% select(shape_id, route_id) %>% distinct(),
    by = "shape_id"
  ) %>%
  left_join(
    routes_in_ad34 %>% select(route_id, route_short_name, route_long_name),
    by = "route_id"
  ) %>%
  filter(!is.na(route_short_name))

cat("  ✓ Created", nrow(route_lines), "route shape geometries\n\n")

# ============================================================
# IDENTIFY KEY ROUTES BY ED
# ============================================================

cat("🎯 Identifying key routes serving high-bus-dependency EDs...\n")

# For each ED, find which routes have stops nearby (within 400m - about 2 blocks)
ed_buffer_400m <- st_buffer(st_transform(ad34_eds, 3857), 400) %>%
  st_transform(4326)

# Spatial join: which stops serve which EDs
stops_by_ed <- st_join(stops_in_ad34, ed_buffer_400m) %>%
  filter(!is.na(ED)) %>%
  st_drop_geometry()

# Get routes for each stop
stops_routes <- stop_times_filtered %>%
  left_join(trips_filtered %>% select(trip_id, route_id), by = "trip_id") %>%
  left_join(routes_in_ad34 %>% select(route_id, route_short_name), by = "route_id") %>%
  select(stop_id, route_short_name) %>%
  distinct()

# Join to get routes by ED
routes_by_ed <- stops_by_ed %>%
  left_join(stops_routes, by = "stop_id") %>%
  filter(!is.na(route_short_name)) %>%
  select(ED, route_short_name, pct_bus, strategic_type) %>%
  distinct() %>%
  arrange(ED, route_short_name)

# Summarize routes per ED
ed_route_summary <- routes_by_ed %>%
  group_by(ED, pct_bus, strategic_type) %>%
  summarise(
    routes = paste(sort(unique(route_short_name)), collapse = ", "),
    n_routes = n_distinct(route_short_name),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_bus))

cat("\n📊 TOP BUS-DEPENDENT EDs AND THEIR ROUTES:\n")
cat("===========================================\n\n")

top_bus_eds <- head(ed_route_summary, 15)
for (i in 1:nrow(top_bus_eds)) {
  ed <- top_bus_eds[i,]
  cat(sprintf("ED %s (%.1f%% bus, %s):\n",
              str_remove(ed$ED, "^34"),
              ed$pct_bus,
              ed$strategic_type))
  cat(sprintf("  Routes: %s\n\n", ed$routes))
}

# ============================================================
# PRIORITY ROUTES ANALYSIS
# ============================================================

cat("🚌 ANALYZING PRIORITY ROUTES...\n")
cat("================================\n\n")

# Weight routes by bus dependency of EDs they serve
route_priority <- routes_by_ed %>%
  group_by(route_short_name) %>%
  summarise(
    n_eds_served = n_distinct(ED),
    avg_bus_pct = mean(pct_bus, na.rm = TRUE),
    total_bus_exposure = sum(pct_bus, na.rm = TRUE),
    strategic_eds = sum(strategic_type %in%
                          c("HIGH OPPORTUNITY", "MEDIUM OPPORTUNITY",
                            "SWING - PERSUADE"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    priority_score = total_bus_exposure * (1 + strategic_eds)
  ) %>%
  arrange(desc(priority_score))

cat("🔴 TOP PRIORITY BUS ROUTES FOR CAMPAIGN:\n\n")

top_routes <- head(route_priority, 10)
for (i in 1:nrow(top_routes)) {
  route <- top_routes[i,]
  cat(sprintf("%d. Route %s\n", i, route$route_short_name))
  cat(sprintf("   - Serves %d EDs in AD34\n", route$n_eds_served))
  cat(sprintf("   - Average bus ridership: %.1f%%\n", route$avg_bus_pct))
  cat(sprintf("   - Serves %d strategic EDs\n", route$strategic_eds))
  cat(sprintf("   - Priority score: %.0f\n\n", route$priority_score))
}

# Save priority routes
write_csv(route_priority, "outputs/transit/routes/priority_routes_ranking.csv")
write_csv(ed_route_summary, "outputs/transit/routes/routes_by_ed.csv")

# ============================================================
# SPEED ANALYSIS (using GTFS stop times)
# ============================================================

cat("⚡ ANALYZING BUS SPEEDS...\n")
cat("==========================\n\n")

# Calculate average speeds for routes in AD34
# Speed = distance / time between stops

calculate_route_speeds <- function(route_name) {

  # Get trips for this route
  route_info <- routes_in_ad34 %>% filter(route_short_name == route_name)
  route_trips <- trips_filtered %>% filter(route_id == route_info$route_id)

  # Sample one trip
  sample_trip <- route_trips %>% slice(1)

  # Get stop times for this trip
  trip_stops <- stop_times_filtered %>%
    filter(trip_id == sample_trip$trip_id) %>%
    arrange(stop_sequence) %>%
    left_join(stops_sf %>% st_drop_geometry(), by = "stop_id")

  if (nrow(trip_stops) < 2) return(NULL)

  # Calculate time differences (in minutes)
  trip_stops <- trip_stops %>%
    mutate(
      arrival_sec = as.numeric(hms::as_hms(arrival_time)),
      departure_sec = as.numeric(hms::as_hms(departure_time))
    ) %>%
    arrange(stop_sequence) %>%
    mutate(
      time_to_next = lead(arrival_sec) - departure_sec,
      time_to_next_min = time_to_next / 60
    )

  # Get shape points to calculate distance
  trip_shape <- shapes_in_ad34 %>%
    filter(shape_id == sample_trip$shape_id)

  if (nrow(trip_shape) < 2) return(NULL)

  # Calculate total route distance (rough estimate)
  shape_line <- trip_shape %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
    st_transform(3857) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")

  total_distance_m <- st_length(shape_line) %>% as.numeric()
  total_distance_mi <- total_distance_m / 1609.34

  # Calculate total travel time
  total_time_min <- max(trip_stops$arrival_sec, na.rm = TRUE) -
                    min(trip_stops$departure_sec, na.rm = TRUE)
  total_time_min <- total_time_min / 60

  # Average speed
  avg_speed_mph <- if (total_time_min > 0) {
    (total_distance_mi / total_time_min) * 60
  } else {
    NA_real_
  }

  tibble(
    route = route_name,
    total_distance_mi = total_distance_mi,
    total_time_min = total_time_min,
    avg_speed_mph = avg_speed_mph,
    n_stops = nrow(trip_stops)
  )
}

# Calculate speeds for top priority routes
cat("  Calculating speeds for priority routes...\n")
speed_results <- map_dfr(head(top_routes$route_short_name, 10),
                         possibly(calculate_route_speeds, NULL))

if (nrow(speed_results) > 0) {
  speed_results <- speed_results %>%
    arrange(avg_speed_mph)

  cat("\n📊 BUS SPEED COMPARISON:\n\n")

  for (i in 1:nrow(speed_results)) {
    route <- speed_results[i,]
    cat(sprintf("Route %s:\n", route$route))
    cat(sprintf("  Average speed: %.1f mph\n", route$avg_speed_mph))
    cat(sprintf("  Distance: %.1f miles\n", route$total_distance_mi))
    cat(sprintf("  Travel time: %.0f minutes\n", route$total_time_min))
    cat(sprintf("  Stops: %d\n\n", route$n_stops))
  }

  write_csv(speed_results, "outputs/transit/routes/route_speeds.csv")
} else {
  cat("  ⚠️  Could not calculate speeds (insufficient data)\n\n")
}

# ============================================================
# CREATE VISUALIZATION
# ============================================================

cat("🗺️  Creating route overlay map...\n")

# Main map: Routes overlaid on bus dependency
map_routes <- ggplot() +
  # Bus dependency by ED
  geom_sf(data = ad34_eds,
          aes(fill = pct_bus),
          color = "white", linewidth = 0.2, alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", name = "Bus %",
                       na.value = "gray95") +

  # Bus routes
  geom_sf(data = route_lines,
          aes(color = route_short_name),
          linewidth = 1.2, alpha = 0.8) +
  scale_color_discrete(name = "Bus Route") +

  # Bus stops
  geom_sf(data = stops_in_ad34,
          color = "red", size = 0.5, alpha = 0.5) +

  # ED labels for high-bus areas
  geom_sf_text(data = filter(ad34_eds, pct_bus >= 10),
               aes(label = str_remove(ED, "^34")),
               size = 3, fontface = "bold", color = "black") +

  # AD34 boundary
  geom_sf(data = ad34_4326, fill = NA, color = "black", linewidth = 1) +

  labs(title = "Bus Routes & Ridership Patterns in AD34",
       subtitle = "Routes serving high bus-dependency Election Districts",
       caption = "Bus stops (red dots) | Routes colored by line | ED shading by % bus ridership") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

ggsave("outputs/transit/routes/bus_routes_overlay.png", map_routes,
       width = 14, height = 10, dpi = 300, bg = "white")

cat("  ✓ Saved bus_routes_overlay.png\n")

# Simplified map with top 5 routes only
top5_routes <- head(route_priority$route_short_name, 5)
route_lines_top5 <- route_lines %>%
  filter(route_short_name %in% top5_routes)

map_top5 <- ggplot() +
  geom_sf(data = ad34_eds,
          aes(fill = pct_bus),
          color = "white", linewidth = 0.3, alpha = 0.6) +
  scale_fill_viridis_c(option = "plasma", name = "Bus %",
                       na.value = "gray95") +
  geom_sf(data = route_lines_top5,
          aes(color = route_short_name),
          linewidth = 2, alpha = 0.9) +
  scale_color_brewer(palette = "Set1", name = "Priority Routes") +
  geom_sf_text(data = filter(ad34_eds, pct_bus >= 12),
               aes(label = str_remove(ED, "^34")),
               size = 4, fontface = "bold", color = "black") +
  geom_sf(data = ad34_4326, fill = NA, color = "black", linewidth = 1.5) +
  labs(title = "Top 5 Priority Bus Routes for Campaign",
       subtitle = "Focus messaging on these routes serving high-bus-dependency EDs",
       caption = "Based on ridership, strategic ED overlap, and route coverage") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )

ggsave("outputs/transit/routes/top5_priority_routes.png", map_top5,
       width = 12, height = 10, dpi = 300, bg = "white")

cat("  ✓ Saved top5_priority_routes.png\n\n")

# ============================================================
# GENERATE SUMMARY REPORT
# ============================================================

cat("📝 Creating summary report...\n")

report_lines <- c(
  "# BUS ROUTES OVERLAY ANALYSIS - AD34",
  "## Campaign Messaging Guide",
  "",
  sprintf("**Analysis Date:** %s", Sys.Date()),
  sprintf("**Routes Analyzed:** %d Q routes serving AD34", nrow(routes_in_ad34)),
  sprintf("**Bus Stops in District:** %d", nrow(stops_in_ad34)),
  "",
  "---",
  "",
  "## Top Priority Routes",
  "",
  "### Campaign Should Focus On:",
  ""
)

for (i in 1:min(5, nrow(top_routes))) {
  route <- top_routes[i,]
  if (!is.na(route$route_short_name)) {
    report_lines <- c(report_lines,
      sprintf("### %d. **Route %s**", i, route$route_short_name),
      sprintf("- **Serves %d EDs** in AD34", route$n_eds_served),
      sprintf("- **Average bus ridership:** %.1f%%", route$avg_bus_pct),
      sprintf("- **Strategic EDs served:** %d", route$strategic_eds),
      sprintf("- **Priority score:** %.0f", route$priority_score),
      ""
    )

    # Add speed info if available
    if (nrow(speed_results) > 0) {
      speed_info <- speed_results %>% filter(route == route$route_short_name)
      if (nrow(speed_info) > 0) {
        report_lines <- c(report_lines,
          sprintf("**Performance:**"),
          sprintf("- Average speed: %.1f mph", speed_info$avg_speed_mph),
          sprintf("- Total route: %.1f miles", speed_info$total_distance_mi),
          sprintf("- Travel time: %.0f minutes", speed_info$total_time_min),
          ""
        )
      }
    }
  }
}

report_lines <- c(report_lines,
  "",
  "---",
  "",
  "## High Bus-Dependency EDs and Their Routes",
  ""
)

for (i in 1:min(10, nrow(top_bus_eds))) {
  ed <- top_bus_eds[i,]
  report_lines <- c(report_lines,
    sprintf("**ED %s** (%.1f%% bus, %s):",
            str_remove(ed$ED, "^34"),
            ed$pct_bus,
            ed$strategic_type),
    sprintf("- Routes: %s", ed$routes),
    ""
  )
}

report_lines <- c(report_lines,
  "",
  "---",
  "",
  "## Campaign Tactics by Route",
  "",
  "### Bus Stop Tabling Locations",
  "",
  "Set up campaign tables at high-traffic stops on priority routes:",
  ""
)

# Add top 3 routes
for (i in 1:min(3, nrow(top_routes))) {
  report_lines <- c(report_lines,
    sprintf("**Route %s:** Target stops in EDs with highest bus ridership",
            top_routes$route_short_name[i]),
    ""
  )
}

report_lines <- c(report_lines,
  "",
  "### Bus Shelter Advertising",
  "",
  "Place campaign materials at shelters along these routes during commute hours (7-9 AM, 5-7 PM)",
  "",
  "### Speed & Reliability Messaging",
  "",
  "- **'Faster Buses'**: Average bus speed is ~10-15 mph due to frequent stops and traffic",
  "- **'More Reliable'**: Emphasize dedicated bus lanes and signal priority",
  "- **'More Frequent'**: Reduce wait times with increased service",
  "",
  "---",
  "",
  "## Files Generated",
  "",
  "- `bus_routes_overlay.png` - All routes overlaid on bus dependency map",
  "- `top5_priority_routes.png` - Simplified map with top 5 priority routes",
  "- `priority_routes_ranking.csv` - Full ranking of routes by priority score",
  "- `routes_by_ed.csv` - Which routes serve which EDs",
  "- `route_speeds.csv` - Speed analysis for priority routes",
  ""
)

writeLines(report_lines, "outputs/transit/routes/ROUTES_ANALYSIS_SUMMARY.md")

cat("  ✓ Saved: ROUTES_ANALYSIS_SUMMARY.md\n\n")

cat("✅ BUS ROUTES OVERLAY COMPLETE!\n\n")
cat("📂 Outputs saved to outputs/transit/routes/\n\n")
cat("🎯 Key Findings:\n")
cat(sprintf("   - %d Q routes serve AD34\n", nrow(routes_in_ad34)))
cat(sprintf("   - Top route: %s (serves %d EDs)\n",
            top_routes$route_short_name[1],
            top_routes$n_eds_served[1]))
cat("   - Focus campaign on routes serving high-bus + strategic EDs\n\n")
