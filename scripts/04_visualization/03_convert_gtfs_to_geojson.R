# Convert MTA GTFS shapes to GeoJSON
# This script processes GTFS transit data and creates GeoJSON files for subway and bus routes

library(tidyverse)
library(sf)

# Set working directory
setwd("~/Mayoral Results AD34")

cat("Reading GTFS data...\n")

# Read GTFS files
shapes <- read_csv("data/raw/transit/shapes.txt", show_col_types = FALSE)
trips <- read_csv("data/raw/transit/trips.txt", show_col_types = FALSE)
routes <- read_csv("data/raw/transit/routes.txt", show_col_types = FALSE)

cat("Processing", nrow(shapes), "shape points\n")
cat("Found", length(unique(shapes$shape_id)), "unique shapes\n")

# Join trips with routes to get route info for each shape
trip_routes <- trips %>%
  left_join(routes, by = "route_id") %>%
  select(shape_id, route_id, route_short_name, route_long_name, route_type, route_color) %>%
  distinct()

# Filter subway routes (route_type == 1)
subway_shapes <- trip_routes %>%
  filter(route_type == 1) %>%
  pull(shape_id) %>%
  unique()

# Filter bus routes (route_type == 3)
bus_shapes <- trip_routes %>%
  filter(route_type == 3) %>%
  pull(shape_id) %>%
  unique()

cat("Found", length(subway_shapes), "subway shapes\n")
cat("Found", length(bus_shapes), "bus shapes\n")

# Function to convert shapes to sf object
convert_shapes_to_sf <- function(shape_ids, route_info) {
  # Filter shapes
  filtered_shapes <- shapes %>%
    filter(shape_id %in% shape_ids) %>%
    arrange(shape_id, shape_pt_sequence)

  # Convert to sf linestrings
  lines_sf <- filtered_shapes %>%
    group_by(shape_id) %>%
    summarise(
      geometry = st_linestring(cbind(shape_pt_lon, shape_pt_lat)) %>% st_sfc(),
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 4326)

  # Join with route info
  lines_with_info <- lines_sf %>%
    left_join(route_info, by = "shape_id")

  return(lines_with_info)
}

# Create subway GeoJSON
cat("\nCreating subway GeoJSON...\n")
subway_route_info <- trip_routes %>%
  filter(shape_id %in% subway_shapes) %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup()

subway_sf <- convert_shapes_to_sf(subway_shapes, subway_route_info)

# Save subway GeoJSON
st_write(subway_sf, "data/raw/transit/subway_lines.geojson", delete_dsn = TRUE)
cat("Saved subway lines to data/raw/transit/subway_lines.geojson\n")
cat("Subway lines:", nrow(subway_sf), "features\n")

# Create bus GeoJSON (limit to reasonable number)
cat("\nCreating bus GeoJSON...\n")
bus_route_info <- trip_routes %>%
  filter(shape_id %in% bus_shapes) %>%
  group_by(shape_id) %>%
  slice(1) %>%
  ungroup()

# Get only Queens/Astoria/Corona area bus routes (major routes)
major_queens_buses <- c("Q03", "Q18", "Q19", "Q33", "Q47", "Q48", "Q49", "Q66", "Q69", "Q70", "Q72")

bus_route_info_filtered <- bus_route_info %>%
  filter(route_short_name %in% major_queens_buses)

cat("Filtering to", nrow(bus_route_info_filtered), "major Queens bus routes\n")

bus_sf <- convert_shapes_to_sf(
  bus_route_info_filtered$shape_id,
  bus_route_info_filtered
)

# Save bus GeoJSON
st_write(bus_sf, "data/raw/transit/bus_routes.geojson", delete_dsn = TRUE)
cat("Saved bus routes to data/raw/transit/bus_routes.geojson\n")
cat("Bus routes:", nrow(bus_sf), "features\n")

cat("\nDone! Transit GeoJSON files created successfully.\n")
