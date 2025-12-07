library(sf)
library(osmdata)
library(tidyverse)

# Test query for a specific known mosque location
# Let's search around Corona/Jackson Heights area more broadly

# Wider bbox for Queens
bbox <- c(-73.93, 40.74, -73.87, 40.78)

cat("Testing OSM query with wider bbox...\n")
cat("Bbox:", bbox, "\n\n")

# Query 1: amenity=place_of_worship + religion=muslim
cat("Query 1: place_of_worship + muslim\n")
q1 <- opq(bbox = bbox) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")

result1 <- osmdata_sf(q1)
cat("Points:", ifelse(is.null(result1$osm_points), 0, nrow(result1$osm_points)), "\n")
cat("Polygons:", ifelse(is.null(result1$osm_polygons), 0, nrow(result1$osm_polygons)), "\n\n")

if (!is.null(result1$osm_points) && nrow(result1$osm_points) > 0) {
  cat("Sample names:\n")
  print(head(result1$osm_points$name, 10))
}

# Query 2: Just search for "mosque" in name
cat("\n\nQuery 2: Searching for 'mosque' in name field\n")
q2 <- opq(bbox = bbox) %>%
  add_osm_feature(key = "name", value = "mosque", value_exact = FALSE)

result2 <- osmdata_sf(q2)
cat("Points:", ifelse(is.null(result2$osm_points), 0, nrow(result2$osm_points)), "\n")
cat("Polygons:", ifelse(is.null(result2$osm_polygons), 0, nrow(result2$osm_polygons)), "\n\n")

if (!is.null(result2$osm_points) && nrow(result2$osm_points) > 0) {
  cat("Sample names:\n")
  print(head(result2$osm_points$name, 10))
}

# Query 3: building=mosque
cat("\n\nQuery 3: building=mosque\n")
q3 <- opq(bbox = bbox) %>%
  add_osm_feature(key = "building", value = "mosque")

result3 <- osmdata_sf(q3)
cat("Points:", ifelse(is.null(result3$osm_points), 0, nrow(result3$osm_points)), "\n")
cat("Polygons:", ifelse(is.null(result3$osm_polygons), 0, nrow(result3$osm_polygons)), "\n\n")

