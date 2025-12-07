library(sf)
library(osmdata)
library(leaflet)
library(htmlwidgets)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("Getting mosques from OSM...\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)
bbox <- st_bbox(ad34)

# Query OSM
q <- opq(bbox) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")

osm_data <- osmdata_sf(q)

# Combine points and polygons
mosques <- bind_rows(
  osm_data$osm_points,
  if(!is.null(osm_data$osm_polygons)) st_centroid(osm_data$osm_polygons) else NULL
) %>% st_transform(4326)

# Filter to AD34
mosques <- st_filter(mosques, ad34)

cat(sprintf("Found %d locations\n", nrow(mosques)))

# Create popups
mosques$popup <- paste0(
  "<b>", ifelse(is.na(mosques$name), "Islamic Center", mosques$name), "</b><br>",
  ifelse(!is.na(mosques$`addr:street`), paste0(mosques$`addr:street`, "<br>"), ""),
  ifelse(!is.na(mosques$phone), paste0("Phone: ", mosques$phone, "<br>"), "")
)

# Create map
map <- leaflet(mosques) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
  addPolygons(data = ad34, fillOpacity = 0.1, fillColor = "blue",
              color = "red", weight = 3, group = "AD34") %>%
  addMarkers(
    popup = ~popup,
    label = ~ifelse(is.na(name), "Islamic Center", name),
    clusterOptions = NULL
  ) %>%
  addLayersControl(
    baseGroups = c("Light", "Street"),
    overlayGroups = "AD34"
  ) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

dir.create("outputs/reports", showWarnings = FALSE, recursive = TRUE)
saveWidget(map, "outputs/reports/mosques_map.html", selfcontained = TRUE)
cat("✓ Saved: outputs/reports/mosques_map.html\n")
