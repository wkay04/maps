# Interactive Leaflet Map of Assembly District 34
# Creates an embeddable HTML map for GitHub Pages with address search

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

# Set working directory
setwd("~/Mayoral Results AD34")

# Census API key
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

cat("Loading Assembly District 34 boundary...\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")

# Transform to WGS84 (required for Leaflet)
ad34_wgs84 <- st_transform(ad34, 4326)

cat("Creating interactive map...\n")

# Create interactive Leaflet map with search functionality
map <- leaflet(ad34_wgs84) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
  addPolygons(
    fillColor = "#2E86AB",
    fillOpacity = 0.3,
    color = "#A23B72",
    weight = 3,
    opacity = 0.9,
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#A23B72",
      fillOpacity = 0.5,
      bringToFront = TRUE
    ),
    label = "NYC Assembly District 34",
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "14px",
      direction = "auto"
    ),
    popup = paste0(
      "<b>NYC Assembly District 34</b><br>",
      "Queens County<br>",
      "Communities: Jackson Heights, Corona, East Elmhurst, Astoria"
    )
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addControl(
    html = "<div style='background: white; padding: 10px; border-radius: 5px; border: 2px solid #A23B72;'>
            <h4 style='margin: 0 0 5px 0;'>NYC Assembly District 34</h4>
            <p style='margin: 0; font-size: 12px;'>Queens County</p>
            </div>",
    position = "topright"
  ) %>%
  # Add geocoding search using OpenStreetMap Nominatim (free, no API key needed)
  addSearchOSM(
    options = searchOptions(
      position = "topleft",
      zoom = 17,
      textPlaceholder = "Search for an address in Queens...",
      moveToLocation = TRUE,
      autoCollapse = TRUE,
      minLength = 3
    )
  ) %>%
  # Add reset map view button
  addResetMapButton()

# Ensure docs directory exists
dir.create("docs", showWarnings = FALSE, recursive = TRUE)

# Save the map
output_file <- "docs/ad34_map.html"
saveWidget(map, file = output_file, selfcontained = TRUE, title = "NYC Assembly District 34 Map")

cat("\nInteractive map saved to:", output_file, "\n")
cat("This map can be embedded in websites using an iframe\n")
cat("\nExample embed code:\n")
cat("<iframe src='https://YOUR-USERNAME.github.io/YOUR-REPO-NAME/ad34_map.html' ")
cat("width='100%' height='600' frameborder='0'></iframe>\n")
