library(sf)
library(tidyverse)
library(osmdata)
library(tigris)
library(leaflet)
library(htmlwidgets)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("Creating Interactive Mosque Map for AD34\n")
cat("========================================\n\n")

# Load AD34 boundary
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_4326 <- st_transform(ad34, 4326)

# Get bounding box for AD34 with some buffer
bbox <- st_bbox(ad34_4326)
bbox_buffered <- bbox + c(-0.01, -0.01, 0.01, 0.01)  # Add buffer

cat("\nQuerying OpenStreetMap for Islamic places of worship...\n")
cat("This may take a moment...\n\n")

# Query OSM for mosques and Islamic centers
# Method 1: Direct query for mosques
osm_query <- opq(bbox = bbox_buffered) %>%
  add_osm_feature(key = "amenity", value = "place_of_worship") %>%
  add_osm_feature(key = "religion", value = "muslim")

mosques_data <- osmdata_sf(osm_query)

# Get all mosque data
all_mosques <- list()

# Points
if (!is.null(mosques_data$osm_points) && nrow(mosques_data$osm_points) > 0) {
  all_mosques$points <- mosques_data$osm_points
  cat(sprintf("Found %d mosque points from query 1\n", nrow(mosques_data$osm_points)))
}

# Polygons (convert to points)
if (!is.null(mosques_data$osm_polygons) && nrow(mosques_data$osm_polygons) > 0) {
  all_mosques$polygons <- st_centroid(mosques_data$osm_polygons)
  cat(sprintf("Found %d mosque polygons from query 1\n", nrow(mosques_data$osm_polygons)))
}

# Method 2: Also search for buildings tagged as mosque
cat("\nSearching for buildings tagged as mosques...\n")
osm_query2 <- opq(bbox = bbox_buffered) %>%
  add_osm_feature(key = "building", value = "mosque")

mosques_data2 <- osmdata_sf(osm_query2)

if (!is.null(mosques_data2$osm_points) && nrow(mosques_data2$osm_points) > 0) {
  all_mosques$building_points <- mosques_data2$osm_points
  cat(sprintf("Found %d mosque building points\n", nrow(mosques_data2$osm_points)))
}

if (!is.null(mosques_data2$osm_polygons) && nrow(mosques_data2$osm_polygons) > 0) {
  all_mosques$building_polygons <- st_centroid(mosques_data2$osm_polygons)
  cat(sprintf("Found %d mosque building polygons\n", nrow(mosques_data2$osm_polygons)))
}

# Combine all results
if (length(all_mosques) > 0) {
  # Find common columns
  all_names <- unique(unlist(lapply(all_mosques, names)))

  # Standardize all dataframes to have same columns
  all_mosques_std <- lapply(all_mosques, function(df) {
    missing_cols <- setdiff(all_names, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df %>% select(all_of(all_names))
  })

  mosques_combined <- bind_rows(all_mosques_std)

  # Remove duplicates based on location
  mosques_combined <- mosques_combined %>%
    distinct(geometry, .keep_all = TRUE)

} else {
  stop("No mosques found in the area")
}

# Ensure CRS is WGS84
mosques_combined <- st_transform(mosques_combined, 4326)

# Filter to only those within AD34 (strict boundary)
cat("\nFiltering to AD34 boundaries...\n")
mosques_in_ad34 <- st_filter(mosques_combined, ad34_4326, .predicate = st_within)

# Also check those very close to boundary (within 50 meters)
mosques_near <- st_filter(
  mosques_combined,
  st_buffer(ad34_4326, dist = 0.0005),
  .predicate = st_within
)

# Combine and remove duplicates
mosques_final <- bind_rows(mosques_in_ad34, mosques_near) %>%
  distinct(geometry, .keep_all = TRUE)

cat(sprintf("\n✓ Found %d Islamic centers/mosques in/near AD34\n\n", nrow(mosques_final)))

# Print details
if (nrow(mosques_final) > 0) {
  cat("Islamic Centers Found:\n")
  cat("========================================\n")
  for (i in 1:nrow(mosques_final)) {
    name <- ifelse(!is.na(mosques_final$name[i]), mosques_final$name[i], "Unnamed Islamic Center")
    cat(sprintf("%d. %s\n", i, name))

    # Get coordinates
    coords <- st_coordinates(mosques_final[i,])
    cat(sprintf("   Location: %.6f, %.6f\n", coords[1], coords[2]))

    # Address if available
    if ("addr:full" %in% names(mosques_final) && !is.na(mosques_final$`addr:full`[i])) {
      cat(sprintf("   Address: %s\n", mosques_final$`addr:full`[i]))
    } else {
      if ("addr:street" %in% names(mosques_final) && !is.na(mosques_final$`addr:street`[i])) {
        addr <- mosques_final$`addr:street`[i]
        if ("addr:housenumber" %in% names(mosques_final) && !is.na(mosques_final$`addr:housenumber`[i])) {
          addr <- paste(mosques_final$`addr:housenumber`[i], addr)
        }
        cat(sprintf("   Address: %s\n", addr))
      }
    }

    # Phone if available
    if ("phone" %in% names(mosques_final) && !is.na(mosques_final$phone[i])) {
      cat(sprintf("   Phone: %s\n", mosques_final$phone[i]))
    }

    cat("\n")
  }
}

# Create interactive popup content
create_popup <- function(mosque) {
  name <- ifelse(!is.na(mosque$name), mosque$name, "Unnamed Islamic Center")

  # Get address
  address <- "Address not available"
  if ("addr:full" %in% names(mosque) && !is.na(mosque$`addr:full`)) {
    address <- mosque$`addr:full`
  } else if ("addr:street" %in% names(mosque) && !is.na(mosque$`addr:street`)) {
    address <- mosque$`addr:street`
    if ("addr:housenumber" %in% names(mosque) && !is.na(mosque$`addr:housenumber`)) {
      address <- paste(mosque$`addr:housenumber`, address)
    }
  }

  # Get phone
  phone <- if ("phone" %in% names(mosque) && !is.na(mosque$phone)) mosque$phone else "Not available"

  # Get denomination
  denom <- if ("denomination" %in% names(mosque) && !is.na(mosque$denomination)) mosque$denomination else ""

  # Get coordinates for Google Maps link
  coords <- st_coordinates(mosque)
  gmaps_link <- sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", coords[2], coords[1])

  sprintf(
    '<div style="font-family: Arial, sans-serif; min-width: 250px;">
      <h3 style="margin: 0 0 10px 0; padding: 10px; background: linear-gradient(135deg, #059669 0%%, #10b981 100%%); color: white; border-radius: 4px;">
        %s
      </h3>
      <div style="padding: 10px;">
        <p style="margin: 5px 0;"><strong>Address:</strong><br/>%s</p>
        <p style="margin: 5px 0;"><strong>Phone:</strong> %s</p>
        %s
        <p style="margin: 10px 0 0 0;">
          <a href="%s" target="_blank" style="color: #059669; text-decoration: none; font-weight: bold;">
            → View on Google Maps
          </a>
        </p>
      </div>
    </div>',
    name,
    address,
    phone,
    if (denom != "") sprintf('<p style="margin: 5px 0;"><strong>Denomination:</strong> %s</p>', denom) else "",
    gmaps_link
  )
}

# Generate popups
cat("Creating interactive map...\n")
popups <- sapply(1:nrow(mosques_final), function(i) {
  create_popup(mosques_final[i,])
})

# Create custom icon for mosques
mosque_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41,
  popupAnchorX = 1, popupAnchorY = -34,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 41, shadowHeight = 41
)

# Create the map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Add AD34 boundary
  addPolygons(
    data = ad34_4326,
    fillColor = "transparent",
    fillOpacity = 0.1,
    color = "#dc2626",
    weight = 3,
    opacity = 0.8,
    group = "AD34 Boundary"
  ) %>%

  # Add mosque markers
  addMarkers(
    data = mosques_final,
    icon = mosque_icon,
    popup = popups,
    label = ~ifelse(!is.na(name), name, "Islamic Center"),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "14px"),
      textsize = "14px",
      direction = "auto"
    ),
    group = "Mosques"
  ) %>%

  # Add layer control
  addLayersControl(
    baseGroups = c("Light", "Street", "Satellite"),
    overlayGroups = c("AD34 Boundary", "Mosques"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Add title
  addControl(
    html = '<div style="padding: 10px; background: rgba(255,255,255,0.95); border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);">
              <h3 style="margin: 0; color: #059669; font-size: 18px;">Islamic Centers & Mosques in AD34</h3>
              <p style="margin: 5px 0 0 0; font-size: 12px; color: #6b7280;">Data from OpenStreetMap - Click markers for details</p>
            </div>',
    position = "topleft"
  ) %>%

  # Set view to center of mosques
  fitBounds(
    lng1 = bbox[1], lat1 = bbox[2],
    lng2 = bbox[3], lat2 = bbox[4]
  )

# Save the map
cat("Saving interactive map...\n")
dir.create("outputs/reports", showWarnings = FALSE, recursive = TRUE)
saveWidget(map, "outputs/reports/mosques_interactive_map.html", selfcontained = TRUE, title = "AD34 Mosques Map")

# Also save to docs for easy viewing
dir.create("docs", showWarnings = FALSE, recursive = TRUE)
saveWidget(map, "docs/mosques.html", selfcontained = TRUE, title = "AD34 Mosques Map")

cat("\n✓ Interactive map saved to:\n")
cat("  - outputs/reports/mosques_interactive_map.html\n")
cat("  - docs/mosques.html\n\n")

cat("========================================\n")
cat("Map Features:\n")
cat("========================================\n")
cat("✓ Fully zoomable and pannable\n")
cat("✓ Click markers for detailed information\n")
cat("✓ Google Maps links for directions\n")
cat("✓ Toggle between map layers (Light, Street, Satellite)\n")
cat("✓ Show/hide AD34 boundary and mosque markers\n")
cat("✓ Hover over markers to see names\n\n")
