library(sf)
library(tidyverse)
library(tigris)
library(leaflet)
library(htmlwidgets)
library(tidygeocoder)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("Creating Verified Mosque Map for AD34\n")
cat("========================================\n\n")

# Manually curated list of known mosques in the area
mosques_data <- tribble(
  ~name, ~address, ~phone, ~website,
  # Jackson Heights / East Elmhurst
  "Islamic Center of Jackson Heights (Masjid Abu Huraira)", "78-04 31st Ave, East Elmhurst, NY 11370", "(718) 424-8502", "http://icojh.com/",
  "Jackson Heights Islamic Center", "71-20 Roosevelt Ave, Jackson Heights, NY 11372", NA, NA,
  "Masjid Dar-ul-Furqan", "73-18 37th Ave, Jackson Heights, NY 11372", NA, NA,
  # Astoria - 31st Avenue area
  "Masjid Al-Hikmah", "48-01 31st Ave, Astoria, NY 11103", "(718) 721-8881", "https://masjidalhikmahnewyork.org/",
  "Masjid Tiba", "25-01 48th St, Astoria, NY 11103", NA, NA,
  # Astoria - other areas
  "Astoria Islamic Center (Masjid Baitul Mukarram)", "22-21 33rd St, Astoria, NY 11105", "(718) 204-7562", "www.astoriaislamiccenter.org",
  "MAS Queens Center", "46-01 20th Ave, Astoria, NY 11105", NA, "https://www.masqueens.org/",
  "Muslim Society of Queens - Dar Al-Dawah", "35-13 23rd Ave, Astoria, NY 11105", "(718) 274-2474", NA,
  "Alamin Jame Masjid", "35-19 36th Ave, Astoria, NY 11106", "(718) 729-6325", NA,
  "MAS Ibn Sina Center", "22-49 31st St, Astoria, NY 11105", NA, NA,
  "Muslim Ummah in America", "23-36 30th Ave, Astoria, NY 11102", NA, NA
)

cat("Geocoding mosque addresses...\n")
cat("This may take a moment...\n\n")

# Geocode addresses
mosques_geocoded <- mosques_data %>%
  geocode(address, method = "osm", lat = latitude, long = longitude)

# Check which ones were successfully geocoded
success_count <- sum(!is.na(mosques_geocoded$latitude))
cat(sprintf("Successfully geocoded %d out of %d addresses\n\n", success_count, nrow(mosques_geocoded)))

# Remove any that failed geocoding
mosques_geocoded <- mosques_geocoded %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Convert to sf object
mosques_sf <- st_as_sf(mosques_geocoded, coords = c("longitude", "latitude"), crs = 4326)

# Load AD34 boundary
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_4326 <- st_transform(ad34, 4326)

# Check which mosques are within AD34
cat("\nFiltering to AD34 boundaries...\n")
mosques_sf <- mosques_sf %>%
  mutate(in_ad34 = as.logical(st_within(geometry, ad34_4326, sparse = FALSE)))

# Print results
cat("\nMosques Found:\n")
cat("========================================\n")
for (i in 1:nrow(mosques_sf)) {
  coords <- st_coordinates(mosques_sf[i,])
  status <- ifelse(mosques_sf$in_ad34[i], "✓ IN AD34", "✗ Outside AD34")
  cat(sprintf("%d. %s [%s]\n", i, mosques_sf$name[i], status))
  cat(sprintf("   Address: %s\n", mosques_sf$address[i]))
  if (!is.na(mosques_sf$phone[i])) {
    cat(sprintf("   Phone: %s\n", mosques_sf$phone[i]))
  }
  if (!is.na(mosques_sf$website[i])) {
    cat(sprintf("   Website: %s\n", mosques_sf$website[i]))
  }
  cat(sprintf("   Coordinates: %.6f, %.6f\n\n", coords[1], coords[2]))
}

# Create popup content
create_popup <- function(mosque) {
  coords <- st_coordinates(mosque)
  gmaps_link <- sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", coords[2], coords[1])

  phone_html <- if (!is.na(mosque$phone)) {
    sprintf('<p style="margin: 5px 0;"><strong>Phone:</strong> <a href="tel:%s">%s</a></p>',
            gsub("[^0-9]", "", mosque$phone), mosque$phone)
  } else {
    ""
  }

  website_html <- if (!is.na(mosque$website)) {
    sprintf('<p style="margin: 5px 0;"><strong>Website:</strong> <a href="%s" target="_blank">%s</a></p>',
            mosque$website, mosque$website)
  } else {
    ""
  }

  status_color <- if (mosque$in_ad34) "#059669" else "#dc2626"
  status_text <- if (mosque$in_ad34) "Within AD34" else "Outside AD34"

  sprintf(
    '<div style="font-family: Arial, sans-serif; min-width: 280px;">
      <h3 style="margin: 0 0 10px 0; padding: 10px; background: linear-gradient(135deg, %s 0%%, %s 100%%); color: white; border-radius: 4px;">
        %s
      </h3>
      <div style="padding: 10px;">
        <p style="margin: 5px 0;"><strong>Address:</strong><br/>%s</p>
        %s
        %s
        <div style="margin: 10px 0; padding: 5px; background: %s; color: white; border-radius: 3px; text-align: center; font-size: 12px;">
          %s
        </div>
        <p style="margin: 10px 0 0 0;">
          <a href="%s" target="_blank" style="color: #059669; text-decoration: none; font-weight: bold;">
            → View on Google Maps
          </a>
        </p>
      </div>
    </div>',
    ifelse(mosque$in_ad34, "#059669", "#dc2626"),
    ifelse(mosque$in_ad34, "#10b981", "#ef4444"),
    mosque$name,
    mosque$address,
    phone_html,
    website_html,
    status_color,
    status_text,
    gmaps_link
  )
}

# Generate popups
popups <- sapply(1:nrow(mosques_sf), function(i) {
  create_popup(mosques_sf[i,])
})

# Create custom icons
mosque_icon_green <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41,
  popupAnchorX = 1, popupAnchorY = -34,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 41, shadowHeight = 41
)

mosque_icon_red <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41,
  popupAnchorX = 1, popupAnchorY = -34,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 41, shadowHeight = 41
)

# Split into two groups
mosques_in <- mosques_sf %>% filter(in_ad34)
mosques_out <- mosques_sf %>% filter(!in_ad34)

cat("\nCreating interactive map...\n")

# Create the map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Add AD34 boundary
  addPolygons(
    data = ad34_4326,
    fillColor = "#3b82f6",
    fillOpacity = 0.1,
    color = "#dc2626",
    weight = 3,
    opacity = 0.8,
    group = "AD34 Boundary",
    label = "Assembly District 34"
  )

# Add markers for mosques inside AD34
if (nrow(mosques_in) > 0) {
  popups_in <- sapply(1:nrow(mosques_in), function(i) create_popup(mosques_in[i,]))
  map <- map %>%
    addMarkers(
      data = mosques_in,
      icon = mosque_icon_green,
      popup = popups_in,
      label = ~name,
      labelOptions = labelOptions(
        style = list("font-weight" = "bold", "font-size" = "14px"),
        textsize = "14px",
        direction = "auto"
      ),
      group = "Mosques in AD34"
    )
}

# Add markers for mosques outside AD34
if (nrow(mosques_out) > 0) {
  popups_out <- sapply(1:nrow(mosques_out), function(i) create_popup(mosques_out[i,]))
  map <- map %>%
    addMarkers(
      data = mosques_out,
      icon = mosque_icon_red,
      popup = popups_out,
      label = ~name,
      labelOptions = labelOptions(
        style = list("font-weight" = "bold", "font-size" = "14px"),
        textsize = "14px",
        direction = "auto"
      ),
      group = "Mosques near AD34"
    )
}

# Finish the map
map <- map %>%
  # Add layer control
  addLayersControl(
    baseGroups = c("Light", "Street", "Satellite"),
    overlayGroups = c("AD34 Boundary", "Mosques in AD34", "Mosques near AD34"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Add legend
  addControl(
    html = '<div style="padding: 10px; background: rgba(255,255,255,0.95); border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);">
              <h3 style="margin: 0 0 10px 0; color: #059669; font-size: 18px;">Verified Mosques & Islamic Centers</h3>
              <p style="margin: 5px 0; font-size: 12px; color: #6b7280;">Assembly District 34, Queens</p>
              <div style="margin-top: 10px; font-size: 12px;">
                <div style="margin: 3px 0;"><span style="color: #059669;">●</span> Within AD34</div>
                <div style="margin: 3px 0;"><span style="color: #dc2626;">●</span> Nearby (outside AD34)</div>
              </div>
            </div>',
    position = "topleft"
  ) %>%

  # Fit bounds to show all mosques
  fitBounds(
    lng1 = min(st_coordinates(mosques_sf)[,1]) - 0.01,
    lat1 = min(st_coordinates(mosques_sf)[,2]) - 0.01,
    lng2 = max(st_coordinates(mosques_sf)[,1]) + 0.01,
    lat2 = max(st_coordinates(mosques_sf)[,2]) + 0.01
  )

# Save the map
cat("Saving map...\n")
dir.create("outputs/reports", showWarnings = FALSE, recursive = TRUE)
saveWidget(map, "outputs/reports/mosques_verified_map.html", selfcontained = TRUE,
           title = "AD34 Verified Mosques Map")

cat("\n✓ Map saved to: outputs/reports/mosques_verified_map.html\n")

# Save data table
mosques_table <- mosques_sf %>%
  st_drop_geometry() %>%
  select(name, address, phone, website, in_ad34) %>%
  arrange(desc(in_ad34), name)

write.csv(mosques_table, "outputs/tables/ad34_mosques_verified.csv", row.names = FALSE)
cat("✓ Data saved to: outputs/tables/ad34_mosques_verified.csv\n\n")

cat("========================================\n")
cat("Summary:\n")
cat("========================================\n")
cat(sprintf("Mosques within AD34: %d\n", sum(mosques_sf$in_ad34)))
cat(sprintf("Mosques nearby (outside AD34): %d\n", sum(!mosques_sf$in_ad34)))
cat(sprintf("Total mapped: %d\n\n", nrow(mosques_sf)))
