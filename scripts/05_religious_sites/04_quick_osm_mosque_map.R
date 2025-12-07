library(sf)
library(leaflet)
library(htmlwidgets)
library(tigris)
library(dplyr)

setwd("~/Mayoral Results AD34")

cat("Creating quick OSM mosque map...\n")

# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)

# Download from Overpass API directly
bbox <- st_bbox(ad34)
bbox_str <- paste(bbox[2], bbox[1], bbox[4], bbox[3], sep = ",")

# Overpass query for mosques
query <- sprintf('[out:json][timeout:25];
(
  node["amenity"="place_of_worship"]["religion"="muslim"](%s);
  way["amenity"="place_of_worship"]["religion"="muslim"](%s);
  node["building"="mosque"](%s);
  way["building"="mosque"](%s);
);
out body;
>;
out skel qt;', bbox_str, bbox_str, bbox_str, bbox_str)

url <- "https://overpass-api.de/api/interpreter"
temp_file <- tempfile(fileext = ".osm")

# Download OSM data
cat("Downloading from Overpass API...\n")
result <- tryCatch({
  download.file(
    url = url,
    destfile = temp_file,
    method = "curl",
    quiet = TRUE,
    extra = paste0('--data "', gsub('"', '\\"', query), '"')
  )
  TRUE
}, error = function(e) {
  cat("Download failed, trying alternative method...\n")
  # Try with httr
  library(httr)
  response <- POST(url, body = query, encode = "form")
  writeBin(content(response, "raw"), temp_file)
  TRUE
})

# Read OSM data
cat("Reading OSM data...\n")
mosques <- st_read(temp_file, layer = "points", quiet = TRUE) %>%
  st_transform(4326) %>%
  filter(!is.na(st_coordinates(.)[,1]))

# Filter to AD34
mosques_in <- st_filter(mosques, ad34)

cat(sprintf("Found %d mosques in AD34\n", nrow(mosques_in)))

# Create map
map <- leaflet(mosques_in) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = ad34, fillOpacity = 0.1, color = "red", weight = 2) %>%
  addMarkers(
    popup = ~paste0("<b>", ifelse(is.na(name), "Islamic Center", name), "</b><br>",
                    ifelse(!is.na(addr.street), paste0(addr.street, "<br>"), "")),
    label = ~ifelse(is.na(name), "Islamic Center", name)
  ) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

saveWidget(map, "outputs/reports/mosques_osm_quick.html", selfcontained = TRUE)
cat("Saved to: outputs/reports/mosques_osm_quick.html\n")
