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
  addTiles(group = "Base Map", options = providerTileOptions(minZoom = 8, maxZoom = 20)) %>%
  addPolygons(
    fillColor = "#0095A0",
    fillOpacity = 0.5,
    color = "#FFAA3B",
    weight = 6,
    opacity = 0.9,
    group = "AD34 Boundary",
    highlightOptions = highlightOptions(
      weight = 6,
      color = "#901419",
      fillOpacity = 0.2,
      bringToFront = TRUE
    )
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addSearchOSM(
    options = searchOptions(
      position = "topleft",
      zoom = 18,
      textPlaceholder = "Search for a Queens/NYC address",
      moveToLocation = TRUE,
      autoCollapse = TRUE,
      minLength = 2,
      autoType = TRUE,
      delayType = 400,
      hideMarkerOnCollapse = FALSE,
      # Restrict search to New York with abbreviation handling
      url = "https://nominatim.openstreetmap.org/search?format=json&q={s}, Queens, New York&countrycodes=us&bounded=1&viewbox=-73.95,-73.70,40.68,40.82",
      # Use icon marker instead of circle
      marker = list(
        icon = makeIcon(
          iconUrl = "https://unpkg.com/leaflet@1.7.1/dist/images/marker-icon.png",
          iconWidth = 25,
          iconHeight = 41,
          iconAnchorX = 12,
          iconAnchorY = 41,
          shadowUrl = "https://unpkg.com/leaflet@1.7.1/dist/images/marker-shadow.png",
          shadowWidth = 41,
          shadowHeight = 41
        ),
        animate = TRUE,
        circle = list(radius = 0, weight = 0, color = "transparent", stroke = FALSE, fill = FALSE)
      )
    )
  ) %>%
  addResetMapButton() %>%
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;

      // Add CSS to hide circle markers from search
      var style = document.createElement('style');
      style.innerHTML = '.leaflet-marker-pane .leaflet-marker-icon.search-marker { display: none !important; } .search-circle-marker { display: none !important; }';
      document.head.appendChild(style);

      // Expand common abbreviations before search
      var searchInput = document.querySelector('.leaflet-control-search input');
      if (searchInput) {
        searchInput.addEventListener('input', function(e) {
          var val = this.value;
          // Expand abbreviations
          val = val.replace(/\\bave\\b/gi, 'avenue');
          val = val.replace(/\\bst\\b/gi, 'street');
          val = val.replace(/\\brd\\b/gi, 'road');
          val = val.replace(/\\bblvd\\b/gi, 'boulevard');
          val = val.replace(/\\bdr\\b/gi, 'drive');
          val = val.replace(/\\bln\\b/gi, 'lane');
          val = val.replace(/\\bpl\\b/gi, 'place');
          val = val.replace(/\\bct\\b/gi, 'court');
          if (val !== this.value) {
            this.value = val;
          }
        });
      }

      // Track custom marker
      var customSearchMarker = null;

      // Listen for when search completes
      map.on('search:locationfound', function(e) {
        // Remove previous custom marker
        if (customSearchMarker) {
          map.removeLayer(customSearchMarker);
        }

        // Hide all circle markers
        setTimeout(function() {
          map.eachLayer(function(layer) {
            if (layer instanceof L.Circle || layer instanceof L.CircleMarker) {
              layer.setStyle({ opacity: 0, fillOpacity: 0 });
            }
          });
        }, 10);

        // Add custom pin marker
        customSearchMarker = L.marker([e.latlng.lat, e.latlng.lng], {
          icon: L.icon({
            iconUrl: 'https://unpkg.com/leaflet@1.7.1/dist/images/marker-icon.png',
            iconSize: [25, 41],
            iconAnchor: [12, 41],
            popupAnchor: [1, -34],
            shadowUrl: 'https://unpkg.com/leaflet@1.7.1/dist/images/marker-shadow.png',
            shadowSize: [41, 41]
          }),
          zIndexOffset: 1000
        }).addTo(map);

        if (e.text) {
          customSearchMarker.bindPopup(e.text).openPopup();
        }
      });

      // Create layer groups for transit
      var subwayLayer = L.layerGroup();
      var busLayer = L.layerGroup();

      // Load subway lines from local MTA GTFS data
      console.log('Loading subway data...');
      fetch('transit_data/subway_lines.geojson')
        .then(response => {
          console.log('Subway response status:', response.status);
          if (!response.ok) throw new Error('HTTP error ' + response.status);
          return response.json();
        })
        .then(data => {
          console.log('Subway data loaded:', data.features ? data.features.length + ' features' : 'raw data');

          L.geoJSON(data, {
            style: function(feature) {
              return {
                color: '#FF6319',
                weight: 3,
                opacity: 0.8
              };
            },
            onEachFeature: function(feature, layer) {
              if (feature.properties) {
                var popupContent = '<b>Subway Line</b><br>';
                if (feature.properties.route_short_name) popupContent += 'Route: ' + feature.properties.route_short_name + '<br>';
                if (feature.properties.route_long_name) popupContent += feature.properties.route_long_name;
                layer.bindPopup(popupContent);
              }
            }
          }).addTo(subwayLayer);
          console.log('Subway layer created with', subwayLayer.getLayers().length, 'features');
        })
        .catch(err => console.error('Error loading subway data:', err));

      // Load bus routes from local MTA GTFS data
      console.log('Loading bus data...');
      fetch('transit_data/bus_routes.geojson')
        .then(response => {
          console.log('Bus response status:', response.status);
          if (!response.ok) throw new Error('HTTP error ' + response.status);
          return response.json();
        })
        .then(data => {
          console.log('Bus data loaded:', data.features ? data.features.length + ' features' : 'raw data');

          L.geoJSON(data, {
            style: function(feature) {
              return {
                color: '#0039A6',
                weight: 2,
                opacity: 0.7
              };
            },
            onEachFeature: function(feature, layer) {
              if (feature.properties) {
                var popupContent = '<b>Bus Route</b><br>';
                if (feature.properties.route_short_name) popupContent += 'Route: ' + feature.properties.route_short_name + '<br>';
                if (feature.properties.route_long_name) popupContent += feature.properties.route_long_name;
                layer.bindPopup(popupContent);
              }
            }
          }).addTo(busLayer);
          console.log('Bus layer created with', busLayer.getLayers().length, 'features');
        })
        .catch(err => console.error('Error loading bus data:', err));

      // Add layer control
      var overlays = {
        'Subway Lines': subwayLayer,
        'Bus Routes': busLayer
      };

      L.control.layers(null, overlays, {collapsed: false}).addTo(map);
    }
  ")
  

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
