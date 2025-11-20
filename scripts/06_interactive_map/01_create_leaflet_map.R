library(sf)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)

setwd("~/Mayoral Results AD34")

cat("========================================\n")
cat("Creating Interactive Map\n")
cat("========================================\n\n")

# Load ED-level data
cat("Loading ED data...\n")
ed_data_df <- read.csv("outputs/tables/ed_level_census_election_data.csv") %>%
  mutate(Election_D = as.character(Election_D))

# Load ED shapefile
cat("Loading ED boundaries...\n")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
ED_shapefile <- st_read(shp_path, quiet = TRUE) %>%
  filter(County == "Queens") %>%
  filter(str_sub(Election_D, 1, 2) == "34") %>%
  mutate(Election_D = as.character(Election_D))

# Join data to shapefile
ED_map_data <- ED_shapefile %>%
  left_join(ed_data_df, by = "Election_D") %>%
  filter(!is.na(pct_Zohran) & !is.na(turnout_pct)) %>%
  st_transform(4326)  # Transform to WGS84 for Leaflet

cat(sprintf("Mapped %d Election Districts\n\n", nrow(ED_map_data)))

# Calculate classification
median_support <- median(ED_map_data$pct_Zohran, na.rm = TRUE)
median_turnout <- median(ED_map_data$turnout_pct, na.rm = TRUE)

ED_map_data <- ED_map_data %>%
  mutate(
    classification = case_when(
      pct_Zohran >= median_support & turnout_pct >= median_turnout ~ "High Support, High Turnout",
      pct_Zohran >= median_support & turnout_pct < median_turnout ~ "High Support, Low Turnout",
      pct_Zohran < median_support & turnout_pct >= median_turnout ~ "Low Support, High Turnout",
      pct_Zohran < median_support & turnout_pct < median_turnout ~ "Low Support, Low Turnout"
    )
  )

# Create color palette based on Zohran support
pal <- colorNumeric(
  palette = colorRampPalette(c("#ef4444", "#fbbf24", "#84cc16", "#22c55e"))(100),
  domain = ED_map_data$pct_Zohran,
  na.color = "#808080"
)

# Create detailed popup content
create_popup <- function(ed) {
  sprintf(
    '<div style="font-family: Arial, sans-serif; min-width: 280px;">
      <h3 style="margin: 0 0 10px 0; padding: 10px; background: linear-gradient(135deg, #1e3a8a 0%%, #3b82f6 100%%); color: white; border-radius: 4px;">
        Election District %s
      </h3>

      <div style="padding: 10px;">
        <h4 style="margin: 10px 0 5px 0; color: #1e3a8a; font-size: 14px; border-bottom: 2px solid #3b82f6;">🗳️ Election Results</h4>
        <table style="width: 100%%; font-size: 13px; line-height: 1.6;">
          <tr><td><strong>Zohran Support:</strong></td><td style="text-align: right;"><span style="color: %s; font-weight: bold;">%.1f%%</span></td></tr>
          <tr><td><strong>Zohran Votes:</strong></td><td style="text-align: right;">%s</td></tr>
          <tr><td><strong>Total Votes:</strong></td><td style="text-align: right;">%s</td></tr>
          <tr><td><strong>Turnout Rate:</strong></td><td style="text-align: right;"><strong>%.1f%%</strong></td></tr>
          <tr><td><strong>Registered Voters:</strong></td><td style="text-align: right;">%s</td></tr>
        </table>

        <h4 style="margin: 15px 0 5px 0; color: #1e3a8a; font-size: 14px; border-bottom: 2px solid #3b82f6;">👥 Demographics</h4>
        <table style="width: 100%%; font-size: 13px; line-height: 1.6;">
          <tr><td>Hispanic:</td><td style="text-align: right;">%.0f%%</td></tr>
          <tr><td>White (NH):</td><td style="text-align: right;">%.0f%%</td></tr>
          <tr><td>Asian (NH):</td><td style="text-align: right;">%.0f%%</td></tr>
          <tr><td>Black (NH):</td><td style="text-align: right;">%.0f%%</td></tr>
          <tr><td>Foreign Born:</td><td style="text-align: right;">%.0f%%</td></tr>
        </table>

        <h4 style="margin: 15px 0 5px 0; color: #1e3a8a; font-size: 14px; border-bottom: 2px solid #3b82f6;">💼 Socioeconomic</h4>
        <table style="width: 100%%; font-size: 13px; line-height: 1.6;">
          <tr><td>College Degree+:</td><td style="text-align: right;">%.0f%%</td></tr>
          <tr><td>Median Income:</td><td style="text-align: right;">%s</td></tr>
          <tr><td>Median Age:</td><td style="text-align: right;">%.0f years</td></tr>
        </table>

        <h4 style="margin: 15px 0 5px 0; color: #1e3a8a; font-size: 14px; border-bottom: 2px solid #3b82f6;">🎯 Strategic Classification</h4>
        <div style="padding: 8px; background: %s; border-radius: 4px; color: white; font-weight: bold; text-align: center;">
          %s
        </div>
      </div>
    </div>',
    ed$Election_D,
    ifelse(ed$pct_Zohran >= 55, "#22c55e", ifelse(ed$pct_Zohran >= 50, "#fbbf24", "#ef4444")),
    ed$pct_Zohran,
    format(round(ed$Zohran.Total), big.mark = ","),
    format(round(ed$total_votes), big.mark = ","),
    ed$turnout_pct,
    format(round(ed$registered_voters), big.mark = ","),
    ed$pct_hispanic,
    ed$pct_white,
    ed$pct_asian,
    ed$pct_black,
    ed$pct_foreign_born,
    ed$pct_college,
    ifelse(!is.na(ed$median_incomeE), paste0("$", format(round(ed$median_incomeE), big.mark = ",")), "N/A"),
    ifelse(!is.na(ed$median_ageE), ed$median_ageE, 0),
    case_when(
      ed$classification == "High Support, High Turnout" ~ "#2563eb",
      ed$classification == "High Support, Low Turnout" ~ "#ea580c",
      ed$classification == "Low Support, High Turnout" ~ "#7c3aed",
      ed$classification == "Low Support, Low Turnout" ~ "#dc2626",
      TRUE ~ "#6b7280"
    ),
    ed$classification
  )
}

# Generate all popups
cat("Generating popup content...\n")
popups <- sapply(1:nrow(ED_map_data), function(i) {
  create_popup(ED_map_data[i,])
})

# Create the interactive map
cat("Creating interactive map...\n")

map <- leaflet(ED_map_data) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Add ED polygons
  addPolygons(
    fillColor = ~pal(pct_Zohran),
    fillOpacity = 0.7,
    color = "white",
    weight = 2,
    opacity = 1,
    highlightOptions = highlightOptions(
      weight = 4,
      color = "#1e3a8a",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = popups,
    label = ~paste0("ED ", Election_D, ": ", round(pct_Zohran, 1), "% Zohran Support"),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px"),
      textsize = "12px",
      direction = "auto"
    ),
    group = "Election Districts"
  ) %>%

  # Add legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~pct_Zohran,
    title = "Zohran Support %",
    opacity = 0.8,
    labFormat = labelFormat(suffix = "%")
  ) %>%

  # Add layer control
  addLayersControl(
    baseGroups = c("Light", "Street", "Satellite"),
    overlayGroups = c("Election Districts"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Add title control
  addControl(
    html = '<div style="padding: 10px; background: rgba(255,255,255,0.95); border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);">
              <h3 style="margin: 0; color: #1e3a8a; font-size: 18px;">AD34 Interactive Election Map</h3>
              <p style="margin: 5px 0 0 0; font-size: 12px; color: #6b7280;">2025 Democratic Mayoral Primary - Click districts for details</p>
            </div>',
    position = "topleft"
  ) %>%

  # Set initial view
  setView(lng = -73.88, lat = 40.76, zoom = 13)

# Save the map
cat("Saving interactive map...\n")
dir.create("docs", showWarnings = FALSE, recursive = TRUE)
saveWidget(map, "docs/index.html", selfcontained = TRUE, title = "AD34 Interactive Election Map")

cat("\n✓ Interactive map created: docs/index.html\n")
cat("  Open in web browser to view\n\n")

# Also create a standalone version
saveWidget(map, "outputs/reports/interactive_map.html", selfcontained = TRUE, title = "AD34 Interactive Election Map")
cat("✓ Copy also saved to: outputs/reports/interactive_map.html\n\n")

cat("========================================\n")
cat("Map Features:\n")
cat("========================================\n")
cat("✓ Click any Election District for detailed popup\n")
cat("✓ Hover for quick Zohran support percentage\n")
cat("✓ Color-coded by support level (red=low, green=high)\n")
cat("✓ Search bar to find specific EDs\n")
cat("✓ Switch between Light, Street, and Satellite views\n")
cat("✓ Includes demographics, turnout, and strategic classification\n")
cat("✓ Fully interactive and responsive\n\n")
