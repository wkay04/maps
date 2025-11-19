# ============================================================
# Interactive Election District Map - AD34
# ============================================================
# Creates interactive map with voter data by ED for micro-targeting

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🗺️  Interactive ED Map - AD34\n")
cat("==============================\n\n")

# ============================================================
# LOAD ED SHAPEFILES
# ============================================================

cat("📂 Loading Election District shapefiles...\n")

# Find shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"

ed_shp <- st_read(shp_path, quiet = TRUE)

# Filter for AD34 (Queens)
ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,  # Already in format "34001"
    ED_num = as.numeric(str_sub(Election_D, 3, -1))  # Extract just the last 3 digits
  )

cat("  ✓ Loaded", nrow(ad34_shp), "Election Districts in AD34\n\n")

# ============================================================
# LOAD TURNOUT DATA
# ============================================================

cat("📊 Loading turnout data...\n")

turnout_2025 <- read_csv("data/intermediate/ad34_turnout_2025_clean.csv",
                         col_types = cols(ED = col_character()),
                         show_col_types = FALSE)

cat("  ✓ Loaded turnout for", nrow(turnout_2025), "EDs\n\n")

# ============================================================
# LOAD 2025 MAYORAL RESULTS BY CANDIDATE
# ============================================================

cat("📊 Loading 2025 mayoral results by candidate...\n")

# Load re-parsed mayor data with Cuomo
candidate_data <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                           col_types = cols(ED = col_character()),
                           show_col_types = FALSE) %>%
  rename(
    mayor_total_votes = total_votes,
    Zohran_Mamdani = Zohran_Mamdani,
    Andrew_Cuomo = Andrew_Cuomo,
    Curtis_Sliwa = Curtis_Sliwa,
    Irene_Estrada = Irene_Estrada,
    Eric_Adams = Eric_Adams
  )

cat("  ✓ Loaded results for", nrow(candidate_data), "EDs\n\n")

# ============================================================
# MERGE DATA
# ============================================================

cat("🔗 Merging spatial and electoral data...\n")

# Join all data
ed_map_data <- ad34_shp %>%
  left_join(turnout_2025, by = "ED") %>%
  left_join(candidate_data, by = "ED") %>%
  st_transform(4326)  # Transform to WGS84 for leaflet

cat("  ✓ Merged data for", nrow(ed_map_data), "EDs\n\n")

# ============================================================
# CREATE INTERACTIVE MAP
# ============================================================

cat("🗺️  Creating interactive map...\n")

# Create color palette based on winner
winner_colors <- c("Mamdani" = "#2E86AB",   # Blue
                   "Cuomo" = "#9B59B6",     # Purple
                   "Sliwa" = "#C1121F",     # Red
                   "Estrada" = "#F77F00",   # Orange
                   "Adams" = "#06A77D")     # Green

# Color function
color_pal <- colorFactor(
  palette = winner_colors,
  domain = ed_map_data$winner
)

# Create popup and label content
ed_map_data <- ed_map_data %>%
  mutate(
    winner_pct = case_when(
      winner == "Mamdani" ~ Mamdani_pct,
      winner == "Cuomo" ~ Cuomo_pct,
      winner == "Sliwa" ~ Sliwa_pct,
      winner == "Estrada" ~ Estrada_pct,
      TRUE ~ Adams_pct
    ),
    popup_text = paste0(
      "<b>Election District ", ED_num, "</b><br>",
      "<hr>",
      "<b>2025 Mayoral Results:</b><br>",
      "Total Votes: ", comma(mayor_total_votes), "<br>",
      "<br><b>Candidates:</b><br>",
      "🔵 Zohran Mamdani: ", comma(Zohran_Mamdani), " (", Mamdani_pct, "%)<br>",
      "🟣 Andrew Cuomo: ", comma(Andrew_Cuomo), " (", Cuomo_pct, "%)<br>",
      "🔴 Curtis Sliwa: ", comma(Curtis_Sliwa), " (", Sliwa_pct, "%)<br>",
      "🟠 Irene Estrada: ", comma(Irene_Estrada), " (", Estrada_pct, "%)<br>",
      "🟢 Eric Adams: ", comma(Eric_Adams), " (", Adams_pct, "%)<br>",
      "<br><b>Winner: ", winner, "</b>"
    ),
    label_text = paste0("ED ", ED_num, ": ", winner, " won with ", winner_pct, "%")
  )

# Create the map
interactive_map <- leaflet(ed_map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~color_pal(winner),
    fillOpacity = 0.7,
    color = "#444444",
    weight = 1,
    opacity = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = ~popup_text,
    label = ~label_text,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = winner_colors,
    labels = names(winner_colors),
    title = "2025 Mayoral Winner<br>by Election District",
    opacity = 0.7
  ) %>%
  addControl(
    html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
            <h4 style='margin: 0;'>AD34 - 2025 Mayoral Election</h4>
            <p style='margin: 5px 0 0 0; font-size: 12px;'>
            Click on districts for detailed results
            </p></div>",
    position = "topright"
  )

# Save map
saveWidget(interactive_map,
           "outputs/maps/AD34_interactive_map.html",
           selfcontained = TRUE,
           title = "AD34 2025 Mayoral Results by Election District")

cat("  ✓ Saved: outputs/maps/AD34_interactive_map.html\n\n")

# ============================================================
# CREATE SUMMARY STATISTICS
# ============================================================

cat("📊 ELECTION DISTRICT SUMMARY\n")
cat(rep("=", 60), "\n\n")

# Winner breakdown
winner_summary <- ed_map_data %>%
  st_drop_geometry() %>%
  count(winner, name = "EDs_won") %>%
  arrange(desc(EDs_won))

cat("Districts won by candidate:\n")
for (i in 1:nrow(winner_summary)) {
  cat(sprintf("  %s: %d EDs (%.1f%%)\n",
              winner_summary$winner[i],
              winner_summary$EDs_won[i],
              100 * winner_summary$EDs_won[i] / sum(winner_summary$EDs_won)))
}

cat("\nTop 5 EDs by turnout:\n")
top_turnout <- ed_map_data %>%
  st_drop_geometry() %>%
  arrange(desc(total_votes)) %>%
  head(5)

for (i in 1:nrow(top_turnout)) {
  cat(sprintf("  ED %d: %s votes (Winner: %s with %.1f%%)\n",
              top_turnout$ED_num[i],
              comma(top_turnout$total_votes[i]),
              top_turnout$winner[i],
              top_turnout[[paste0(top_turnout$winner[i], "_pct")]][i]))
}

cat("\n✅ Interactive map complete!\n")
cat("📂 Open: outputs/maps/AD34_interactive_map.html\n\n")
