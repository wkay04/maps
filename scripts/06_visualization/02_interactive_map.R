# ============================================================
# INTERACTIVE STRATEGIC TARGETING MAP
# ============================================================
# Creates a simplified, interactive HTML map using leaflet
# Click on EDs to see detailed stats and strategic recommendations
# ============================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

setwd("~/Mayoral Results AD34")

cat("\n🗺️  CREATING INTERACTIVE MAP\n")
cat("============================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading data...\n")

# Load master strategic dataset
master <- read_csv("outputs/analysis/actionable/master_strategic_dataset.csv",
                   col_types = cols(ED = col_character()),
                   show_col_types = FALSE)

# Load ED shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(ED = Election_D) %>%
  st_transform(4326)  # Leaflet uses WGS84

# Merge data
map_data <- ad34_shp %>%
  left_join(master, by = "ED") %>%
  filter(!is.na(strategic_type))

cat("  ✓ Data loaded:", nrow(map_data), "EDs\n\n")

# ============================================================
# PREPARE SIMPLIFIED CATEGORIES
# ============================================================

cat("🎨 Creating simplified categories...\n")

map_data <- map_data %>%
  mutate(
    # Simplified 3-tier system
    priority_tier = case_when(
      strategic_type %in% c("HIGH OPPORTUNITY", "MEDIUM OPPORTUNITY") ~ "🔴 MOBILIZE",
      strategic_type == "SWING - PERSUADE" ~ "🟡 PERSUADE",
      strategic_type == "PROTECT & EXPAND" ~ "🟢 PROTECT",
      TRUE ~ "⚪ OTHER"
    ),

    # Clean ED label
    ed_label = str_remove(ED, "^34"),

    # Format numbers for display
    mamdani_display = sprintf("%.1f%%", Mamdani_pct),
    turnout_display = sprintf("%.1f%%", turnout_rate * 100),
    margin_display = sprintf("%+.1f%%", margin),
    potential_display = scales::comma(potential_voters)
  )

# Define color palette
pal <- colorFactor(
  palette = c("🔴 MOBILIZE" = "#E74C3C",
              "🟡 PERSUADE" = "#F39C12",
              "🟢 PROTECT" = "#27AE60",
              "⚪ OTHER" = "#BDC3C7"),
  domain = map_data$priority_tier
)

cat("  ✓ Categories created\n\n")

# ============================================================
# CREATE POPUP CONTENT
# ============================================================

cat("📝 Creating interactive popups...\n")

map_data <- map_data %>%
  mutate(
    popup_content = sprintf(
      "<div style='font-family: Arial, sans-serif; min-width: 200px;'>
        <h3 style='margin: 0 0 10px 0; color: #2C3E50;'>ED %s</h3>
        <hr style='margin: 5px 0;'>

        <p style='margin: 5px 0;'><strong>🎯 Strategy:</strong> %s</p>

        <h4 style='margin: 10px 0 5px 0; color: #34495E;'>Election Results:</h4>
        <table style='width: 100%%; font-size: 13px;'>
          <tr><td>Mamdani:</td><td style='text-align: right;'><strong>%s</strong></td></tr>
          <tr><td>Cuomo:</td><td style='text-align: right;'>%.1f%%</td></tr>
          <tr><td>Margin:</td><td style='text-align: right; color: %s;'><strong>%s</strong></td></tr>
          <tr><td>Total Votes:</td><td style='text-align: right;'>%s</td></tr>
        </table>

        <h4 style='margin: 10px 0 5px 0; color: #34495E;'>Turnout:</h4>
        <table style='width: 100%%; font-size: 13px;'>
          <tr><td>Current:</td><td style='text-align: right;'><strong>%s</strong></td></tr>
          <tr><td>Potential Voters:</td><td style='text-align: right;'>%s</td></tr>
        </table>

        <h4 style='margin: 10px 0 5px 0; color: #34495E;'>Demographics:</h4>
        <table style='width: 100%%; font-size: 13px;'>
          <tr><td>Total Pop:</td><td style='text-align: right;'>%s</td></tr>
          <tr><td>South Asian:</td><td style='text-align: right;'>%s (%.1f%%)</td></tr>
        </table>

        %s
      </div>",
      ed_label,
      priority_tier,
      mamdani_display,
      Cuomo_pct,
      if_else(margin > 0, "#27AE60", "#E74C3C"),
      margin_display,
      scales::comma(total_votes),
      turnout_display,
      potential_display,
      scales::comma(total_pop),
      scales::comma(south_asian_total),
      south_asian_pct,
      # Add strategic recommendation
      case_when(
        priority_tier == "🔴 MOBILIZE" ~
          "<div style='margin-top: 10px; padding: 8px; background: #FFE5E5; border-left: 3px solid #E74C3C;'>
            <strong>💡 Action:</strong> GOTV operations. High support but low turnout - maximize mobilization!
          </div>",
        priority_tier == "🟡 PERSUADE" ~
          "<div style='margin-top: 10px; padding: 8px; background: #FFF4E5; border-left: 3px solid #F39C12;'>
            <strong>💡 Action:</strong> Targeted persuasion. Competitive - focus on swing voters.
          </div>",
        priority_tier == "🟢 PROTECT" ~
          "<div style='margin-top: 10px; padding: 8px; background: #E8F8F5; border-left: 3px solid #27AE60;'>
            <strong>💡 Action:</strong> Maintain support. Solid base - protect and expand margins.
          </div>",
        TRUE ~ ""
      )
    )
  )

cat("  ✓ Popups created\n\n")

# ============================================================
# CREATE INTERACTIVE MAP
# ============================================================

cat("🗺️  Building interactive leaflet map...\n")

# Create base map
m <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # Add ED polygons
  addPolygons(
    fillColor = ~pal(priority_tier),
    fillOpacity = 0.7,
    color = "white",
    weight = 2,
    opacity = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = ~popup_content,
    label = ~paste0("ED ", ed_label, ": ", priority_tier, " (", mamdani_display, " Mamdani)"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    )
  ) %>%

  # Add legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~priority_tier,
    title = "Strategic Priority",
    opacity = 1
  ) %>%

  # Add title
  addControl(
    html = "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);'>
              <h3 style='margin: 0; color: #2C3E50;'>AD34 Strategic Targeting Map</h3>
              <p style='margin: 5px 0 0 0; font-size: 12px; color: #7F8C8D;'>
                Click on any Election District for details
              </p>
            </div>",
    position = "topleft"
  ) %>%

  # Set view to AD34
  setView(lng = -73.88, lat = 40.76, zoom = 13)

cat("  ✓ Map created\n\n")

# ============================================================
# SAVE INTERACTIVE MAP
# ============================================================

cat("💾 Saving interactive map...\n")

# Save as HTML
saveWidget(m, "outputs/maps/interactive_strategic_map.html",
           selfcontained = TRUE,
           title = "AD34 Strategic Targeting Map")

cat("  ✓ Saved: outputs/maps/interactive_strategic_map.html\n")

# ============================================================
# CREATE SIMPLIFIED STATIC VERSION
# ============================================================

cat("\n🎨 Creating simplified static map...\n")

library(ggplot2)

# Simple 3-color map
p_simple <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = priority_tier),
          color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("🔴 MOBILIZE" = "#E74C3C",
               "🟡 PERSUADE" = "#F39C12",
               "🟢 PROTECT" = "#27AE60",
               "⚪ OTHER" = "#BDC3C7"),
    name = NULL,
    labels = c("🔴 MOBILIZE (High Support, Low Turnout)",
               "🟡 PERSUADE (Competitive/Swing)",
               "🟢 PROTECT (Solid Base)",
               "⚪ OTHER")
  ) +
  geom_sf_text(data = map_data,
               aes(label = ed_label),
               size = 3.5, color = "black", fontface = "bold") +
  labs(title = "AD34 Strategic Targeting - Simplified",
       subtitle = "Three-Tier Priority System",
       caption = "Red = GOTV Focus | Yellow = Persuasion | Green = Maintain") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 11)
  )

ggsave("outputs/maps/simple_strategic_map.png", p_simple,
       width = 10, height = 10, dpi = 300, bg = "white")

cat("  ✓ Saved: simple_strategic_map.png\n")

# ============================================================
# CREATE SUMMARY TABLE
# ============================================================

cat("\n📊 Creating ED summary table...\n")

summary_table <- map_data %>%
  st_drop_geometry() %>%
  select(ED = ed_label, Priority = priority_tier,
         `Mamdani %` = Mamdani_pct, `Turnout %` = turnout_rate,
         `Total Votes` = total_votes, `Potential Voters` = potential_voters,
         `South Asian` = south_asian_total, Margin = margin) %>%
  mutate(
    `Mamdani %` = round(`Mamdani %`, 1),
    `Turnout %` = round(`Turnout %` * 100, 1),
    Margin = round(Margin, 1)
  ) %>%
  arrange(desc(`Potential Voters`))

write_csv(summary_table, "outputs/analysis/actionable/ed_summary_table.csv")

cat("  ✓ Saved: ed_summary_table.csv\n")

cat("\n✅ INTERACTIVE MAP COMPLETE!\n")
cat("\n📂 Files created:\n")
cat("   1. outputs/maps/interactive_strategic_map.html (OPEN IN BROWSER)\n")
cat("   2. outputs/maps/simple_strategic_map.png\n")
cat("   3. outputs/analysis/actionable/ed_summary_table.csv\n")
cat("\n🌐 To view: Open the HTML file in any web browser\n")
cat("   - Click EDs for detailed stats\n")
cat("   - Hover for quick info\n")
cat("   - Zoom and pan\n")
