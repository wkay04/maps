# ============================================================
# STRATEGIC TARGETING MAPS
# ============================================================
# Creates maps highlighting strategic ED classifications
# for campaign targeting and resource allocation
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)

setwd("~/Mayoral Results AD34")

cat("\n🗺️  CREATING STRATEGIC TARGETING MAPS\n")
cat("====================================\n\n")

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
  st_transform(4326)

# Merge data
map_data <- ad34_shp %>%
  left_join(master, by = "ED")

cat("  ✓ Data loaded:", nrow(map_data), "EDs\n\n")

# ============================================================
# MAP 1: STRATEGIC CLASSIFICATION
# ============================================================

cat("🎯 Creating Strategic Classification Map...\n")

# Define colors for strategic types
strategic_colors <- c(
  "HIGH OPPORTUNITY" = "#FF6B6B",        # Red
  "MEDIUM OPPORTUNITY" = "#FFA07A",      # Light red
  "PROTECT & EXPAND" = "#4ECDC4",        # Teal
  "SWING - PERSUADE" = "#FFE66D",        # Yellow
  "UPHILL PERSUASION" = "#C7CEEA",       # Light blue
  "OPPOSITION TERRITORY" = "#95A5A6"     # Gray
)

p1 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = strategic_type),
          color = "white", linewidth = 0.3) +
  scale_fill_manual(values = strategic_colors,
                    name = "Strategic\nClassification",
                    na.value = "gray90") +
  geom_sf_text(data = map_data %>% filter(!is.na(strategic_type)),
               aes(label = str_remove(ED, "^34")),
               size = 2, color = "black", fontface = "bold") +
  labs(title = "Strategic Classification by Election District",
       subtitle = "AD34 - 2025 Mayoral Election",
       caption = "High Opportunity = Strong Support + Low Turnout | Swing = Competitive 45-50%") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/strategic_classification.png", p1,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: strategic_classification.png\n")

# ============================================================
# MAP 2: HIGH OPPORTUNITY EDs (Highlight Priority Targets)
# ============================================================

cat("🚨 Creating High Opportunity Targets Map...\n")

map_data <- map_data %>%
  mutate(
    priority_category = case_when(
      strategic_type == "HIGH OPPORTUNITY" ~ "HIGHEST PRIORITY",
      strategic_type == "MEDIUM OPPORTUNITY" ~ "HIGH PRIORITY",
      strategic_type == "SWING - PERSUADE" ~ "SWING - PERSUADE",
      strategic_type == "PROTECT & EXPAND" ~ "PROTECT & EXPAND",
      TRUE ~ "Other"
    )
  )

priority_colors <- c(
  "HIGHEST PRIORITY" = "#FF0000",     # Bright red
  "HIGH PRIORITY" = "#FF6B6B",        # Red
  "SWING - PERSUADE" = "#FFD700",     # Gold
  "PROTECT & EXPAND" = "#4ECDC4",     # Teal
  "Other" = "#E8E8E8"                 # Light gray
)

p2 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = priority_category),
          color = "white", linewidth = 0.3) +
  scale_fill_manual(values = priority_colors,
                    name = "Priority Level") +
  geom_sf_text(data = map_data %>%
                 filter(priority_category %in% c("HIGHEST PRIORITY", "HIGH PRIORITY", "SWING - PERSUADE")),
               aes(label = str_remove(ED, "^34")),
               size = 3, color = "black", fontface = "bold") +
  labs(title = "Campaign Priority Targeting Map",
       subtitle = "Focus: High Opportunity + Swing EDs",
       caption = "Red = Strong Support but Low Turnout | Gold = Competitive/Persuadable") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/priority_targets.png", p2,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: priority_targets.png\n")

# ============================================================
# MAP 3: POTENTIAL VOTERS (Size by Opportunity)
# ============================================================

cat("📊 Creating Potential Voters Heat Map...\n")

p3 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = potential_voters),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis(option = "plasma",
                     name = "Potential\nVoters",
                     na.value = "gray90",
                     labels = scales::comma) +
  geom_sf_text(data = map_data %>%
                 filter(potential_voters > 800),
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "white", fontface = "bold") +
  labs(title = "Potential Voters by Election District",
       subtitle = "Population - Current Turnout = Untapped Potential",
       caption = "Brightest colors = Highest opportunity for voter mobilization") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/potential_voters_heatmap.png", p3,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: potential_voters_heatmap.png\n")

# ============================================================
# MAP 4: SUPPORT + TURNOUT (2D Classification)
# ============================================================

cat("📈 Creating Support x Turnout Matrix Map...\n")

map_data <- map_data %>%
  mutate(
    support_level = case_when(
      Mamdani_pct >= 60 ~ "Strong (60%+)",
      Mamdani_pct >= 50 ~ "Moderate (50-60%)",
      Mamdani_pct >= 45 ~ "Competitive (45-50%)",
      TRUE ~ "Weak (<45%)"
    ),
    turnout_level = case_when(
      turnout_rate >= 0.30 ~ "High (30%+)",
      turnout_rate >= 0.20 ~ "Medium (20-30%)",
      TRUE ~ "Low (<20%)"
    ),
    matrix_category = paste0(support_level, " / ", turnout_level)
  )

# Define key combinations
map_data <- map_data %>%
  mutate(
    matrix_simple = case_when(
      Mamdani_pct >= 60 & turnout_rate < 0.25 ~ "Strong Support\nLow Turnout",
      Mamdani_pct >= 50 & turnout_rate >= 0.25 ~ "Moderate Support\nHigh Turnout",
      Mamdani_pct >= 45 & Mamdani_pct < 50 ~ "Competitive\n(Swing)",
      Mamdani_pct < 45 ~ "Opposition\nTerritory",
      TRUE ~ "Other"
    )
  )

matrix_colors <- c(
  "Strong Support\nLow Turnout" = "#FF4444",
  "Moderate Support\nHigh Turnout" = "#4ECDC4",
  "Competitive\n(Swing)" = "#FFD700",
  "Opposition\nTerritory" = "#95A5A6",
  "Other" = "#E0E0E0"
)

p4 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = matrix_simple),
          color = "white", linewidth = 0.3) +
  scale_fill_manual(values = matrix_colors,
                    name = "Support +\nTurnout") +
  geom_sf_text(data = map_data %>%
                 filter(matrix_simple != "Other"),
               aes(label = str_remove(ED, "^34")),
               size = 2, color = "black", fontface = "bold") +
  labs(title = "Support Level × Turnout Matrix",
       subtitle = "Red = Maximize opportunity | Teal = Protect base | Gold = Persuade",
       caption = "Strategic quadrants based on current support and voter turnout") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/support_turnout_matrix.png", p4,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: support_turnout_matrix.png\n")

# ============================================================
# MAP 5: SOUTH ASIAN CONCENTRATION + SUPPORT
# ============================================================

cat("🇧🇩 Creating South Asian Community Map...\n")

p5 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = south_asian_pct),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis(option = "magma",
                     name = "South Asian\n% of Pop",
                     na.value = "gray90") +
  geom_sf_text(data = map_data %>%
                 filter(south_asian_total > 150),
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "white", fontface = "bold") +
  labs(title = "South Asian Population Concentration",
       subtitle = "Bangladeshi + Pakistani + Indian (proxy for Muslim/Hindu communities)",
       caption = "Darker = Higher concentration | Mamdani's core support base") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/south_asian_concentration.png", p5,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: south_asian_concentration.png\n")

# ============================================================
# MAP 6: MOSQUE PROXIMITY (Religious Geography)
# ============================================================

cat("🕌 Creating Mosque Proximity Map...\n")

map_data <- map_data %>%
  mutate(
    mosque_category = case_when(
      dist_to_mosque < 200 ~ "< 200m (Very Close)",
      dist_to_mosque < 500 ~ "200-500m (Close)",
      dist_to_mosque < 1000 ~ "500-1000m (Moderate)",
      dist_to_mosque >= 1000 ~ "> 1000m (Far)",
      TRUE ~ "Unknown"
    )
  )

mosque_colors <- c(
  "< 200m (Very Close)" = "#8B0000",
  "200-500m (Close)" = "#FF4500",
  "500-1000m (Moderate)" = "#FFA500",
  "> 1000m (Far)" = "#FFE4B5",
  "Unknown" = "#E8E8E8"
)

p6 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = mosque_category),
          color = "white", linewidth = 0.3) +
  scale_fill_manual(values = mosque_colors,
                    name = "Distance to\nNearest Mosque") +
  geom_sf_text(data = map_data %>%
                 filter(dist_to_mosque < 500),
               aes(label = str_remove(ED, "^34")),
               size = 2.5, color = "white", fontface = "bold") +
  labs(title = "Proximity to Mosques (OpenStreetMap Data)",
       subtitle = "EDs within 200m show 60-100% Mamdani support",
       caption = "Dark red = 'Mosque Belt' core support area") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/mosque_proximity.png", p6,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: mosque_proximity.png\n")

# ============================================================
# MAP 7: PERFORMANCE GAP (Over/Under-performing)
# ============================================================

cat("⚡ Creating Performance Gap Map...\n")

p7 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = performance_gap),
          color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#D32F2F", mid = "white", high = "#388E3C",
                       midpoint = 0,
                       name = "Performance\nGap",
                       limits = c(-40, 40),
                       na.value = "gray90") +
  geom_sf_text(data = map_data %>%
                 filter(abs(performance_gap) > 10, !is.na(performance_gap)),
               aes(label = str_remove(ED, "^34")),
               size = 2, color = "black", fontface = "bold") +
  labs(title = "Electoral Performance vs Expected Support",
       subtitle = "Green = Over-performing | Red = Under-performing",
       caption = "Based on demographic model: Expected vs Actual Mamdani support") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/performance_gap.png", p7,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: performance_gap.png\n")

# ============================================================
# MAP 8: VOTE MARGIN (Electoral Security)
# ============================================================

cat("🗳️  Creating Vote Margin Map...\n")

p8 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = margin),
          color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#D32F2F", mid = "#FFE082", high = "#388E3C",
                       midpoint = 0,
                       name = "Margin\n(Mamdani %\n- Cuomo %)",
                       na.value = "gray90") +
  geom_sf_text(data = map_data %>%
                 filter(abs(margin) < 10 | margin > 30),
               aes(label = str_remove(ED, "^34")),
               size = 2, color = "black", fontface = "bold") +
  labs(title = "Electoral Margin by Election District",
       subtitle = "Green = Mamdani lead | Yellow = Competitive | Red = Cuomo lead",
       caption = "Close margins (<5 points) highlighted for targeting") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right"
  )

ggsave("outputs/maps/vote_margin.png", p8,
       width = 10, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: vote_margin.png\n")

# ============================================================
# COMBINED DASHBOARD
# ============================================================

cat("📊 Creating Combined Dashboard...\n")

# Smaller versions for dashboard
p1_small <- p1 + theme(legend.position = "bottom", legend.direction = "horizontal")
p2_small <- p2 + theme(legend.position = "bottom", legend.direction = "horizontal")
p5_small <- p5 + theme(legend.position = "bottom", legend.direction = "horizontal")
p6_small <- p6 + theme(legend.position = "bottom", legend.direction = "horizontal")

combined <- (p1_small | p2_small) / (p5_small | p6_small) +
  plot_annotation(
    title = "Strategic Targeting Dashboard - AD34 Mayoral Race 2025",
    subtitle = "Campaign Resource Allocation Guide",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

ggsave("outputs/maps/strategic_dashboard.png", combined,
       width = 16, height = 12, dpi = 300, bg = "white")

cat("  ✓ Saved: strategic_dashboard.png\n")

cat("\n✅ ALL STRATEGIC MAPS CREATED!\n")
cat("   Location: outputs/maps/\n")
cat("   Files:\n")
cat("     - strategic_classification.png\n")
cat("     - priority_targets.png\n")
cat("     - potential_voters_heatmap.png\n")
cat("     - support_turnout_matrix.png\n")
cat("     - south_asian_concentration.png\n")
cat("     - mosque_proximity.png\n")
cat("     - performance_gap.png\n")
cat("     - vote_margin.png\n")
cat("     - strategic_dashboard.png (combined)\n")
