# ============================================================
# AD34 — Elections 2024–2025 Harmonized Two-Panel Map
# ============================================================

library(tidyverse)
library(sf)
library(viridis)
library(ggspatial)
library(patchwork)

setwd("~/Mayoral Results AD34")
dir.create("outputs/AD34_outputs", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1. Load Geometry
# ============================================================

arc_url <- paste0(
  "https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/ArcGIS/rest/services/",
  "NYS_Elections_Districts_and_Polling_Locations/FeatureServer/4/query?",
  "where=Election_District%20LIKE%20'34%25'&outFields=County,Municipality,Election_District&f=geojson"
)

ed_all <- st_read(arc_url, quiet = TRUE) %>%
  mutate(
    AD = "34",
    ED = str_sub(Election_District, -3),
    ed_code = paste0(AD, "-", ED)
  ) %>%
  st_transform(2263)

cat("✅ Geometry loaded:", nrow(ed_all), "ED polygons\n")

# ============================================================
# 2. Load Results
# ============================================================
# NOTE: These CSV files must be created manually from election results
# Place them in outputs/AD34_outputs/ before running this script

pres_margin <- read_csv("outputs/AD34_outputs/ED_Presidential_Margins_2024.csv", show_col_types = FALSE)
zohran_2025 <- read_csv("outputs/AD34_outputs/ED_Zohran_Results_2025.csv", show_col_types = FALSE)

# Normalize ED codes just in case
fix_ed <- function(x) {
  x <- as.character(x)
  x <- gsub(" ", "", x)
  x <- ifelse(str_detect(x, "^34-"), x, paste0("34-", str_pad(str_extract(x, "\\d+$"), 3, pad = "0")))
  return(x)
}
pres_margin <- pres_margin %>% mutate(ed_code = fix_ed(ed_code))
zohran_2025 <- zohran_2025 %>% mutate(ed_code = fix_ed(ed_code))
ed_all <- ed_all %>% mutate(ed_code = fix_ed(ed_code))

# ============================================================
# 3. Join Datasets
# ============================================================

map_data <- ed_all %>%
  left_join(pres_margin %>% select(ed_code, margin_pct), by = "ed_code") %>%
  left_join(zohran_2025 %>% select(ed_code, zohran_share), by = "ed_code")

cat("\n--- Data Checks ---\n")
print(colSums(is.na(map_data[, c("margin_pct", "zohran_share")])))
cat("\nPresidential Margin Summary:\n"); print(summary(map_data$margin_pct))
cat("\nZohran Share Summary:\n"); print(summary(map_data$zohran_share))

# ============================================================
# 4. Build Plots (Color-consistent and aligned)
# ============================================================

# Common theme for clean layout
base_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11, margin = margin(b = 6))
  )

# ---- 2024 Presidential Margin ----
p_margin <- ggplot(map_data) +
  geom_sf(aes(fill = margin_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    name = "Democratic Advantage (%)",
    low = "#d73027", mid = "white", high = "#4575b4",
    midpoint = 0, limits = c(-50, 100), na.value = "grey90"
  ) +
  labs(
    title = "2024 Presidential Margin — Harris vs Trump",
    subtitle = "Positive values indicate stronger Democratic performance"
  ) +
  base_theme

# ---- 2025 Zohran Mamdani ----
p_zohran <- ggplot(map_data) +
  geom_sf(aes(fill = zohran_share - 50), color = "white", size = 0.1) +
  scale_fill_gradient2(
    name = "Zohran Mamdani Vote Share (Centered at 50%)",
    low = "#d73027", mid = "white", high = "#4575b4",
    midpoint = 0, limits = c(-50, 50), na.value = "grey90"
  ) +
  labs(
    title = "2025 General Election — Zohran Mamdani",
    subtitle = "Vote share relative to 50% threshold (blue = >50%)"
  ) +
  base_theme

# ---- Combine ----
combined <- (p_margin | p_zohran) +
  plot_annotation(
    title = "NY Assembly District 34 — 2024 Presidential & 2025 General Elections",
    caption = "Source: NYC Board of Elections | Analysis: Will Kay"
  )

# ============================================================
# 5. Save Output
# ============================================================

ggsave("AD34_outputs/AD34_Election_TwoPanel_Harmonized.png",
       combined, width = 14, height = 6, dpi = 300)

cat("\n✅ Saved: AD34_outputs/AD34_Election_TwoPanel_Harmonized.png\n")
