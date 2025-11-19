# ============================================================
# AD34 — Zohran Mamdani 2025 General Election Results Map
# ============================================================

library(tidyverse)
library(sf)
library(viridis)
library(ggspatial)
library(patchwork)

setwd("~/Mayoral Results AD34")
dir.create("outputs/AD34_outputs", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1. Load ED Geometry for AD34
# ============================================================

arc_url <- paste0(
  "https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/ArcGIS/rest/services/",
  "NYS_Elections_Districts_and_Polling_Locations/FeatureServer/4/query?",
  "where=Election_District%20LIKE%20'34%25'&outFields=County,Municipality,Election_District&f=geojson"
)

ed_all <- st_read(arc_url, quiet = FALSE) %>%
  mutate(
    AD = "34",
    ED = str_extract(Election_District, "\\d+$"),
    ED = str_pad(ED, 3, pad = "0"),
    ed_code = paste0(AD, "-", ED)
  ) %>%
  st_transform(2263)

cat("✅ Geometry loaded —", nrow(ed_all), "ED polygons\n")

# ============================================================
# 2. Load Zohran 2025 Election Results
# ============================================================
# NOTE: This CSV file must be created manually from election results
# Place it in outputs/AD34_outputs/ before running this script

zohran_2025 <- read_csv("outputs/AD34_outputs/ED_Zohran_Results_2025.csv", show_col_types = FALSE)

cat("✅ Loaded Zohran results —", nrow(zohran_2025), "rows\n")
cat("Columns:", paste(names(zohran_2025), collapse = ", "), "\n")

# ============================================================
# 3. Join shapefile + results
# ============================================================

map_data_zohran <- ed_all %>%
  left_join(zohran_2025, by = "ed_code")

# ============================================================
# 4. Plot — Zohran Vote Share
# ============================================================

p_zohran <- ggplot(map_data_zohran) +
  geom_sf(aes(fill = zohran_share), color = "white", size = 0.1) +
  scale_fill_viridis(
    option = "mako",
    name = "Zohran vote share (%)",
    limits = c(0, 100),
    na.value = "grey90"
  ) +
  labs(
    title = "AD34 — Zohran Mamdani Vote Share (2025 General Election)",
    subtitle = "Percent of total votes by Election District",
    caption = "Source: NYC Board of Elections | Analysis: Will Kay"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ============================================================
# 5. Save map
# ============================================================

ggsave("AD34_outputs/Zohran_Share_2025.png", p_zohran,
       width = 8, height = 6, dpi = 300)

cat("\n✅ Map saved to AD34_outputs/Zohran_Share_2025.png\n")
