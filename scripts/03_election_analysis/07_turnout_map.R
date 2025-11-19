# ============================================================
# AD34 — Turnout Maps: 2021 Mayor (Gen), 2024 Presidential, 2025 Zohran (Gen)
# ============================================================

library(tidyverse)
library(sf)
library(viridis)
library(ggspatial)
library(patchwork)
library(scales)

options(stringsAsFactors = FALSE)
setwd("~/Mayoral Results AD34")
dir.create("outputs/AD34_outputs", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1) Geometry for AD34 Election Districts
# ------------------------------------------------------------
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
stopifnot(nrow(ed_all) == 61)

# ------------------------------------------------------------
# 2) Load data files
# ------------------------------------------------------------
# NOTE: These CSV files must be created manually from election results
# Place them in outputs/AD34_outputs/ before running this script

# 2021 Mayor (General) — already cleaned to ed_code, turnout
turn_2021 <- read_csv("outputs/AD34_outputs/ED_Turnout_2021_Mayor.csv", show_col_types = FALSE) %>%
  mutate(ed_code = str_trim(ed_code)) %>%
  select(ed_code, turnout_2021 = turnout)

# 2024 Presidential margins — we'll compute turnout = Dem + Rep + Other (more complete than provided 'total')
pres24 <- read_csv("outputs/AD34_outputs/ED_Presidential_Margins_2024.csv", show_col_types = FALSE) %>%
  mutate(ed_code = str_trim(ed_code),
         turnout_2024 = coalesce(Dem, 0) + coalesce(Rep, 0) + coalesce(Other, 0)) %>%
  select(ed_code, turnout_2024, margin_pct)

# 2025 Zohran (General) — total_votes is the district turnout for that contest
zohran25 <- read_csv("outputs/AD34_outputs/ED_Zohran_Results_2025.csv", show_col_types = FALSE) %>%
  mutate(ed_code = str_trim(ed_code)) %>%
  select(ed_code, turnout_2025 = total_votes, zohran_share)

# Quick shapes
cat("\n--- Shapes ---\n")
cat("turn_2021:", nrow(turn_2021), "rows\n")
cat("pres24:", nrow(pres24), "rows\n")
cat("zohran25:", nrow(zohran25), "rows\n")

# ------------------------------------------------------------
# 3) Join to geometry
# ------------------------------------------------------------
map_data <- ed_all %>%
  left_join(turn_2021, by = "ed_code") %>%
  left_join(pres24,   by = "ed_code") %>%
  left_join(zohran25, by = "ed_code")

# Diagnostics
cat("\n--- Missing counts ---\n")
print(colSums(is.na(st_drop_geometry(map_data) %>% 
                      select(turnout_2021, turnout_2024, turnout_2025, margin_pct, zohran_share))))

cat("\n--- Ranges (ignoring NA) ---\n")
safesum <- function(x) summary(x[is.finite(x)])
cat("2021 turnout:\n"); print(safesum(map_data$turnout_2021))
cat("2024 turnout:\n"); print(safesum(map_data$turnout_2024))
cat("2025 turnout (Zohran):\n"); print(safesum(map_data$turnout_2025))

# ------------------------------------------------------------
# 4) Maps — three standalone PNGs + one combined
# ------------------------------------------------------------

p_2021 <- ggplot(map_data) +
  geom_sf(aes(fill = turnout_2021), color = "white", size = 0.12) +
  scale_fill_viridis(option = "mako", na.value = "grey90",
                     labels = comma, name = "Ballots") +
  labs(title = "AD34 — 2021 Mayor (General) Turnout") +
  theme_minimal(base_size = 12)

p_2024 <- ggplot(map_data) +
  geom_sf(aes(fill = turnout_2024), color = "white", size = 0.12) +
  scale_fill_viridis(option = "mako", na.value = "grey90",
                     labels = comma, name = "Ballots") +
  labs(title = "AD34 — 2024 Presidential Turnout") +
  theme_minimal(base_size = 12)

p_2025 <- ggplot(map_data) +
  geom_sf(aes(fill = turnout_2025), color = "white", size = 0.12) +
  scale_fill_viridis(option = "mako", na.value = "grey90",
                     labels = comma, name = "Ballots") +
  labs(title = "AD34 — 2025 Zohran (General) Turnout") +
  theme_minimal(base_size = 12)

# Save individual maps
ggsave("AD34_outputs/AD34_Turnout_2021_Mayor.png", p_2021, width = 6.5, height = 6, dpi = 300)
ggsave("AD34_outputs/AD34_Turnout_2024_Pres.png",  p_2024, width = 6.5, height = 6, dpi = 300)
ggsave("AD34_outputs/AD34_Turnout_2025_Zohran.png", p_2025, width = 6.5, height = 6, dpi = 300)

# Combined triptych
combined <- (p_2021 | p_2024 | p_2025) +
  plot_annotation(
    title   = "NY Assembly District 34 — Turnout by Contest (ED level)",
    caption = "Source: NYC Board of Elections 2021–2025 | Analysis: Will Kay"
  )

ggsave("AD34_outputs/AD34_Turnout_Triptych_2021_2024_2025.png",
       combined, width = 19, height = 6.2, dpi = 300)

cat("\n✅ Saved:\n",
    " - AD34_outputs/AD34_Turnout_2021_Mayor.png\n",
    " - AD34_outputs/AD34_Turnout_2024_Pres.png\n",
    " - AD34_outputs/AD34_Turnout_2025_Zohran.png\n",
    " - AD34_outputs/AD34_Turnout_Triptych_2021_2024_2025.png\n", sep = "")
