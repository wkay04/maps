# =====================================================
#  Country of Birth (Foreign-Born) — Queens AD34 & AD36
# =====================================================

# --- Libraries ---
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(maptiles)
library(ggspatial)
library(RColorBrewer)
library(purrr)
library(patchwork)
library(stringr)
library(grid)

options(tigris_use_cache = TRUE)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")

# =====================================================
#  Setup
# =====================================================
setwd("~/Mayoral Results AD34")
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# Load ACS variable dictionary
v23 <- load_variables(2023, "acs5", cache = TRUE)

vars_B05006 <- v23 %>%
  filter(str_starts(name, "B05006")) %>%
  select(name, label, concept)

# =====================================================
#  Geography – Assembly Districts 34 & 36
# =====================================================
sldl <- state_legislative_districts(state = "NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad36 <- sldl %>% filter(NAMELSAD == "Assembly District 36")

tracts <- tracts(state = "NY", county = "Queens", year = 2022)
tracts34 <- st_filter(tracts, ad34)
tracts36 <- st_filter(tracts, ad36)

# =====================================================
#  Pull ACS data
# =====================================================
foreign_country <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = vars_B05006$name,
  year = 2023,
  geometry = FALSE
) %>%
  left_join(vars_B05006, by = c("variable" = "name"))

# =====================================================
#  Helper function
# =====================================================
get_top_country <- function(tracts_sf) {
  joined <- left_join(tracts_sf, foreign_country, by = "GEOID")
  
  joined %>%
    mutate(
      parts_raw = str_split(label, "!!"),
      last_raw  = map_chr(parts_raw, ~ if (length(.x)) tail(.x, 1) else NA_character_),
      is_country = !is.na(last_raw) & !str_detect(last_raw, ":$") &
        !str_detect(last_raw, regex("^Other", ignore_case = TRUE)),
      country = str_squish(str_remove_all(last_raw, ":"))
    ) %>%
    filter(is_country) %>%
    group_by(GEOID) %>%
    slice_max(estimate, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    st_as_sf()
}

country34 <- get_top_country(tracts34)
country36 <- get_top_country(tracts36)

# =====================================================
#  Visualization Setup
# =====================================================
country34_3857 <- st_transform(country34, 3857)
country36_3857 <- st_transform(country36, 3857)
ad34_3857 <- st_transform(ad34, 3857)
ad36_3857 <- st_transform(ad36, 3857)

dir.create("tile_cache", showWarnings = FALSE)
tiles34 <- get_tiles(ad34_3857, provider = "CartoDB.Positron", zoom = 12, cachedir = "tile_cache")
tiles36 <- get_tiles(ad36_3857, provider = "CartoDB.Positron", zoom = 12, cachedir = "tile_cache")

# ✅ unified palette across both maps
countries_all <- sort(unique(c(country34_3857$country, country36_3857$country)))
pal <- colorRampPalette(brewer.pal(12, "Paired"))(length(countries_all))
names(pal) <- countries_all

# =====================================================
#  Base map generator (no individual legends)
# =====================================================
base_map <- function(data_sf, boundary_sf, tiles, district_label) {
  ggplot() +
    annotation_spatial(tiles) +
    geom_sf(data = data_sf, aes(fill = country), color = "gray25", size = 0.05) +
    geom_sf(data = boundary_sf, fill = NA, color = "black", linewidth = 0.5) +
    scale_fill_manual(values = pal, limits = countries_all, drop = FALSE) +
    coord_sf(crs = st_crs(data_sf)) +
    theme_minimal(base_size = 11) +
    labs(title = district_label) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "none",
      panel.grid = element_blank()
    )
}

map34 <- base_map(country34_3857, ad34_3857, tiles34, "Assembly District 34")
map36 <- base_map(country36_3857, ad36_3857, tiles36, "Assembly District 36")

# =====================================================
#  Shared legend (consistent colors)
# =====================================================
# =====================================================
#  Shared legend (consistent colors)
# =====================================================
# Create an empty data frame that includes all country levels
legend_data <- data.frame(country = factor(countries_all, levels = countries_all))

legend_plot <- ggplot(legend_data, aes(x = country, fill = country)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
  scale_fill_manual(
    values = pal,
    limits = countries_all,
    drop = FALSE,
    guide = guide_legend(ncol = 5)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 6.5),
    legend.title = element_text(face = "bold", size = 8)
  ) +
  labs(fill = "Country of Birth")

legend <- cowplot::get_legend(legend_plot)

# =====================================================
#  Combine everything (side-by-side maps + one legend)
# =====================================================
combined <- (map34 | map36) +
  plot_annotation(
    title = "Most Common Country of Birth — Queens Assembly Districts 34 & 36",
    subtitle = "ACS 2019–2023 5-Year • Table B05006",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11)
    )
  )

final_plot <- cowplot::plot_grid(combined, legend, ncol = 1, rel_heights = c(1, 0.15))
print(final_plot)
