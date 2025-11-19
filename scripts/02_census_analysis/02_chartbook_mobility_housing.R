# ============================================================
# Queens Assembly District 34 — ACS 2023 Chartbook (Mobility + Housing Edition)
# ============================================================

rm(list = ls())

# --- Libraries ---
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(ggspatial)
library(viridis)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(stringr)
options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# --- Setup ---
setwd("~/Mayoral Results AD34")
dir.create("outputs/AD34_outputs", showWarnings = FALSE, recursive = TRUE)
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

cat("\n🚀 Starting AD34 ACS 2023 Chartbook — Mobility + Housing Edition...\n")

# ============================================================
# 1) Geometry Setup
# ============================================================
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_mask <- st_union(st_transform(ad34, 3857)) %>% st_make_valid() %>% st_cast("MULTIPOLYGON")

filter_to_ad34 <- function(sf_obj) {
  centroids  <- suppressWarnings(st_centroid(sf_obj))
  ad34_local <- st_transform(ad34_mask, st_crs(sf_obj))
  sf_obj$in_ad34 <- st_within(centroids, ad34_local, sparse = FALSE)[,1]
  sf_obj %>% filter(in_ad34)
}

# ============================================================
# 2) Helper Functions
# ============================================================
pull_sf <- function(vars, label) {
  cat("\n🔍 Pulling:", label, "\n")
  acs <- get_acs(
    geography = "tract",
    state = "NY",
    county = "Queens",
    variables = vars,
    year = 2023,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  )
  st_transform(st_make_valid(acs), 3857)
}

make_map <- function(geodata, varname, title, discrete = FALSE) {
  ggplot() +
    annotation_map_tile(type = "osm", zoom = 14) +
    geom_sf(data = st_transform(geodata, 4326),
            aes(fill = .data[[varname]]),
            color = "white", linewidth = 0.08) +
    geom_sf(data = st_transform(ad34_mask, 4326),
            fill = NA, color = "red", linewidth = 0.6) +
    (if (discrete)
      scale_fill_viridis_d(option = "turbo", name = NULL, drop = FALSE)
     else
       scale_fill_viridis_c(option = "plasma", name = NULL)) +
    labs(title = title, caption = "ACS 2019–2023 5-year • NYC AD34") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

weighted_mean <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}
E <- function(code) paste0(code, "E")

# ============================================================
# 3) Housing (Median Rent + Owner-Occupied)
# ============================================================
rent_sf <- pull_sf("B25064_001", "Median Gross Rent") %>%
  mutate(median_rent = B25064_001E)
owner_sf <- pull_sf(c("B25003_001", "B25003_002"), "Owner Occupied") %>%
  mutate(owner_occ = 100 * B25003_002E / B25003_001E)

rent_ad34 <- filter_to_ad34(rent_sf)
owner_ad34 <- filter_to_ad34(owner_sf)

p_rent <- make_map(rent_ad34, "median_rent", "Median Gross Rent (USD)")
p_owner <- make_map(owner_ad34, "owner_occ", "Owner-Occupied Housing (%)")

# ============================================================
# 4) Transit (B08301)
# ============================================================
vars_meta <- load_variables(2023, "acs5", cache = TRUE)

find_code_in_group <- function(group = "B08301", patterns) {
  pool <- vars_meta %>% filter(str_detect(name, paste0("^", group, "_\\d{3}$")))
  for (pat in patterns) {
    hit <- pool %>%
      filter(str_detect(label, regex(pat, ignore_case = TRUE))) %>% slice(1)
    if (nrow(hit) > 0) return(hit$name)
  }
  stop("No match in ", group)
}

codes <- list(
  total     = find_code_in_group("B08301", c("^Estimate!!Total:?$")),
  pt_total  = find_code_in_group("B08301", c("Public transportation \\(excluding taxicab\\):$")),
  bus       = find_code_in_group("B08301", c("Bus")),
  subway    = find_code_in_group("B08301", c("Subway")),
  rail      = find_code_in_group("B08301", c("Long-distance train|Commuter rail")),
  streetcar = find_code_in_group("B08301", c("Streetcar|Light rail")),
  ferry     = find_code_in_group("B08301", c("Ferry"))
)
modal_vars <- unique(unlist(codes))

# --- Borough-level function ---
get_transit_summary <- function(counties) {
  df <- map_dfr(counties, function(cty) {
    get_acs(
      geography = "tract",
      state = "NY",
      county = cty,
      variables = modal_vars,
      year = 2023, survey = "acs5", output = "wide"
    )
  })
  df %>%
    mutate(
      total_workers = .data[[E(codes$total)]],
      bus = .data[[E(codes$bus)]],
      subway = .data[[E(codes$subway)]],
      rail = .data[[E(codes$rail)]],
      street = .data[[E(codes$streetcar)]],
      ferry = .data[[E(codes$ferry)]],
      pt_total = bus + subway + rail + street + ferry
    )
}

# --- NYC & AD34 summaries ---
nyc_counties <- c("Bronx","Kings","New York","Queens","Richmond")
nyc_pt <- get_transit_summary(nyc_counties)

ad34_sf <- get_acs(
  geography = "tract", state = "NY", county = "Queens",
  variables = modal_vars, year = 2023, output = "wide", geometry = TRUE
)
ad34_filtered <- filter_to_ad34(ad34_sf) %>%
  mutate(
    pct_bus_all = 100 * .data[[E(codes$bus)]] / .data[[E(codes$total)]]
  )

p_bus_map <- make_map(ad34_filtered, "pct_bus_all", "Bus Commuters (% of Workers)")

# --- Mode Share Comparison (Stacked Bar) ---
boroughs <- c("Bronx","Kings","New York","Queens","Richmond")
borough_labels <- c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

borough_modes <- map2_dfr(boroughs, borough_labels, function(cty, label) {
  df <- get_transit_summary(cty)
  df %>%
    summarise(
      borough = label,
      bus = weighted_mean(100 * bus / pt_total, total_workers),
      subway = weighted_mean(100 * subway / pt_total, total_workers),
      rail = weighted_mean(100 * rail / pt_total, total_workers),
      street = weighted_mean(100 * street / pt_total, total_workers),
      ferry = weighted_mean(100 * ferry / pt_total, total_workers)
    )
})

# Add AD34
ad34_modes <- ad34_filtered %>%
  st_drop_geometry() %>%
  summarise(
    borough = "AD34",
    bus = weighted_mean(100 * .data[[E(codes$bus)]] /
                          (.data[[E(codes$bus)]] + .data[[E(codes$subway)]] +
                             .data[[E(codes$rail)]] + .data[[E(codes$streetcar)]] +
                             .data[[E(codes$ferry)]]), .data[[E(codes$total)]]),
    subway = weighted_mean(100 * .data[[E(codes$subway)]] /
                             (.data[[E(codes$bus)]] + .data[[E(codes$subway)]] +
                                .data[[E(codes$rail)]] + .data[[E(codes$streetcar)]] +
                                .data[[E(codes$ferry)]]), .data[[E(codes$total)]]),
    rail = weighted_mean(100 * .data[[E(codes$rail)]] /
                           (.data[[E(codes$bus)]] + .data[[E(codes$subway)]] +
                              .data[[E(codes$rail)]] + .data[[E(codes$streetcar)]] +
                              .data[[E(codes$ferry)]]), .data[[E(codes$total)]]),
    street = weighted_mean(100 * .data[[E(codes$streetcar)]] /
                             (.data[[E(codes$bus)]] + .data[[E(codes$subway)]] +
                                .data[[E(codes$rail)]] + .data[[E(codes$streetcar)]] +
                                .data[[E(codes$ferry)]]), .data[[E(codes$total)]]),
    ferry = weighted_mean(100 * .data[[E(codes$ferry)]] /
                            (.data[[E(codes$bus)]] + .data[[E(codes$subway)]] +
                               .data[[E(codes$rail)]] + .data[[E(codes$streetcar)]] +
                               .data[[E(codes$ferry)]]), .data[[E(codes$total)]])
  )

compare_modes <- bind_rows(borough_modes, ad34_modes) %>%
  pivot_longer(-borough, names_to = "Mode", values_to = "Share")

p_modeshare <- ggplot(compare_modes, aes(x = borough, y = Share, fill = Mode)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Transit Mode Share — Boroughs & AD34 (Public Transport Only)",
       x = NULL, y = "Percent of Public Transit Users") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# 5) Country of Birth (B05006)
# ============================================================
# 5) Ancestry Region of Birth (B05006) — Most Common Region per Tract
# ============================================================
# 5) Place of Birth — Subregion Map (B05006)
# ============================================================
cat("\n🌎 Pulling ancestry subregion data (B05006)...\n")

vars_meta <- load_variables(2023, "acs5", cache = TRUE)

b05006_labels <- vars_meta %>%
  filter(str_detect(name, "^B05006_")) %>%
  select(name, label) %>%
  mutate(levels = str_count(label, "!!"))

# --- Extract level-3 subregions (e.g., Caribbean, Western Europe, Eastern Asia) ---
subregion_lookup <- b05006_labels %>%
  filter(levels == 3) %>%
  mutate(subregion = label %>%
           str_remove("^Estimate!!Total:!!Born in ") %>%
           str_remove("^Estimate!!Total:!!") %>%
           str_replace_all("!!", ": ") %>%
           str_replace(".*: ", "") %>%
           str_squish()) %>%
  distinct(name, subregion) %>%
  filter(!is.na(subregion) & subregion != "Total" & subregion != "") %>%
  arrange(subregion)

cat("\n✅ Subregions captured:\n")
print(unique(subregion_lookup$subregion))

# --- Pull ACS data for these variables ---
subregion_sf <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = subregion_lookup$name,
  year = 2023, survey = "acs5",
  output = "wide", geometry = TRUE
) %>% st_transform(3857)

# --- Identify top subregion per tract ---
subregion_long <- subregion_sf %>%
  st_drop_geometry() %>%
  select(GEOID, matches("^B05006_\\d{3}E$")) %>%
  pivot_longer(cols = starts_with("B05006_"),
               names_to = "varE", values_to = "count") %>%
  mutate(var = sub("E$", "", varE)) %>%
  left_join(subregion_lookup, by = c("var" = "name")) %>%
  filter(!is.na(subregion) & count > 0)

top_subregion <- subregion_long %>%
  group_by(GEOID) %>%
  slice_max(count, n = 1, with_ties = FALSE) %>%
  ungroup()

# --- Join back to geometry ---
subregion_map <- subregion_sf %>%
  select(GEOID, geometry) %>%
  left_join(top_subregion, by = "GEOID") %>%
  filter(!is.na(subregion)) %>%
  st_make_valid()

subregion_ad34 <- filter_to_ad34(subregion_map)

# --- Limit to top 10 for clear legend ---
top_subregions <- names(sort(table(subregion_ad34$subregion), decreasing = TRUE)[1:10])
subregion_ad34$subregion_clean <- ifelse(subregion_ad34$subregion %in% top_subregions,
                                         subregion_ad34$subregion, "Other Subregion")

cat("\n🗺️ Top subregions in AD34:\n")
print(sort(table(subregion_ad34$subregion_clean), decreasing = TRUE))

p_subregion <- make_map(subregion_ad34, "subregion_clean",
                        "Most Common Ancestry Subregion (Place of Birth)",
                        discrete = TRUE)

# ============================================================
# 6) PDF Export
pdf("outputs/AD34_outputs/AD34_Chartbook_2023_MOBILITY_HOUSING.pdf", width = 8.5, height = 11)

# Page 1 — Housing
grid.arrange(p_rent, p_owner, nrow = 2)

# Page 2 — Transit
grid.arrange(p_bus_map, p_modeshare, nrow = 2)

# Page 3 — Ancestry Subregion
grid.arrange(p_subregion, nrow = 1)

dev.off()

cat("\n✅ Saved PDF: outputs/AD34_outputs/AD34_Chartbook_2023_MOBILITY_HOUSING.pdf\n")
