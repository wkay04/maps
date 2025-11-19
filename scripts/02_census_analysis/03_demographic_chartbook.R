# ================================
# Queens AD34 Census Chartbook — ACS 2023 (5-year, DP tables)
# ================================
rm(list = ls())

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(patchwork)
library(viridis)
library(ggspatial)
options(tigris_use_cache = TRUE)

setwd("~/Mayoral Results AD34")
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

# -------------------------------
# 1. Load and Prepare Geometries
# -------------------------------
# -------------------------------
# 1. Load and Prepare Geometries (drop partial tracts)
# -------------------------------
# -------------------------------
# 1. Load and Prepare Geometries
# -------------------------------
sldl_raw   <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34_raw   <- sldl_raw %>% filter(NAMELSAD == "Assembly District 34")
tracts_raw <- tracts("NY", "Queens", year = 2022)

# Transform to Web Mercator for OSM
ad34_3857   <- st_transform(st_make_valid(ad34_raw), 3857)
tracts_3857 <- st_transform(st_make_valid(tracts_raw), 3857)

# Union AD34 (outline)
ad34_mask <- ad34_3857 %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Bounding box for zoom region
bb <- st_bbox(st_transform(ad34_mask, 4326))  # use lon/lat for tile request




# -------------------------------
# 2. ACS Pull Helper
# -------------------------------
pull_dp_wide_sf <- function(short_to_dp) {
  acs <- get_acs(
    geography = "tract",
    state     = "NY",
    county    = "Queens",
    variables = unname(short_to_dp),
    year      = 2023,
    survey    = "acs5",
    output    = "wide",
    geometry  = TRUE
  )
  
  find_col <- function(code) {
    pe <- paste0(code, "PE")
    e  <- paste0(code, "E")
    if (pe %in% names(acs) && !all(is.na(acs[[pe]]))) pe
    else if (e %in% names(acs)) e
    else stop("No column found for ", code)
  }
  
  cols <- vapply(unname(short_to_dp), find_col, character(1))
  names(cols) <- names(short_to_dp)
  
  geom   <- st_geometry(acs)
  acs_df <- st_drop_geometry(acs)[, c("GEOID", cols)]
  names(acs_df)[match(cols, names(acs_df))] <- names(short_to_dp)
  
  acs_clean <- cbind(acs_df, geometry = geom) |> st_as_sf(sf_column_name = "geometry")
  
  # ✅ make ACS tracts match the basemap/outline CRS
  acs_clean <- st_transform(acs_clean, 3857)
  
  return(acs_clean)
}






# -------------------------------
# 3. Map Helper with OSM Tiles
# -------------------------------
make_map <- function(geodata, varname, title, option = "plasma") {
  # Convert geometries to lon/lat for correct alignment with tiles
  geodata_ll <- st_transform(geodata, 4326)
  ad34_mask_ll <- st_transform(ad34_mask, 4326)
  
  ggplot() +
    annotation_map_tile(
      type = "osm",
      zoom = 15,
      progress = "none"
    ) +
    geom_sf(
      data = geodata_ll,
      aes(fill = .data[[varname]]),
      color = "white",
      linewidth = 0.05
    ) +
    geom_sf(
      data = ad34_mask_ll,
      fill = NA,
      color = "red",
      linewidth = 0.6
    ) +
    coord_sf(
      xlim = c(
        st_bbox(ad34_mask_ll)$xmin - 0.01,  # add buffer west/east
        st_bbox(ad34_mask_ll)$xmax + 0.01
      ),
      ylim = c(
        st_bbox(ad34_mask_ll)$ymin - 0.005, # add buffer north/south
        st_bbox(ad34_mask_ll)$ymax + 0.005
      ),
      expand = FALSE,
      crs = st_crs(4326)
    )+
    scale_fill_viridis_c(option = option, name = NULL) +
    labs(
      title = title,
      caption = "ACS 2019–2023 5-Year • Basemap: Carto Light"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold"), legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
      
    )
}




# -------------------------------
# 4. Themes (verified DP variables)
# -------------------------------
themes <- list(
  demographics = list(
    vars   = c(pop = "DP05_0001", age = "DP05_0018"),
    titles = c("Total Population", "Median Age (Years)")
  ),
  economics = list(
    vars   = c(income = "DP03_0062", poverty = "DP03_0128P"),
    titles = c("Median Household Income (USD)", "Below Poverty Level (%)")
  ),
  housing = list(
    vars   = c(med_rent = "DP04_0134", rent_burden = "DP04_0137P"),
    titles = c("Median Gross Rent (USD)", "Households Paying ≥30% Income on Rent (%)")
  ),
  nativity = list(
    vars   = c(native = "DP02_0093P", foreign = "DP02_0094P"),
    titles = c("Native (%)", "Foreign-Born (%)")
  ),
  race = list(
    vars   = c(white = "DP05_0037P", asian = "DP05_0066P"),
    titles = c("White Alone (%)", "Asian Alone (%)")
  )
)

# -------------------------------
# 5. Build Pages (side-by-side)
# -------------------------------
build_page <- function(vset, title) {
  sfw <- pull_dp_wide_sf(vset$vars)
  nm  <- names(vset$vars)
  
  # Transform AD34 to match tract CRS
  ad34_local <- st_transform(ad34_mask, st_crs(sfw))
  
  # Calculate intersection area fraction for each tract
  # Compute area fraction of each tract overlapping AD34
  intersections <- st_intersection(
    sfw %>% mutate(tract_area = st_area(.)),
    ad34_local
  ) %>%
    mutate(intersect_area = st_area(.)) %>%
    st_drop_geometry() %>%
    select(GEOID, tract_area, intersect_area) %>%
    group_by(GEOID) %>%
    summarise(
      tract_area = first(tract_area),
      intersect_area = sum(intersect_area)
    ) %>%
    mutate(frac_in_ad34 = as.numeric(intersect_area / tract_area))
  
  # Join the fraction back to the full tract dataset
  sfw <- sfw %>%
    left_join(intersections %>% select(GEOID, frac_in_ad34), by = "GEOID")
  
  # Replace NAs (no overlap) with 0
  sfw$frac_in_ad34[is.na(sfw$frac_in_ad34)] <- 0
  
  # Keep tracts where at least 25% of area lies inside AD34
  sfw_filt <- sfw %>% filter(frac_in_ad34 >= 0.25)
  
  # Clip visually to AD34 boundary
  sfw_filt <- st_intersection(sfw_filt, ad34_local)
  
  # Keep tracts where at least 25% of area lies inside AD34
  sfw_filt <- sfw %>% filter(frac_in_ad34 >= 0.25)
  
  # Optional: clip them visually to AD34 boundary
  sfw_filt <- st_intersection(sfw_filt, ad34_local)
  
  (make_map(sfw_filt, nm[1], vset$titles[1]) |
      make_map(sfw_filt, nm[2], vset$titles[2])) +
    plot_annotation(title = title)
}


page1 <- build_page(themes$demographics, "Demographic Overview – Queens AD34")
page2 <- build_page(themes$economics,    "Economic Indicators – Queens AD34")
page3 <- build_page(themes$housing,      "Housing Characteristics – Queens AD34")
page4 <- build_page(themes$nativity,     "Nativity – Queens AD34")
page5 <- build_page(themes$race,         "Race & Ethnicity – Queens AD34")

# -------------------------------
# 6. Export PDF
# -------------------------------
pdf("outputs/reports/Queens_AD34_Census_Chartbook.pdf", width = 11, height = 8.5)  # landscape
print(page1); print(page2); print(page3); print(page4); print(page5)
dev.off()

cat("✅ Chartbook saved: outputs/reports/Queens_AD34_Census_Chartbook.pdf\n")
