library(sf)
library(stringr)
library(ggplot2)
library(viridis)
library(ggspatial)
library(prettymapr)
library(scales)
library(dplyr)
library(patchwork)

setwd("~/Mayoral Results AD34")

# -----------------------------
# 1. Load shapefile and district shapes
# -----------------------------
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
sldl <- state_legislative_districts(state = "NY", house = "lower", year = 2025)

# Filter for Assembly Districts 34 and 36
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad36 <- sldl %>% filter(NAMELSAD == "Assembly District 36")

# -----------------------------
# 2. Load and clean election results
# -----------------------------
vote_columns <- c(
  "Zohran.Kwame.Mamdani", "Curtis.A..Sliwa", "Irene.Estrada",
  "Zohran.Kwame.Mamdani.1", "Curtis.A..Sliwa.1", "Eric.L..Adams",
  "Joseph.Hernandez", "Andrew.M..Cuomo", "Jim.Walden", "WRITE.IN"
)

process_results <- function(df, district_prefix) {
  df <- Filter(function(x) !all(is.na(x)), df)
  df <- df[-1,]
  
  df <- df %>%
    mutate(across(all_of(vote_columns), ~ as.numeric(.))) %>%
    mutate(
      Zohran.Total = Zohran.Kwame.Mamdani + Zohran.Kwame.Mamdani.1,
      Sliwa.Total  = Curtis.A..Sliwa + Curtis.A..Sliwa.1,
      Cuomo.Total  = Andrew.M..Cuomo,
      total_votes  = rowSums(across(all_of(vote_columns)), na.rm = TRUE),
      pct_Zohran   = Zohran.Total / total_votes,
      pct_Zohran_100 = pct_Zohran * 100
    ) %>%
    mutate(
      max_votes = pmax(Zohran.Total, Sliwa.Total, Cuomo.Total, na.rm = TRUE),
      winner = case_when(
        Zohran.Total == max_votes ~ "Mamdani",
        Sliwa.Total  == max_votes ~ "Sliwa",
        Cuomo.Total  == max_votes ~ "Cuomo",
        TRUE ~ "No Winner"
      )
    )
  
  df$X <- sprintf("%s%03d", district_prefix, as.numeric(sub("ED ", "", df$X)))
  return(df)
}

# Read both precinct files
results34 <- process_results(read.csv("data/raw/election/Precinct results.csv"), "34")
results36 <- process_results(read.csv("data/intermediate/District 36.csv"), "36")

# -----------------------------
# 3. Join shapefile with results
# -----------------------------
ED_shapefile <- st_read(shp_path) %>%
  filter(County == "Queens") %>%
  mutate(Election_D = as.character(Election_D))

# Align CRS for spatial operations
ED_shapefile <- st_transform(ED_shapefile, st_crs(ad34))
ad36 <- st_transform(ad36, st_crs(ad34))

# Join separately for each district
ED34 <- ED_shapefile %>%
  left_join(results34, by = c("Election_D" = "X")) %>%
  filter(str_sub(Election_D, 1, 2) == "34")

ED36 <- ED_shapefile %>%
  left_join(results36, by = c("Election_D" = "X")) %>%
  filter(str_sub(Election_D, 1, 2) == "36")

# -----------------------------
# 4. Plot function
# -----------------------------
plot_ad_map <- function(ED_data, ad_shape, ad_label) {
  ggplot() +
    annotation_map_tile(type = "osm", zoom = 17, "toner-lite") +
    geom_sf(
      data = ED_data,
      aes(fill = winner),
      color = "white", size = 0.4, alpha = 0.8
    ) +
    geom_sf(data = ad_shape, fill = NA, color = "black", size = 0.8) +
    scale_fill_manual(
      values = c(
        "Mamdani" = "#F94144",
        "Sliwa" = "#43AA8B",
        "Cuomo" = "#4928EB",
        "No Winner" = "grey80"
      ),
      name = "Winner"
    ) +
    coord_sf() +
    theme_minimal() +
    labs(
      title = "Winning Candidate by Election District",
      subtitle = ad_label,
      caption = "Source: NYC Election Data"
    ) +
    theme(plot.title = element_text(face = "bold", size = 16))
}

# -----------------------------
# 5. Plot both districts
# -----------------------------
map34 <- plot_ad_map(ED34, ad34, "Assembly District 34")
map36 <- plot_ad_map(ED36, ad36, "Assembly District 36")

map34 + map36
