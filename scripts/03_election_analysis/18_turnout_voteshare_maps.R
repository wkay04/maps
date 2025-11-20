library(sf)
library(stringr)
library(ggplot2)
library(viridis)
library(ggspatial)
library(prettymapr)
library(scales)
library(dplyr)
library(tidyverse)
library(tigris)

setwd("~/Mayoral Results AD34")

# Load election results
cat("Loading election results...\n")
Precinct.results <- read.csv("data/raw/election/Precinct results.csv")

# Load shapefiles
cat("Loading Election District shapefile...\n")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
ED_shapefile <- st_read(shp_path, quiet = TRUE)

# Load AD34 boundary
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts(state = "NY", house = "lower", year = 2025)
ad34 <- sldl %>% dplyr::filter(NAMELSAD == "Assembly District 34")

# Filter ED shapefile to Queens and AD34
ED_shapefile <- ED_shapefile %>%
  filter(County == "Queens") %>%
  filter(str_sub(Election_D, 1, 2) == "34")

# Clean election results data
results <- data.frame(Precinct.results)
results <- Filter(function(x) !all(is.na(x)), results)
results <- results[-1,]  # Remove header row

# Format ED identifiers
results$X <- sprintf("34%03d", as.numeric(sub("ED ", "", results$X)))

# Define vote columns
vote_columns <- c("Zohran.Kwame.Mamdani", "Curtis.A..Sliwa", "Irene.Estrada",
                  "Zohran.Kwame.Mamdani.1", "Curtis.A..Sliwa.1","Eric.L..Adams",
                  "Joseph.Hernandez", "Andrew.M..Cuomo", "Jim.Walden", "WRITE.IN")

# Convert vote columns to numeric
results <- results %>%
  mutate(across(all_of(vote_columns), ~ as.numeric(.)))

# Calculate Zohran total votes (Democratic + Working Families lines)
results$Zohran.Total <- as.numeric(results$Zohran.Kwame.Mamdani) +
                        as.numeric(results$Zohran.Kwame.Mamdani.1)

# Calculate total votes cast
results <- results %>%
  mutate(total_votes = rowSums(across(all_of(vote_columns)), na.rm = TRUE))

# Calculate Zohran vote share percentage
results <- results %>%
  mutate(
    pct_Zohran = (Zohran.Total / total_votes) * 100
  )

# Parse the "Reported" column to extract registered voters or turnout
# The format is "99.00%" or "85.71%" indicating reporting percentage
# We need to look up registered voters from another source or estimate
# For turnout calculation, we'll use Public Counter data if available

# Since we don't have registered voters in this file, we'll calculate turnout
# as percentage of total ballots cast relative to the maximum in the district
# This is a proxy for relative turnout

# Calculate turnout proxy: votes as percentage of district maximum
max_votes_in_district <- max(results$total_votes, na.rm = TRUE)
results <- results %>%
  mutate(
    turnout_pct = (total_votes / max_votes_in_district) * 100
  )

# Prepare for join
ED_shapefile$Election_D <- as.character(ED_shapefile$Election_D)
results$X <- as.character(results$X)

# Join election results to shapefile
ED_data <- ED_shapefile %>%
  left_join(results, by = c("Election_D" = "X"))

# Transform to appropriate CRS
ED_data <- st_transform(ED_data, 3857)
ad34_boundary <- st_transform(ad34, 3857)

cat("\n")
cat("Creating maps...\n")
cat("\n")

# Create output directory
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# MAP 1: Turnout Percentage by ED
cat("Generating Turnout Percentage map...\n")
turnout_map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles", quiet = TRUE) +
  geom_sf(data = st_transform(ED_data, 4326),
          aes(fill = turnout_pct),
          color = "white", linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = st_transform(ad34_boundary, 4326),
          fill = NA, color = "red", linewidth = 1.2) +
  scale_fill_viridis_c(option = "plasma", name = "Turnout (%)",
                       na.value = "grey90",
                       labels = comma_format(accuracy = 1)) +
  labs(title = "Voter Turnout by Election District",
       subtitle = "Assembly District 34 - 2025 Democratic Mayoral Primary",
       caption = "Source: NYC Board of Elections • AD34 boundary in red\nTurnout shown as % of maximum ED turnout in district") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40"),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("outputs/maps/AD34_turnout_by_ED.png",
       turnout_map, width = 12, height = 10, dpi = 300, bg = "white")
cat("   Saved: outputs/maps/AD34_turnout_by_ED.png\n")

# MAP 2: Zohran Vote Share by ED
cat("Generating Zohran Vote Share map...\n")
voteshare_map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles", quiet = TRUE) +
  geom_sf(data = st_transform(ED_data, 4326),
          aes(fill = pct_Zohran),
          color = "white", linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = st_transform(ad34_boundary, 4326),
          fill = NA, color = "red", linewidth = 1.2) +
  scale_fill_viridis_c(option = "mako", name = "Vote Share (%)",
                       na.value = "grey90",
                       labels = comma_format(accuracy = 1)) +
  labs(title = "Zohran Kwame Mamdani Vote Share by Election District",
       subtitle = "Assembly District 34 - 2025 Democratic Mayoral Primary",
       caption = "Source: NYC Board of Elections • AD34 boundary in red\nIncludes votes on Democratic and Working Families ballot lines") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40"),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("outputs/maps/AD34_zohran_voteshare_by_ED.png",
       voteshare_map, width = 12, height = 10, dpi = 300, bg = "white")
cat("   Saved: outputs/maps/AD34_zohran_voteshare_by_ED.png\n")

cat("\n")
cat("Maps created successfully!\n")
cat("\n")
cat("Summary Statistics:\n")
cat("------------------\n")
cat(sprintf("Total EDs mapped: %d\n", nrow(ED_data)))
cat(sprintf("Zohran vote share range: %.1f%% - %.1f%%\n",
            min(ED_data$pct_Zohran, na.rm = TRUE),
            max(ED_data$pct_Zohran, na.rm = TRUE)))
cat(sprintf("Average Zohran vote share: %.1f%%\n",
            mean(ED_data$pct_Zohran, na.rm = TRUE)))
cat(sprintf("Turnout range: %.1f%% - %.1f%%\n",
            min(ED_data$turnout_pct, na.rm = TRUE),
            max(ED_data$turnout_pct, na.rm = TRUE)))
