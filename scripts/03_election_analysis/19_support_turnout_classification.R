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

# Remove EDs with no data (NA or zero votes)
ED_data <- ED_data %>%
  filter(!is.na(total_votes) & total_votes > 0)

# Calculate median values for classification thresholds
median_support <- median(ED_data$pct_Zohran, na.rm = TRUE)
median_turnout <- median(ED_data$turnout_pct, na.rm = TRUE)

cat("\n")
cat("Classification Thresholds:\n")
cat("-------------------------\n")
cat(sprintf("Median Zohran Support: %.1f%%\n", median_support))
cat(sprintf("Median Turnout: %.1f%%\n", median_turnout))
cat("\n")

# Classify each ED into four categories
ED_data <- ED_data %>%
  mutate(
    classification = case_when(
      pct_Zohran >= median_support & turnout_pct >= median_turnout ~ "High Support\nHigh Turnout",
      pct_Zohran >= median_support & turnout_pct < median_turnout ~ "High Support\nLow Turnout",
      pct_Zohran < median_support & turnout_pct >= median_turnout ~ "Low Support\nHigh Turnout",
      pct_Zohran < median_support & turnout_pct < median_turnout ~ "Low Support\nLow Turnout",
      TRUE ~ NA_character_
    )
  )

# Create ordered factor for consistent legend ordering
ED_data$classification <- factor(ED_data$classification,
                                  levels = c("High Support\nHigh Turnout",
                                           "High Support\nLow Turnout",
                                           "Low Support\nHigh Turnout",
                                           "Low Support\nLow Turnout"))

# Count EDs in each category
category_counts <- ED_data %>%
  st_drop_geometry() %>%
  group_by(classification) %>%
  summarise(
    n_EDs = n(),
    avg_support = mean(pct_Zohran, na.rm = TRUE),
    avg_turnout = mean(turnout_pct, na.rm = TRUE)
  )

cat("EDs by Category:\n")
cat("----------------\n")
print(category_counts)
cat("\n")

# Transform to appropriate CRS
ED_data <- st_transform(ED_data, 3857)
ad34_boundary <- st_transform(ad34, 3857)

cat("Creating classification map...\n")
cat("\n")

# Create output directory
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# Define color palette for the four categories
# Using a diverging color scheme that emphasizes the combinations
category_colors <- c(
  "High Support\nHigh Turnout" = "#1a9850",      # Dark green - best case
  "High Support\nLow Turnout" = "#91cf60",       # Light green - untapped potential
  "Low Support\nHigh Turnout" = "#fc8d59",       # Orange - high engagement, low support
  "Low Support\nLow Turnout" = "#d73027"         # Red - needs work
)

# Create the classification map
classification_map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles", quiet = TRUE) +
  geom_sf(data = st_transform(ED_data, 4326),
          aes(fill = classification),
          color = "white", linewidth = 0.4, alpha = 0.85) +
  geom_sf(data = st_transform(ad34_boundary, 4326),
          fill = NA, color = "black", linewidth = 1.2) +
  scale_fill_manual(
    values = category_colors,
    name = "Classification",
    na.value = "grey90",
    drop = FALSE
  ) +
  labs(
    title = "Zohran Support & Turnout Classification by Election District",
    subtitle = sprintf("AD34 - 2025 Democratic Mayoral Primary (Thresholds: %.1f%% support, %.1f%% turnout)",
                      median_support, median_turnout),
    caption = "Source: NYC Board of Elections • AD34 boundary in black\nClassification based on median splits for support and turnout"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("outputs/maps/AD34_support_turnout_classification.png",
       classification_map, width = 13, height = 10, dpi = 300, bg = "white")
cat("   Saved: outputs/maps/AD34_support_turnout_classification.png\n")

# Create a summary table with ED details
ED_summary <- ED_data %>%
  st_drop_geometry() %>%
  select(Election_D, pct_Zohran, turnout_pct, total_votes, Zohran.Total, classification) %>%
  arrange(classification, desc(pct_Zohran))

# Save summary table
write.csv(ED_summary, "outputs/tables/AD34_ED_classification_summary.csv", row.names = FALSE)
cat("   Saved: outputs/tables/AD34_ED_classification_summary.csv\n")

cat("\n")
cat("Classification complete!\n")
cat("\n")
cat("Summary by Category:\n")
cat("-------------------\n")

# Print detailed summary
for (cat_name in levels(ED_data$classification)) {
  eds_in_cat <- ED_data %>%
    st_drop_geometry() %>%
    filter(classification == cat_name)

  if (nrow(eds_in_cat) > 0) {
    cat(sprintf("\n%s: %d EDs\n", cat_name, nrow(eds_in_cat)))
    cat(sprintf("  Average Zohran Support: %.1f%%\n", mean(eds_in_cat$pct_Zohran, na.rm = TRUE)))
    cat(sprintf("  Average Turnout: %.1f%%\n", mean(eds_in_cat$turnout_pct, na.rm = TRUE)))
    cat(sprintf("  Total Votes: %d\n", sum(eds_in_cat$total_votes, na.rm = TRUE)))
    cat(sprintf("  Zohran Votes: %d\n", sum(eds_in_cat$Zohran.Total, na.rm = TRUE)))
  }
}

cat("\n")
