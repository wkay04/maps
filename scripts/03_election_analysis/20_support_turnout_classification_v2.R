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
library(readxl)

setwd("~/Mayoral Results AD34")

# Load registration data (skip header rows)
cat("Loading voter registration data...\n")
registration <- read_excel("queensed_nov25.xlsx", skip = 4)

# Check the structure of registration data
cat("Registration data columns:\n")
print(colnames(registration))
cat("\n")

# Show sample data
cat("Sample registration data:\n")
print(head(registration %>% filter(!is.na(STATUS)), 10))
cat("\n")

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

# Process registration data to match ED format
# The ELECTION DIST column has format "Queens 23001" where 23 is AD and 001 is ED
registration <- registration %>%
  filter(!is.na(STATUS) & !is.na(`ELECTION DIST`)) %>%
  mutate(
    # Extract the 5-digit code from "Queens XXXXX"
    ed_code = str_extract(`ELECTION DIST`, "\\d{5}"),
    AD = as.numeric(substr(ed_code, 1, 2)),
    ED = as.numeric(substr(ed_code, 3, 5)),
    ED_ID = sprintf("%02d%03d", AD, ED)
  ) %>%
  filter(AD == 34 & STATUS == "Active")

cat("Filtered to AD34 Active voters:\n")
cat(sprintf("Number of EDs: %d\n", n_distinct(registration$ED_ID)))
cat("\n")

# Use TOTAL column for registered voters (sum across all parties)
registration$registered_voters <- registration$TOTAL

# Join registration data to results
results <- results %>%
  left_join(registration %>% select(ED_ID, registered_voters),
            by = c("X" = "ED_ID"))

# Calculate actual turnout percentage
results <- results %>%
  mutate(
    turnout_pct = (total_votes / registered_voters) * 100
  )

cat("\n")
cat("Turnout Statistics:\n")
cat("-------------------\n")
cat(sprintf("Min turnout: %.1f%%\n", min(results$turnout_pct, na.rm = TRUE)))
cat(sprintf("Max turnout: %.1f%%\n", max(results$turnout_pct, na.rm = TRUE)))
cat(sprintf("Mean turnout: %.1f%%\n", mean(results$turnout_pct, na.rm = TRUE)))
cat(sprintf("Median turnout: %.1f%%\n", median(results$turnout_pct, na.rm = TRUE)))
cat("\n")

# Prepare for join
ED_shapefile$Election_D <- as.character(ED_shapefile$Election_D)
results$X <- as.character(results$X)

# Join election results to shapefile
ED_data <- ED_shapefile %>%
  left_join(results, by = c("Election_D" = "X"))

# Remove EDs with no data (NA or zero votes)
ED_data <- ED_data %>%
  filter(!is.na(total_votes) & total_votes > 0 & !is.na(turnout_pct))

# Calculate median values for classification thresholds
median_support <- median(ED_data$pct_Zohran, na.rm = TRUE)
median_turnout <- median(ED_data$turnout_pct, na.rm = TRUE)

cat("Classification Thresholds:\n")
cat("-------------------------\n")
cat(sprintf("Median Zohran Support: %.1f%%\n", median_support))
cat(sprintf("Median Turnout: %.1f%%\n", median_turnout))
cat("\n")

# Classify each ED into four categories
ED_data <- ED_data %>%
  mutate(
    support_level = ifelse(pct_Zohran >= median_support, "High Support", "Low Support"),
    turnout_level = ifelse(turnout_pct >= median_turnout, "High Turnout", "Low Turnout"),
    classification = paste(support_level, turnout_level, sep = "\n")
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
    avg_turnout = mean(turnout_pct, na.rm = TRUE),
    total_registered = sum(registered_voters, na.rm = TRUE),
    total_voted = sum(total_votes, na.rm = TRUE)
  ) %>%
  mutate(
    category_turnout = (total_voted / total_registered) * 100
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
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Define color palette for the four categories
# Using intuitive colors with clear distinction
category_colors <- c(
  "High Support\nHigh Turnout" = "#1b7837",      # Dark green - ideal quadrant
  "High Support\nLow Turnout" = "#7fbc41",       # Light green - mobilization opportunity
  "Low Support\nHigh Turnout" = "#e08214",       # Orange - persuasion needed
  "Low Support\nLow Turnout" = "#c51b7d"         # Magenta - challenge area
)

# Create the classification map with clean, simple legend
classification_map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14, cachedir = "cache/tiles", quiet = TRUE) +
  geom_sf(data = st_transform(ED_data, 4326),
          aes(fill = classification),
          color = "white", linewidth = 0.25, alpha = 0.85) +
  geom_sf(data = st_transform(ad34_boundary, 4326),
          fill = NA, color = "black", linewidth = 1.5) +
  scale_fill_manual(
    values = category_colors,
    name = NULL,
    na.value = "grey90",
    drop = FALSE,
    labels = c(
      sprintf("High Support, High Turnout (%d EDs)",
              sum(ED_data$classification == "High Support\nHigh Turnout", na.rm = TRUE)),
      sprintf("High Support, Low Turnout (%d EDs)",
              sum(ED_data$classification == "High Support\nLow Turnout", na.rm = TRUE)),
      sprintf("Low Support, High Turnout (%d EDs)",
              sum(ED_data$classification == "Low Support\nHigh Turnout", na.rm = TRUE)),
      sprintf("Low Support, Low Turnout (%d EDs)",
              sum(ED_data$classification == "Low Support\nLow Turnout", na.rm = TRUE))
    )
  ) +
  labs(
    title = "Zohran Support & Voter Turnout Classification",
    subtitle = "Assembly District 34 - 2025 Democratic Mayoral Primary",
    caption = "Source: NYC Board of Elections | AD34 boundary in black"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey50", margin = margin(t = 10)),
    legend.position = "right",
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    legend.spacing.y = unit(0.4, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  guides(fill = guide_legend(byrow = TRUE))

ggsave("outputs/maps/AD34_support_turnout_classification_v2.png",
       classification_map, width = 14, height = 11, dpi = 300, bg = "white")
cat("   Saved: outputs/maps/AD34_support_turnout_classification_v2.png\n")

# Create a detailed summary table with ED details
ED_summary <- ED_data %>%
  st_drop_geometry() %>%
  select(Election_D, pct_Zohran, turnout_pct, registered_voters, total_votes,
         Zohran.Total, classification, support_level, turnout_level) %>%
  arrange(classification, desc(pct_Zohran))

# Save summary table
write.csv(ED_summary, "outputs/tables/AD34_ED_classification_summary_v2.csv", row.names = FALSE)
cat("   Saved: outputs/tables/AD34_ED_classification_summary_v2.csv\n")

cat("\n")
cat("Classification complete!\n")
cat("\n")
cat("Detailed Summary by Category:\n")
cat("=============================\n")

# Print detailed summary
for (cat_name in levels(ED_data$classification)) {
  eds_in_cat <- ED_data %>%
    st_drop_geometry() %>%
    filter(classification == cat_name)

  if (nrow(eds_in_cat) > 0) {
    cat(sprintf("\n%s\n", gsub("\n", " / ", cat_name)))
    cat(sprintf("  Number of EDs: %d\n", nrow(eds_in_cat)))
    cat(sprintf("  Average Zohran Support: %.1f%%\n", mean(eds_in_cat$pct_Zohran, na.rm = TRUE)))
    cat(sprintf("  Average Turnout: %.1f%%\n", mean(eds_in_cat$turnout_pct, na.rm = TRUE)))
    cat(sprintf("  Total Registered Voters: %s\n", format(sum(eds_in_cat$registered_voters, na.rm = TRUE), big.mark = ",")))
    cat(sprintf("  Total Votes Cast: %s\n", format(sum(eds_in_cat$total_votes, na.rm = TRUE), big.mark = ",")))
    cat(sprintf("  Zohran Votes: %s\n", format(sum(eds_in_cat$Zohran.Total, na.rm = TRUE), big.mark = ",")))
    cat(sprintf("  Category-wide Turnout: %.1f%%\n",
                (sum(eds_in_cat$total_votes, na.rm = TRUE) / sum(eds_in_cat$registered_voters, na.rm = TRUE)) * 100))
  }
}

cat("\n")
cat("Overall District Summary:\n")
cat("------------------------\n")
total_registered <- sum(ED_data$registered_voters, na.rm = TRUE)
total_voted <- sum(ED_data$total_votes, na.rm = TRUE)
total_zohran <- sum(ED_data$Zohran.Total, na.rm = TRUE)
cat(sprintf("Total Registered Voters: %s\n", format(total_registered, big.mark = ",")))
cat(sprintf("Total Votes Cast: %s\n", format(total_voted, big.mark = ",")))
cat(sprintf("Overall Turnout: %.1f%%\n", (total_voted / total_registered) * 100))
cat(sprintf("Zohran Total Votes: %s\n", format(total_zohran, big.mark = ",")))
cat(sprintf("Zohran Share of Votes: %.1f%%\n", (total_zohran / total_voted) * 100))
cat("\n")
