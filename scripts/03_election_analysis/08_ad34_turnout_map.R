# ============================================================
# AD34 Turnout Analysis and Mapping - November 2025
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(scales)
library(rvest)

setwd("~/Mayoral Results AD34")

cat("\n🗳️  AD34 Turnout Analysis - November 2025\n")
cat("=========================================\n\n")

# ============================================================
# STEP 1: PARSE ENR DATA AND COMBINE PARTY LINES
# ============================================================

cat("📥 Parsing Mayor results from ENR...\n")

# Fetch fresh data
mayor_url <- "https://enr.boenyc.gov/CD27286AD344.html"
mayor_page <- read_html(mayor_url)
tables <- mayor_page %>% html_table(fill = TRUE)

# Get main table
main_table <- tables[[which.max(sapply(tables, nrow))]]

# Find data rows (those starting with whitespace followed by numbers)
# The ENR format has EDs in a column, with vote counts following

# Extract the table more carefully
# Look for rows that have ED numbers

cat("  Table size:", nrow(main_table), "rows\n")

# The table structure: each row after headers has ED info
# Let's find where the data actually starts

# Strategy: Look for rows where first column contains "ED" followed by digits
data_rows <- which(str_detect(main_table[[1]], "ED\\s+\\d+"))

if (length(data_rows) > 0) {
  cat("  Found", length(data_rows), "ED data rows\n")

  # Extract those rows
  ed_results <- main_table[data_rows, ]

  # Parse ED numbers from first column
  ed_results$ED_num <- str_extract(ed_results[[1]], "\\d+") %>% as.numeric()

  # The vote counts are in subsequent columns
  # We need to identify which columns are vote counts vs labels

  # Typically the format alternates: candidate columns have numbers, labels don't
  # Let's extract numeric columns
  numeric_cols <- sapply(ed_results, function(x) {
    all(str_detect(x, "^\\d+$|^\\d+\\.\\d+%$") | x == "" | is.na(x))
  })

  cat("  Found", sum(numeric_cols), "numeric data columns\n")

  # Now we need to map candidates to their party lines
  # From the header, we know the candidates appear in order

  # For Mayor 2025, the candidates are:
  # Zohran Kwame Mamdani: Democratic (col), Working Families (col)
  # Curtis A. Sliwa: Republican, Protect Animals
  # Eric L. Adams: Safe&Affordable/EndAntiSemitism, Fight and Deliver
  # Irene Estrada: Conservative
  # Joseph Hernandez: Quality of Life
  # Andrew M. Cuomo: Integrity
  # Jim Walden: (party line?)

  # Let's manually parse the visible columns
  # From examining the HTML, typical structure has:
  # - Reported % column
  # - Then vote columns for each candidate/party line

  # Extract column indices for vote data
  # Usually starts around column 2 or 3

  # MANUAL MAPPING based on ENR structure inspection
  # This requires looking at the actual column positions

  # Alternative: Let's extract just what we need - total votes by ED
  # Sum all numeric columns to get total turnout

  vote_cols_idx <- which(numeric_cols)
  # Skip the first one (might be reported %)
  vote_cols_idx <- vote_cols_idx[vote_cols_idx > 2]

  cat("  Processing vote columns:", length(vote_cols_idx), "\n\n")

  # Calculate total votes per ED
  ed_turnout <- ed_results %>%
    mutate(
      ED = paste0("34", str_pad(ED_num, 3, pad = "0")),  # Format: 34001, 34002, etc.
      # Sum across all candidate vote columns
      total_votes = rowSums(across(all_of(vote_cols_idx), ~as.numeric(.) ), na.rm = TRUE)
    ) %>%
    select(ED, ED_num, total_votes)

  cat("✓ Calculated turnout for", nrow(ed_turnout), "Election Districts\n")
  cat("  Total votes across all EDs:", sum(ed_turnout$total_votes, na.rm = TRUE), "\n\n")

} else {
  cat("⚠️  Could not find ED data rows\n")
  ed_turnout <- NULL
}

# ============================================================
# STEP 2: LOAD ED SHAPEFILES
# ============================================================

cat("🗺️  Loading Election District shapefiles...\n")

ed_shp <- st_read(
  "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp",
  quiet = TRUE
)

# Filter for AD34 (Queens, Election_D starts with "34")
ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34"))

cat("  Found", nrow(ad34_shp), "AD34 Election Districts in shapefile\n\n")

# ============================================================
# STEP 3: JOIN TURNOUT DATA WITH SHAPEFILE
# ============================================================

cat("🔗 Merging turnout data with shapes...\n")

# Join
ad34_map_data <- ad34_shp %>%
  left_join(ed_turnout, by = c("Election_D" = "ED"))

# Check merge success
matched <- sum(!is.na(ad34_map_data$total_votes))
cat("  Matched", matched, "out of", nrow(ad34_shp), "Election Districts\n\n")

# ============================================================
# STEP 4: CREATE TURNOUT MAP
# ============================================================

cat("📊 Creating turnout map...\n")

# Create output directory
dir.create("outputs/maps", showWarnings = FALSE, recursive = TRUE)

# Transform to appropriate projection for NYC
ad34_map_data <- st_transform(ad34_map_data, 3857)

# Create the map
turnout_map <- ggplot(ad34_map_data) +
  geom_sf(aes(fill = total_votes), color = "white", size = 0.1) +
  scale_fill_viridis(
    name = "Total Votes",
    option = "plasma",
    na.value = "grey80",
    labels = comma
  ) +
  labs(
    title = "Voter Turnout by Election District",
    subtitle = "Assembly District 34 - November 4, 2025 General Election (Unofficial)",
    caption = "Source: NYC Board of Elections ENR System\nAD34: Jackson Heights, Corona, East Elmhurst, Astoria (Queens)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Save map
ggsave("outputs/maps/AD34_turnout_map_2025.png",
       turnout_map,
       width = 12, height = 10, dpi = 300)

cat("  ✓ Saved: outputs/maps/AD34_turnout_map_2025.png\n\n")

# ============================================================
# STEP 5: TURNOUT STATISTICS
# ============================================================

cat("📈 Turnout Statistics:\n")
cat(rep("=", 50), "\n")

if (!is.null(ed_turnout) && nrow(ed_turnout) > 0) {
  cat("  Total votes cast:", comma(sum(ed_turnout$total_votes, na.rm = TRUE)), "\n")
  cat("  Average per ED:", comma(round(mean(ed_turnout$total_votes, na.rm = TRUE))), "\n")
  cat("  Median per ED:", comma(median(ed_turnout$total_votes, na.rm = TRUE)), "\n")
  cat("  Range:", comma(min(ed_turnout$total_votes, na.rm = TRUE)), "-",
      comma(max(ed_turnout$total_votes, na.rm = TRUE)), "\n\n")

  # Top 5 EDs by turnout
  cat("  Top 5 EDs by turnout:\n")
  top5 <- ed_turnout %>%
    arrange(desc(total_votes)) %>%
    head(5)

  for(i in 1:nrow(top5)) {
    cat("    ", i, ". ED", top5$ED_num[i], "-", comma(top5$total_votes[i]), "votes\n")
  }
  cat("\n")
}

# ============================================================
# STEP 6: SAVE PROCESSED DATA
# ============================================================

cat("💾 Saving processed data...\n")

# Save turnout data
if (!is.null(ed_turnout)) {
  write_csv(ed_turnout, "data/intermediate/ad34_turnout_2025.csv")
  cat("  ✓ data/intermediate/ad34_turnout_2025.csv\n")
}

# Save map data (as shapefile)
st_write(ad34_map_data,
         "data/intermediate/ad34_turnout_map_2025.shp",
         delete_dsn = TRUE, quiet = TRUE)
cat("  ✓ data/intermediate/ad34_turnout_map_2025.shp\n\n")

cat("✅ Analysis complete!\n\n")

cat("📂 Output files:\n")
cat("  • Map: outputs/maps/AD34_turnout_map_2025.png\n")
cat("  • Data: data/intermediate/ad34_turnout_2025.csv\n")
cat("  • Shapefile: data/intermediate/ad34_turnout_map_2025.shp\n\n")
