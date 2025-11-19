# ============================================================
# AD34 Turnout Analysis - November 2025 Election
# ============================================================
# Parses ENR results, combines votes across party lines,
# calculates turnout, and creates visualizations

library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📊 AD34 Turnout Analysis\n")
cat("========================\n\n")

# ============================================================
# STEP 1: PARSE AND CLEAN ENR DATA
# ============================================================

cat("📥 Reading and parsing Mayor results...\n")

# Read the raw ENR data
mayor_raw <- read_csv("data/raw/election/ad34_enr/AD34_Mayor_20251116_1457.csv",
                      show_col_types = FALSE)

# The ENR data has a complex structure - let me examine it
cat("  Raw dimensions:", nrow(mayor_raw), "rows ×", ncol(mayor_raw), "cols\n")

# Find the row with candidate names (has "Democratic", "Republican", etc.)
header_row_idx <- which(str_detect(mayor_raw[[1]], "Reported"))

if (length(header_row_idx) > 0) {
  cat("  Found data starting at row:", header_row_idx + 1, "\n")

  # Extract data rows (after header)
  data_start <- header_row_idx + 1
  mayor_data_raw <- mayor_raw[data_start:nrow(mayor_raw), ]

  # The first column has ED numbers like "ED    1", "ED    2"
  # Other columns have vote counts

  # Extract ED numbers
  ed_col <- mayor_data_raw[[1]]
  eds <- str_extract(ed_col, "\\d+") %>% na.omit() %>% as.numeric()

  cat("  Found", length(eds), "Election Districts\n\n")
}

# ============================================================
# ALTERNATIVE: MANUAL PARSING APPROACH
# ============================================================
# The ENR HTML tables are complex. Let's read the raw CSV more carefully

cat("📋 Parsing election data structure...\n")

# Read line by line to find structure
lines <- read_lines("data/raw/election/ad34_enr/AD34_Mayor_20251116_1457.csv")

# Find the line with candidate names
candidate_line_idx <- which(str_detect(lines, "Zohran Kwame Mamdani.*Curtis A. Sliwa"))[1]

if (!is.na(candidate_line_idx)) {
  cat("  Found candidate header at line:", candidate_line_idx, "\n")

  # Parse candidate names from that line
  candidate_line <- lines[candidate_line_idx]

  # Split and extract candidate info
  # Format: "Zohran Kwame Mamdani Curtis A. Sliwa ... (Democratic) (Republican) ..."
}

# ============================================================
# SIMPLER APPROACH: PARSE FROM HTML DIRECTLY
# ============================================================

cat("\n🔄 Switching to HTML parsing for cleaner data extraction...\n\n")

library(rvest)

# Download fresh from ENR
mayor_url <- "https://enr.boenyc.gov/CD27286AD344.html"

cat("📥 Fetching live data from ENR...\n")
mayor_page <- read_html(mayor_url)

# Get the main results table
tables <- mayor_page %>% html_table(fill = TRUE)

# Find the table with ED data (usually the largest)
main_table <- tables[[which.max(sapply(tables, nrow))]]

cat("  Table dimensions:", nrow(main_table), "rows ×", ncol(main_table), "cols\n")

# The table structure typically has:
# Row 1: Headers with candidate names
# Following rows: ED data

# Clean the table
# First, find which row has "ED" labels
ed_row_start <- which(str_detect(main_table[[1]], "ED\\s+\\d+"))[1]

if (!is.na(ed_row_start)) {
  # Extract ED data
  ed_data <- main_table[ed_row_start:nrow(main_table), ]

  # Clean column names from header row
  header_row <- main_table[ed_row_start - 1, ] %>%
    unlist() %>%
    as.character()

  # Set column names
  names(ed_data) <- make.names(header_row, unique = TRUE)

  cat("  Extracted", nrow(ed_data), "Election Districts\n\n")

  # Extract ED numbers
  ed_data <- ed_data %>%
    mutate(
      ED = str_extract(.[[1]], "\\d+") %>% as.numeric()
    ) %>%
    filter(!is.na(ED))

  cat("✓ Parsed", nrow(ed_data), "Election Districts\n\n")
}

# ============================================================
# EXTRACT CANDIDATE TOTALS (COMBINING PARTY LINES)
# ============================================================

cat("🔢 Combining votes across party lines...\n\n")

# Manually extract key candidates and their vote columns
# This requires examining the actual column structure

# For now, let's create a function that will work once we identify the pattern

parse_mayor_results <- function(table_data) {
  # TODO: Extract and combine votes for each candidate
  # Mamdani appears on: Democratic, Working Families
  # Adams appears on: Safe&Affordable/EndAntiSemitism
  # Sliwa appears on: Republican
  # etc.

  # Return a clean dataframe with:
  # ED, Candidate, Total_Votes, Reported_Pct
}

# ============================================================
# SAVE CLEANED DATA
# ============================================================

cat("💾 Saving intermediate cleaned data...\n")

# For now, save what we have
saveRDS(list(
  raw_table = main_table,
  ed_data = if(exists("ed_data")) ed_data else NULL,
  timestamp = Sys.time()
), "data/intermediate/ad34_mayor_parsed.rds")

cat("✓ Saved to data/intermediate/ad34_mayor_parsed.rds\n\n")

cat("⚠️  DATA STRUCTURE ANALYSIS NEEDED\n")
cat("   The ENR HTML tables require careful parsing.\n")
cat("   Next steps:\n")
cat("   1. Examine the actual column structure\n")
cat("   2. Identify which columns contain vote counts for each candidate\n")
cat("   3. Map candidates to their multiple party lines\n")
cat("   4. Combine totals\n\n")

cat("📊 For now, let's work with the saved intermediate data\n\n")

# ============================================================
# FIND ED SHAPEFILES
# ============================================================

cat("🗺️  Searching for Election District shapefiles...\n\n")

# Check what shapefiles we have
shapefile_dirs <- c(
  "data/raw/election",
  "data/raw/census"
)

for (dir in shapefile_dirs) {
  if (dir.exists(dir)) {
    shapefiles <- list.files(dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shapefiles) > 0) {
      cat("  Found in", dir, ":\n")
      for (shp in shapefiles) {
        cat("    •", basename(shp), "\n")
      }
    }
  }
}

cat("\n💡 NEXT STEPS:\n")
cat("   1. Carefully parse ENR table structure\n")
cat("   2. Combine candidate votes across party lines\n")
cat("   3. Calculate turnout by ED\n")
cat("   4. Find or download ED shapefiles\n")
cat("   5. Match ED naming conventions\n")
cat("   6. Create turnout map\n\n")
