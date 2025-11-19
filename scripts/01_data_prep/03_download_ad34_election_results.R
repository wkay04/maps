# ============================================================
# NYC AD34 Election Results Downloader
# ============================================================
# Downloads Election District level results for Assembly District 34
# Source: https://vote.nyc/page/election-results-summary
#
# This script:
# 1. Scrapes vote.nyc for EDLevel CSV files
# 2. Downloads them
# 3. Filters for AD34 data only
# 4. Saves clean, AD34-specific datasets

library(tidyverse)
library(rvest)
library(httr)

setwd("~/Mayoral Results AD34")

# Create directories
dir.create("data/raw/election/ed_level", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw/election/ad34", showWarnings = FALSE, recursive = TRUE)

cat("\n🗳️  NYC AD34 Election Results Downloader\n")
cat("=========================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

VOTE_NYC_URL <- "https://vote.nyc/page/election-results-summary"
ASSEMBLY_DISTRICT <- "34"
BASE_URL <- "https://www.vote.nyc"

# Which elections to download (most recent N)
MAX_ELECTIONS <- 20  # Download more elections for historical analysis

# Which types of files to download
CONFIG <- list(
  download_edlevel = TRUE,      # ED Level detail (recommended)
  download_recap = FALSE,        # Recap summaries (less detailed)
  filter_queens_only = FALSE,   # Get all boroughs for citywide races
  prioritize_general = TRUE      # Focus on General Elections (Mayor, President, etc.)
)

# ============================================================
# SCRAPE CSV LINKS FROM VOTE.NYC
# ============================================================

cat("📥 Fetching election results page...\n")
page <- read_html(VOTE_NYC_URL)

cat("🔍 Finding CSV download links...\n\n")

# Extract all CSV links from the page
csv_links <- page %>%
  html_elements("table a") %>%
  {data.frame(
    text = html_text(., trim = TRUE),
    href = html_attr(., "href"),
    stringsAsFactors = FALSE
  )} %>%
  filter(!is.na(href)) %>%
  filter(str_detect(href, "\\.csv$")) %>%
  mutate(
    # Categorize file types
    is_edlevel = str_detect(href, "EDLevel"),
    is_recap = str_detect(href, "Recap"),

    # Extract election info
    election_date = str_extract(href, "\\d{8}"),
    year = str_sub(election_date, 1, 4),

    # Identify Manhattan/citywide races
    is_manhattan = str_detect(href, "New York"),

    # Identify Queens races (AD34 is in Queens!)
    is_queens = str_detect(href, "Queens"),

    # Identify specific AD34 races
    is_ad34 = str_detect(href, " 34") | str_detect(href, "034") | str_detect(href, "34th"),

    # Generate clean filename from href (more reliable than text)
    filename = str_extract(href, "[^/]+\\.csv$") %>%
      str_replace_all("%20", "_"),

    # Clean description
    description = if_else(text == "", "Unknown", str_squish(text)),

    # Build full URL - don't modify the href, let httr handle encoding
    full_url = if_else(
      str_starts(href, "http"),
      href,
      paste0(BASE_URL, href)
    )
  )

cat("✓ Found", nrow(csv_links), "total CSV files\n")

# ============================================================
# FILTER FOR RELEVANT FILES
# ============================================================

# Filter based on config
relevant_links <- csv_links %>%
  filter(
    # File type
    (CONFIG$download_edlevel & is_edlevel) | (CONFIG$download_recap & is_recap)
  ) %>%
  filter(
    # Geography - for historical analysis, get Queens races OR citywide races from any borough
    is_queens | is_manhattan | is_ad34 |
    # Also get any file that mentions major citywide offices
    str_detect(description, regex("Mayor|President|Comptroller|Advocate|Governor", ignore_case = TRUE))
  ) %>%
  # Prioritize General Elections if requested
  mutate(
    is_general = str_detect(href, "General"),
    is_primary = str_detect(href, "Primary")
  ) %>%
  {if(CONFIG$prioritize_general) arrange(., desc(is_general), desc(election_date)) else arrange(., desc(election_date))} %>%
  group_by(election_date) %>%
  slice_head(n = 100) %>%  # Max 100 files per election
  ungroup() %>%
  slice_head(n = MAX_ELECTIONS * 100)  # Total cap

cat("✓ Filtered to", nrow(relevant_links), "relevant files\n")
cat("  •", sum(relevant_links$is_edlevel), "ED Level files\n")
cat("  •", sum(relevant_links$is_recap), "Recap files\n\n")

if (nrow(relevant_links) == 0) {
  cat("❌ No relevant files found to download\n")
  quit(save = "no")
}

# Show sample of what will be downloaded
cat("📋 Sample files to download:\n")
for (i in 1:min(5, nrow(relevant_links))) {
  cat("  ", i, ":", relevant_links$description[i], "\n")
}
cat("\n")

# ============================================================
# DOWNLOAD CSV FILES
# ============================================================

cat("📥 Downloading", nrow(relevant_links), "files...\n")
cat("(This may take a few minutes)\n\n")

downloaded_files <- c()
failed_downloads <- c()

for (i in seq_len(nrow(relevant_links))) {
  row <- relevant_links[i,]

  dest_path <- file.path("data/raw/election/ed_level", row$filename)

  # Skip if already downloaded
  if (file.exists(dest_path)) {
    cat("  ⏭  Already exists:", row$filename, "\n")
    downloaded_files <- c(downloaded_files, dest_path)
    next
  }

  # Download - manually encode only the spaces in the path
  result <- tryCatch({
    # URL encode only the path portion (replace spaces with %20)
    encoded_url <- str_replace_all(row$full_url, " ", "%20")

    response <- GET(encoded_url, timeout(30))
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), dest_path)
      downloaded_files <- c(downloaded_files, dest_path)
      cat("  ✓", i, "of", nrow(relevant_links), ":", row$filename, "\n")
      TRUE
    } else {
      cat("  ✗ HTTP", status_code(response), ":", row$filename, "\n")
      failed_downloads <- c(failed_downloads, row$filename)
      FALSE
    }
  }, error = function(e) {
    cat("  ✗ Error:", row$filename, "-", e$message, "\n")
    failed_downloads <- c(failed_downloads, row$filename)
    FALSE
  })

  Sys.sleep(0.3)  # Be nice to the server
}

cat("\n✓ Downloaded:", length(downloaded_files), "files\n")
if (length(failed_downloads) > 0) {
  cat("✗ Failed:", length(failed_downloads), "files\n")
}

# ============================================================
# FILTER FOR AD34
# ============================================================

cat("\n🔍 Filtering for Assembly District", ASSEMBLY_DISTRICT, "...\n\n")

ad34_files <- c()
files_without_ad <- c()

for (csv_file in downloaded_files) {
  result <- tryCatch({
    # Read CSV - the files have headers mixed with data, actual AD is in column 12
    data <- read_csv(csv_file, show_col_types = FALSE, col_names = FALSE)

    # The NYC BOE ED Level files have this structure:
    # Cols 1-11: header labels (AD, ED, County, etc.)
    # Cols 12-22: actual data values (34, 001, Queens, etc.)
    # We need column 12 which is the actual AD number

    if (ncol(data) >= 12) {
      # Use proper column names
      names(data) <- c("AD_label", "ED_label", "County_label", "EDAD_Status_label",
                       "Event_label", "Party_label", "Office_label", "District_Key_label",
                       "VoteFor_label", "Unit_Name_label", "Tally_label",
                       "AD", "ED", "County", "EDAD_Status", "Event", "Party",
                       "Office", "District_Key", "VoteFor", "Unit_Name", "Tally")[1:ncol(data)]

      # Filter for AD34
      ad34_data <- data %>%
        filter(AD == ASSEMBLY_DISTRICT)

      if (nrow(ad34_data) > 0) {
        # Save AD34-only version - keep only the actual data columns
        ad34_clean <- ad34_data %>%
          select(12:ncol(data))  # Select only data columns, not label columns

        ad34_filename <- basename(csv_file) %>%
          str_replace("\\.csv$", "_AD34.csv")
        ad34_path <- file.path("data/raw/election/ad34", ad34_filename)

        write_csv(ad34_clean, ad34_path)
        ad34_files <- c(ad34_files, ad34_path)

        cat("  ✓", basename(csv_file), "→", nrow(ad34_data), "rows for AD34\n")
      } else {
        cat("  ℹ ", basename(csv_file), "- no AD34 data\n")
      }
    } else {
      # Unusual structure
      files_without_ad <- c(files_without_ad, csv_file)
    }

    TRUE
  }, error = function(e) {
    cat("  ✗ Error reading", basename(csv_file), ":", e$message, "\n")
    FALSE
  })
}

# Handle files without AD column (citywide data)
if (length(files_without_ad) > 0) {
  cat("\n📊", length(files_without_ad), "files have no AD column (citywide/summary data)\n")
  cat("   These files saved in: data/raw/election/ed_level/\n")
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("📊 DOWNLOAD SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat("Total downloads:", length(downloaded_files), "files\n")
cat("AD34 datasets:", length(ad34_files), "files\n")
cat("Failed downloads:", length(failed_downloads), "files\n\n")

cat("📂 Locations:\n")
cat("  • All downloads: data/raw/election/ed_level/\n")
cat("  • AD34 filtered: data/raw/election/ad34/\n\n")

if (length(ad34_files) > 0) {
  cat("📋 AD34 Files Created:\n")
  for (f in head(ad34_files, 10)) {
    cat("  •", basename(f), "\n")
  }
  if (length(ad34_files) > 10) {
    cat("  ... and", length(ad34_files) - 10, "more\n")
  }
}

cat("\n💡 TIPS:\n")
cat("  • Re-run this script after elections for updated results\n")
cat("  • Adjust MAX_ELECTIONS config to download more/fewer elections\n")
cat("  • Set filter_queens_only=FALSE to get all boroughs\n")
cat("  • AD34 is in Queens (Jackson Heights, Corona, East Elmhurst, Astoria)\n\n")

cat("✅ Download complete!\n\n")
