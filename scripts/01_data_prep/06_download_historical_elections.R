# ============================================================
# Download Historical Election Data for AD34
# ============================================================
# Scrapes vote.nyc archive pages for 2021-2024 elections
# Focus on General Elections with major races

library(tidyverse)
library(rvest)
library(httr)

setwd("~/Mayoral Results AD34")

cat("\n📚 Historical Election Data Downloader\n")
cat("======================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

BASE_URL <- "https://www.vote.nyc"
ASSEMBLY_DISTRICT <- "34"

# Archive pages to scrape
ARCHIVE_YEARS <- c(
  "2024" = "/page/election-results-summary-2024",  # Presidential
  "2022" = "/page/election-results-summary-2022",  # Governor
  "2021" = "/page/election-results-summary-2021"   # Mayor
)

# Create directories
dir.create("data/raw/election/ed_level", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw/election/ad34", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# SCRAPE EACH ARCHIVE YEAR
# ============================================================

all_downloads <- c()

for (year in names(ARCHIVE_YEARS)) {
  cat("📅 Scraping", year, "elections...\n")

  archive_url <- paste0(BASE_URL, ARCHIVE_YEARS[year])

  page <- tryCatch({
    read_html(archive_url)
  }, error = function(e) {
    cat("  ✗ Could not load page:", e$message, "\n")
    return(NULL)
  })

  if (is.null(page)) next

  # Extract CSV links
  csv_links <- page %>%
    html_elements("table a") %>%
    {data.frame(
      text = html_text(., trim = TRUE),
      href = html_attr(., "href"),
      stringsAsFactors = FALSE
    )} %>%
    filter(!is.na(href)) %>%
    filter(str_detect(href, "\\.csv$")) %>%
    filter(str_detect(href, "EDLevel")) %>%  # Only ED Level files
    mutate(
      full_url = if_else(
        str_starts(href, "http"),
        href,
        paste0(BASE_URL, href)
      ),
      # Generate filename
      filename = str_extract(href, "[^/]+\\.csv$") %>%
        str_replace_all("%20", "_"),
      # Check if Queens or citywide
      is_queens = str_detect(href, "Queens"),
      is_citywide = str_detect(text, regex("Mayor|President|Comptroller|Advocate|Governor", ignore_case = TRUE))
    ) %>%
    filter(is_queens | is_citywide)  # Get Queens races or citywide races

  cat("  Found", nrow(csv_links), "relevant ED Level CSVs\n")

  if (nrow(csv_links) == 0) {
    cat("  ⚠  No files found for", year, "\n\n")
    next
  }

  # Download files
  for (i in seq_len(nrow(csv_links))) {
    row <- csv_links[i, ]
    dest_path <- file.path("data/raw/election/ed_level", row$filename)

    # Skip if exists
    if (file.exists(dest_path)) {
      cat("  ⏭  ", row$filename, "\n")
      all_downloads <- c(all_downloads, dest_path)
      next
    }

    # Download
    result <- tryCatch({
      # Encode spaces
      url <- str_replace_all(row$full_url, " ", "%20")
      response <- GET(url, timeout(30))

      if (status_code(response) == 200) {
        writeBin(content(response, "raw"), dest_path)
        all_downloads <- c(all_downloads, dest_path)
        cat("  ✓ ", row$filename, "\n")
        TRUE
      } else {
        cat("  ✗ HTTP", status_code(response), "-", row$filename, "\n")
        FALSE
      }
    }, error = function(e) {
      cat("  ✗ Error:", row$filename, "\n")
      FALSE
    })

    Sys.sleep(0.3)
  }

  cat("\n")
}

cat("✓ Total files downloaded/found:", length(all_downloads), "\n\n")

# ============================================================
# FILTER FOR AD34
# ============================================================

cat("🔍 Filtering for AD34...\n\n")

ad34_files <- c()

for (csv_file in all_downloads) {
  result <- tryCatch({
    data <- read_csv(csv_file, show_col_types = FALSE, col_names = FALSE)

    # Handle NYC BOE ED Level CSV format
    # Columns 12+ contain actual data, column 12 is AD number
    if (ncol(data) >= 12) {
      names(data) <- c("AD_label", "ED_label", "County_label", "EDAD_Status_label",
                       "Event_label", "Party_label", "Office_label", "District_Key_label",
                       "VoteFor_label", "Unit_Name_label", "Tally_label",
                       "AD", "ED", "County", "EDAD_Status", "Event", "Party",
                       "Office", "District_Key", "VoteFor", "Unit_Name", "Tally")[1:ncol(data)]

      # Filter for AD34
      ad34_data <- data %>%
        filter(AD == ASSEMBLY_DISTRICT)

      if (nrow(ad34_data) > 0) {
        # Save AD34 version
        ad34_filename <- basename(csv_file) %>%
          str_replace("\\.csv$", "_AD34.csv")
        ad34_path <- file.path("data/raw/election/ad34", ad34_filename)

        # Keep only data columns
        ad34_clean <- ad34_data %>% select(12:ncol(data))
        write_csv(ad34_clean, ad34_path)

        ad34_files <- c(ad34_files, ad34_path)
        cat("  ✓", basename(csv_file), "→", nrow(ad34_data), "rows\n")
      }
    }

    TRUE
  }, error = function(e) {
    # Skip files that can't be parsed
    FALSE
  })
}

cat("\n✓ Created", length(ad34_files), "AD34-filtered datasets\n\n")

# ============================================================
# SUMMARY
# ============================================================

cat(rep("=", 60), "\n")
cat("📊 DOWNLOAD SUMMARY\n")
cat(rep("=", 60), "\n")
cat("Historical files processed:", length(all_downloads), "\n")
cat("AD34 datasets created:", length(ad34_files), "\n")
cat("\n📂 Locations:\n")
cat("  • All files: data/raw/election/ed_level/\n")
cat("  • AD34 filtered: data/raw/election/ad34/\n\n")

if (length(ad34_files) > 0) {
  cat("📋 AD34 Files:\n")
  for (f in ad34_files) {
    cat("  •", basename(f), "\n")
  }
}

cat("\n✅ Historical download complete!\n\n")
