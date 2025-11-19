# ============================================================
# NYC Board of Elections - Election Results Downloader
# ============================================================
# Downloads election results from vote.nyc in multiple formats
# Handles: CSV (Recap & ED Level), PDF, HTML, Excel files
# Source: https://vote.nyc/page/election-results-summary

library(tidyverse)
library(rvest)
library(httr)
library(stringr)

setwd("~/Mayoral Results AD34")

# Create download directories
dir.create("data/raw/election/downloads", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw/election/unofficial", showWarnings = FALSE, recursive = TRUE)

cat("\n🗳️  NYC Election Results Downloader\n")
cat("=====================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

BASE_URL <- "https://vote.nyc"
RESULTS_PAGE <- "https://vote.nyc/page/election-results-summary"

# Which data to download (set to TRUE/FALSE)
CONFIG <- list(
  download_csv_recap = TRUE,      # Summary results by contest
  download_csv_ed = TRUE,          # Detailed results by Election District
  download_pdf_recap = FALSE,      # PDF summaries (less useful for analysis)
  download_pdf_ed = FALSE,         # PDF details (less useful for analysis)
  download_unofficial = TRUE,      # Current unofficial results from enr.boenyc.gov
  max_elections = 3,               # How many recent elections to download (NULL = all)
  assembly_district = "34"         # Filter for specific assembly district
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Clean filename for saving
clean_filename <- function(text) {
  text %>%
    str_replace_all("[^[:alnum:]_-]", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_trunc(200)
}

# Download file with error handling
safe_download <- function(url, dest_path, description = "") {
  tryCatch({
    response <- GET(url, timeout(30))
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), dest_path)
      cat("  ✓", description, "\n")
      return(TRUE)
    } else {
      cat("  ✗", description, "- HTTP", status_code(response), "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("  ✗", description, "- Error:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================
# SCRAPE MAIN RESULTS PAGE
# ============================================================

cat("📥 Fetching election results page...\n")
page <- read_html(RESULTS_PAGE)

# Extract all tables containing election results
tables <- page %>% html_elements("table")

cat("✓ Found", length(tables), "tables on page\n")

# Extract all links from tables
result_links <- page %>%
  html_elements("table a") %>%
  {data.frame(
    text = html_text(., trim = TRUE),
    href = html_attr(., "href"),
    stringsAsFactors = FALSE
  )} %>%
  filter(!is.na(href)) %>%
  # Filter for actual result files - be more inclusive
  filter(str_detect(href, "election_results") |
         str_detect(href, "\\.(csv|pdf|xls|xlsx|html|zip)$")) %>%
  # Remove anchors and non-file links
  filter(!str_detect(href, "^#|^javascript:|^mailto:"))

# Identify file types and levels
result_links <- result_links %>%
  mutate(
    file_type = str_extract(href, "\\.(csv|pdf|xls|xlsx|html|zip)$"),
    is_ed_level = str_detect(href, regex("EDLevel|ED_Level", ignore_case = TRUE)) |
                  str_detect(text, regex("ED Level|Election District", ignore_case = TRUE)),
    is_recap = str_detect(href, regex("Recap", ignore_case = TRUE)) |
               str_detect(text, regex("Recap|Summary", ignore_case = TRUE)),
    # Build full URL (handle both absolute and relative paths)
    full_url = case_when(
      str_starts(href, "http") ~ href,
      str_starts(href, "/") ~ paste0(BASE_URL, href),
      TRUE ~ paste0(BASE_URL, "/", href)
    ),
    # Extract election date from path if available
    election_date = str_extract(href, "\\d{8}[^/]*"),
    # Clean up the text description
    description = str_squish(text)
  ) %>%
  filter(description != "", !is.na(file_type)) %>%
  # Remove any rows with invalid URLs
  filter(!str_detect(full_url, "^http.*\\s"))

cat("✓ Found", nrow(result_links), "downloadable result files\n\n")

# DEBUG: Show first few URLs
if (nrow(result_links) > 0) {
  cat("📝 Sample URLs (first 3):\n")
  for (i in 1:min(3, nrow(result_links))) {
    cat("  ", i, ":", result_links$full_url[i], "\n")
  }
  cat("\n")
}

# ============================================================
# DOWNLOAD CSV FILES
# ============================================================

if (CONFIG$download_csv_recap || CONFIG$download_csv_ed) {
  cat("📊 Downloading CSV files...\n")

  csv_links <- result_links %>%
    filter(file_type == ".csv") %>%
    filter(
      (CONFIG$download_csv_recap & is_recap) |
      (CONFIG$download_csv_ed & is_ed_level)
    )

  if (!is.null(CONFIG$max_elections)) {
    csv_links <- csv_links %>% slice_head(n = CONFIG$max_elections * 2)
  }

  cat("Found", nrow(csv_links), "CSV files to download\n")

  for (i in seq_len(nrow(csv_links))) {
    row <- csv_links[i, ]
    filename <- clean_filename(paste0(row$description, "_", basename(row$href)))
    dest_path <- file.path("data/raw/election/downloads", filename)

    # Don't URL encode the entire URL - spaces are already handled
    url_to_download <- row$href
    if (!str_starts(url_to_download, "http")) {
      url_to_download <- paste0(BASE_URL, url_to_download)
    }

    safe_download(url_to_download, dest_path, row$description)
    Sys.sleep(0.5)  # Be nice to the server
  }

  cat("\n")
}

# ============================================================
# DOWNLOAD PDF FILES (Optional)
# ============================================================

if (CONFIG$download_pdf_recap || CONFIG$download_pdf_ed) {
  cat("📄 Downloading PDF files...\n")

  pdf_links <- result_links %>%
    filter(file_type == ".pdf") %>%
    filter(
      (CONFIG$download_pdf_recap & is_recap) |
      (CONFIG$download_pdf_ed & is_ed_level)
    )

  if (!is.null(CONFIG$max_elections)) {
    pdf_links <- pdf_links %>% slice_head(n = CONFIG$max_elections * 2)
  }

  for (i in seq_len(nrow(pdf_links))) {
    row <- pdf_links[i, ]
    filename <- clean_filename(paste0(row$text, "_", basename(row$href)))
    dest_path <- file.path("data/raw/election/downloads", filename)

    safe_download(row$full_url, dest_path, row$text)
    Sys.sleep(0.5)
  }

  cat("\n")
}

# ============================================================
# DOWNLOAD UNOFFICIAL RESULTS (ENR)
# ============================================================

if (CONFIG$download_unofficial) {
  cat("🔴 Checking for unofficial results...\n")

  # Find the unofficial results link (typically goes to enr.boenyc.gov)
  unofficial_link <- page %>%
    html_elements("a") %>%
    {data.frame(
      text = html_text(., trim = TRUE),
      href = html_attr(., "href"),
      stringsAsFactors = FALSE
    )} %>%
    filter(str_detect(text, regex("unofficial.*results.*2025", ignore_case = TRUE)) |
           str_detect(href, "enr\\.boenyc\\.gov")) %>%
    slice(1)

  if (nrow(unofficial_link) > 0) {
    cat("  ✓ Found:", unofficial_link$text, "\n")
    cat("  📍 URL:", unofficial_link$href, "\n\n")

    # Try to scrape the unofficial results page (ENR system)
    tryCatch({
      cat("  Fetching unofficial results page...\n")
      enr_page <- read_html(unofficial_link$href)

      # ENR pages use tables for results
      tables <- enr_page %>% html_table(fill = TRUE)

      if (length(tables) > 0) {
        cat("  ✓ Found", length(tables), "tables\n")

        # Save all tables
        for (i in seq_along(tables)) {
          table_data <- tables[[i]]

          # Skip empty tables
          if (nrow(table_data) > 0 && ncol(table_data) > 0) {
            # Try to identify what the table contains
            table_name <- if (i == 1) "main_results" else paste0("table_", i)

            table_file <- file.path("data/raw/election/unofficial",
                                   paste0("unofficial_", table_name, "_",
                                          format(Sys.Date(), "%Y%m%d"), ".csv"))
            write_csv(table_data, table_file)
            cat("    → Saved", basename(table_file),
                paste0("(", nrow(table_data), " rows)\n"))
          }
        }

        cat("\n  💡 Tip: Check the ENR page directly for interactive features\n")
        cat("     ", unofficial_link$href, "\n")

      } else {
        cat("  ⚠ No tables found - results may be in a different format\n")
        cat("  Visit the page directly:", unofficial_link$href, "\n")
      }
    }, error = function(e) {
      cat("  ⚠ Could not automatically parse unofficial results\n")
      cat("  Error:", e$message, "\n")
      cat("  📌 Visit manually:", unofficial_link$href, "\n")
    })
  } else {
    cat("  ℹ No current unofficial results link found\n")
    cat("  (This is normal between elections)\n")
  }

  cat("\n")
}

# ============================================================
# FILTER FOR ASSEMBLY DISTRICT (if specified)
# ============================================================

if (!is.null(CONFIG$assembly_district)) {
  cat("🔍 Filtering for Assembly District", CONFIG$assembly_district, "...\n")

  # Look for downloaded CSV files that might contain AD data
  csv_files <- list.files("data/raw/election/downloads",
                          pattern = "\\.csv$", full.names = TRUE)

  ad_pattern <- paste0("AD", CONFIG$assembly_district, "|",
                       "Assembly District ", CONFIG$assembly_district, "|",
                       "District ", CONFIG$assembly_district)

  for (csv_file in csv_files) {
    tryCatch({
      data <- read_csv(csv_file, show_col_types = FALSE)

      # Try to find AD column
      ad_cols <- names(data)[str_detect(names(data),
                                        regex("district|AD|assembly", ignore_case = TRUE))]

      if (length(ad_cols) > 0) {
        # Filter for the specified AD
        ad_data <- data %>%
          filter(if_any(all_of(ad_cols), ~ str_detect(as.character(.), CONFIG$assembly_district)))

        if (nrow(ad_data) > 0) {
          filtered_file <- str_replace(basename(csv_file), "\\.csv$",
                                      paste0("_AD", CONFIG$assembly_district, ".csv"))
          filtered_path <- file.path("data/raw/election", filtered_file)
          write_csv(ad_data, filtered_path)
          cat("  ✓ Extracted AD", CONFIG$assembly_district, "data to",
              basename(filtered_path), "\n")
        }
      }
    }, error = function(e) {
      # Skip files that can't be parsed
    })
  }

  cat("\n")
}

# ============================================================
# SUMMARY
# ============================================================

cat("📋 Download Summary\n")
cat("===================\n")
cat("Downloaded files:", length(list.files("data/raw/election/downloads")), "\n")
cat("Location: data/raw/election/downloads/\n")

if (dir.exists("data/raw/election/unofficial") &&
    length(list.files("data/raw/election/unofficial")) > 0) {
  cat("Unofficial results:", length(list.files("data/raw/election/unofficial")),
      "files\n")
  cat("Location: data/raw/election/unofficial/\n")
}

cat("\n✅ Download complete!\n\n")

# ============================================================
# USAGE NOTES
# ============================================================
cat("📖 Next Steps:\n")
cat("1. Check data/raw/election/downloads/ for downloaded files\n")
cat("2. Review CSV files for the data you need\n")
cat("3. Use scripts in scripts/03_election_analysis/ to analyze results\n")
cat("4. Adjust CONFIG at top of script to customize downloads\n")
