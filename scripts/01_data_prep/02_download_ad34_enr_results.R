# ============================================================
# NYC ENR - Assembly District 34 Unofficial Results Downloader
# ============================================================
# Downloads live unofficial election results for AD34
# Source: https://enr.boenyc.gov (Election Night Reporting system)
#
# This script focuses on getting current, unofficial results during
# and immediately after elections. For official certified results,
# use the precinct-level data from BOE.

library(tidyverse)
library(rvest)
library(httr)

setwd("~/Mayoral Results AD34")

# Create download directory
dir.create("data/raw/election/unofficial", showWarnings = FALSE, recursive = TRUE)

cat("\n🗳️  NYC ENR - AD34 Unofficial Results Downloader\n")
cat("================================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

ENR_BASE_URL <- "https://enr.boenyc.gov"
VOTE_NYC_URL <- "https://vote.nyc/page/election-results-summary"
ASSEMBLY_DISTRICT <- "34"

# ============================================================
# FIND CURRENT UNOFFICIAL RESULTS LINK
# ============================================================

cat("🔍 Checking for current unofficial results...\n")

vote_page <- read_html(VOTE_NYC_URL)

# Find the ENR link
enr_link <- vote_page %>%
  html_elements("a") %>%
  {data.frame(
    text = html_text(., trim = TRUE),
    href = html_attr(., "href")
  )} %>%
  filter(str_detect(text, regex("unofficial.*results", ignore_case = TRUE)) |
         str_detect(href, "enr\\.boenyc\\.gov")) %>%
  slice(1)

if (nrow(enr_link) == 0) {
  cat("❌ No unofficial results currently available\n")
  cat("   This is normal between elections.\n\n")
  cat("💡 For official results, check data/raw/election/Precinct results.csv\n")
  quit(save = "no")
}

cat("✅ Found:", enr_link$text, "\n")
cat("📍 URL:", enr_link$href, "\n\n")

# ============================================================
# DOWNLOAD UNOFFICIAL RESULTS
# ============================================================

cat("📥 Downloading unofficial results tables...\n")

# Try to read the ENR page with error handling
enr_page <- tryCatch({
  # Use httr::GET with a user agent to avoid blocks
  response <- GET(enr_link$href,
                 user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
                 timeout(30))

  if (status_code(response) != 200) {
    stop(paste("HTTP status:", status_code(response)))
  }

  read_html(content(response, as = "text"))
}, error = function(e) {
  cat("❌ Could not access ENR page\n")
  cat("   Error:", e$message, "\n\n")
  cat("💡 The ENR system may be:\n")
  cat("   - Temporarily down\n")
  cat("   - Blocking automated access\n")
  cat("   - Only available during active elections\n\n")
  cat("📌 Visit manually:", enr_link$href, "\n")
  return(NULL)
})

if (is.null(enr_page)) {
  quit(save = "no")
}

# Get all tables from the ENR page
tables <- enr_page %>% html_table(fill = TRUE)

if (length(tables) == 0) {
  cat("⚠️  No tables found on ENR page\n")
  cat("   The page may use JavaScript or a different format.\n")
  cat("   Visit manually:", enr_link$href, "\n")
  quit(save = "no")
}

cat("✓ Found", length(tables), "result tables\n\n")

# Save all tables
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
saved_files <- c()

for (i in seq_along(tables)) {
  table_data <- tables[[i]]

  # Skip empty tables
  if (nrow(table_data) == 0 || ncol(table_data) == 0) {
    next
  }

  # Determine table type from content
  table_name <- case_when(
    i == 1 ~ "main_results",
    any(str_detect(tolower(names(table_data)), "turnout")) ~ "turnout",
    any(str_detect(tolower(names(table_data)), "district")) ~ "by_district",
    TRUE ~ paste0("table_", i)
  )

  # Save with timestamp
  output_file <- file.path("data/raw/election/unofficial",
                          paste0("ENR_", table_name, "_", timestamp, ".csv"))

  write_csv(table_data, output_file)
  saved_files <- c(saved_files, output_file)

  cat("  ✓ Saved:", basename(output_file),
      paste0("(", nrow(table_data), " rows, ", ncol(table_data), " cols)\n"))
}

# ============================================================
# LOOK FOR DOWNLOADABLE FILES ON ENR PAGE
# ============================================================

cat("\n🔗 Checking for downloadable files on ENR page...\n")

enr_links <- enr_page %>%
  html_elements("a") %>%
  {data.frame(
    text = html_text(., trim = TRUE),
    href = html_attr(., "href")
  )} %>%
  filter(str_detect(href, "\\.(csv|xlsx|xls|pdf)$"))

if (nrow(enr_links) > 0) {
  cat("  Found", nrow(enr_links), "downloadable files:\n")

  for (i in seq_len(min(nrow(enr_links), 10))) {  # Show first 10
    cat("    •", enr_links$text[i], "-", enr_links$href[i], "\n")
  }

  cat("\n  💾 To download these files, add download logic or visit page manually\n")
} else {
  cat("  No additional downloadable files found\n")
}

# ============================================================
# FILTER FOR ASSEMBLY DISTRICT 34
# ============================================================

cat("\n🔍 Filtering data for Assembly District", ASSEMBLY_DISTRICT, "...\n")

ad34_files <- c()

for (csv_file in saved_files) {
  data <- read_csv(csv_file, show_col_types = FALSE)

  # Look for columns that might contain district info
  district_cols <- names(data)[str_detect(tolower(names(data)),
                                          "district|ad|assembly|ed")]

  if (length(district_cols) > 0) {
    # Try to filter for AD34
    ad34_data <- data %>%
      filter(if_any(all_of(district_cols),
                   ~ str_detect(as.character(.), ASSEMBLY_DISTRICT)))

    if (nrow(ad34_data) > 0) {
      # Save filtered version
      ad34_file <- str_replace(csv_file, "\\.csv$",
                              paste0("_AD", ASSEMBLY_DISTRICT, ".csv"))
      write_csv(ad34_data, ad34_file)
      ad34_files <- c(ad34_files, ad34_file)

      cat("  ✓ Filtered", basename(csv_file), "→",
          basename(ad34_file),
          paste0("(", nrow(ad34_data), " rows)\n"))
    }
  } else {
    # If no district column, it might be citywide data - keep it
    cat("  ℹ ", basename(csv_file), "- no district column (likely citywide)\n")
  }
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n" , rep("=", 60), "\n", sep = "")
cat("📊 SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat("Downloaded:", length(saved_files), "files\n")
cat("Filtered for AD34:", length(ad34_files), "files\n")
cat("Location: data/raw/election/unofficial/\n")
cat("Timestamp:", timestamp, "\n\n")

cat("📂 Files saved:\n")
for (f in saved_files) {
  cat("  •", basename(f), "\n")
}

if (length(ad34_files) > 0) {
  cat("\n📂 AD34-filtered files:\n")
  for (f in ad34_files) {
    cat("  •", basename(f), "\n")
  }
}

cat("\n💡 TIP: Re-run this script during election night to get updated results\n")
cat("📌 ENR Page:", enr_link$href, "\n\n")

cat("✅ Download complete!\n\n")
