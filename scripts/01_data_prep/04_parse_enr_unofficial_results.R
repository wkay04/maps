# ============================================================
# NYC ENR Unofficial Results Parser - AD34
# ============================================================
# Parses live unofficial election results from ENR system
# Focuses on citywide races (Mayor, Public Advocate, Comptroller)
# Source: https://enr.boenyc.gov

library(tidyverse)
library(rvest)
library(httr)

setwd("~/Mayoral Results AD34")

# Create directories
dir.create("data/raw/election/enr_results", showWarnings = FALSE, recursive = TRUE)

cat("\n🗳️  NYC ENR Unofficial Results Parser\n")
cat("======================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

ENR_BASE_URL <- "https://enr.boenyc.gov"
ASSEMBLY_DISTRICT <- "34"

# Define contests to download (citywide races only, no judges)
CONTESTS <- list(
  list(name = "Mayor", file = "CD27286ADI0.html"),
  list(name = "Public_Advocate", file = "CD27287ADI0.html"),
  list(name = "Comptroller", file = "CD27293ADI0.html"),
  list(name = "Borough_President", file = "OF15AD0PY1.html"),
  list(name = "City_Council", file = "OF18AD0PY3.html"),
  list(name = "Proposal_1", file = "CD27305ADI0.html"),
  list(name = "Proposal_2", file = "CD27306ADI0.html"),
  list(name = "Proposal_3", file = "CD27307ADI0.html"),
  list(name = "Proposal_4", file = "CD27308ADI0.html"),
  list(name = "Proposal_5", file = "CD27309ADI0.html"),
  list(name = "Proposal_6", file = "CD27310ADI0.html")
)

# ============================================================
# DOWNLOAD AND PARSE EACH CONTEST
# ============================================================

cat("📥 Downloading and parsing", length(CONTESTS), "contests...\n\n")

all_results <- list()
downloaded_count <- 0
failed_count <- 0

for (contest in CONTESTS) {
  cat("  📊 Processing:", contest$name, "...\n")

  contest_url <- paste0(ENR_BASE_URL, "/", contest$file)

  result <- tryCatch({
    # Download page
    response <- GET(contest_url,
                   user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
                   timeout(30))

    if (status_code(response) != 200) {
      cat("     ✗ HTTP", status_code(response), "\n")
      failed_count <<- failed_count + 1
      return(NULL)
    }

    # Parse HTML
    page <- read_html(content(response, as = "text"))

    # Extract all tables
    tables <- page %>% html_table(fill = TRUE)

    if (length(tables) == 0) {
      cat("     ⚠  No tables found\n")
      failed_count <<- failed_count + 1
      return(NULL)
    }

    cat("     ✓ Found", length(tables), "table(s)\n")

    # Save each table
    for (i in seq_along(tables)) {
      table_data <- tables[[i]]

      if (nrow(table_data) > 0 && ncol(table_data) > 0) {
        # Try to identify if this has AD data
        # Look for columns that might indicate Assembly District
        has_ad_data <- any(str_detect(tolower(names(table_data)), "district|assembly")) ||
                       any(str_detect(tolower(as.character(table_data[,1])), "district|assembly"))

        # Save the table
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
        filename <- paste0("ENR_", contest$name, "_table", i, "_", timestamp, ".csv")
        output_path <- file.path("data/raw/election/enr_results", filename)

        write_csv(table_data, output_path)
        cat("     → Saved:", filename,
            paste0("(", nrow(table_data), " rows, ", ncol(table_data), " cols)\n"))

        all_results[[paste0(contest$name, "_", i)]] <- list(
          contest = contest$name,
          table_num = i,
          data = table_data,
          file = output_path
        )
      }
    }

    downloaded_count <<- downloaded_count + 1
    Sys.sleep(0.5)  # Be nice to the server
    TRUE

  }, error = function(e) {
    cat("     ✗ Error:", e$message, "\n")
    failed_count <<- failed_count + 1
    FALSE
  })
}

cat("\n✓ Downloaded:", downloaded_count, "contests\n")
cat("✗ Failed:", failed_count, "contests\n\n")

# ============================================================
# LOOK FOR AD34 DATA IN THE RESULTS
# ============================================================

cat("🔍 Searching for AD34 data in downloaded tables...\n\n")

ad34_found <- list()

for (result_key in names(all_results)) {
  result <- all_results[[result_key]]
  data <- result$data

  # Try multiple strategies to find AD34
  found <- FALSE

  # Strategy 1: Look for "34" or "District 34" in any column
  for (col in names(data)) {
    if (any(str_detect(as.character(data[[col]]), "\\b34\\b|District 34"), na.rm = TRUE)) {
      cat("  ✓ Found AD34 in", result$contest, "table", result$table_num,
          "column:", col, "\n")
      ad34_found[[result_key]] <- result
      found <- TRUE
      break
    }
  }

  # Strategy 2: Look in first column for district numbers
  if (!found && ncol(data) > 0) {
    first_col_vals <- as.character(data[[1]])
    if (any(first_col_vals == "34", na.rm = TRUE)) {
      cat("  ✓ Found AD34 in", result$contest, "table", result$table_num,
          "(first column)\n")
      ad34_found[[result_key]] <- result
      found <- TRUE
    }
  }
}

if (length(ad34_found) == 0) {
  cat("  ⚠  No AD34-specific data found in tables\n")
  cat("     (Tables may show borough-level or citywide aggregates)\n")
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("📊 SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat("Contests downloaded:", downloaded_count, "\n")
cat("Total tables saved:", length(all_results), "\n")
cat("Tables with AD34 data:", length(ad34_found), "\n")
cat("Location: data/raw/election/enr_results/\n\n")

cat("📋 Files saved:\n")
saved_files <- sapply(all_results, function(x) basename(x$file))
for (f in unique(saved_files)) {
  cat("  •", f, "\n")
}

cat("\n💡 NOTES:\n")
cat("  • ENR 'AD Details' pages may show borough-level aggregates\n")
cat("  • For true AD-level data, use the vote.nyc ED Level files\n")
cat("  • These are UNOFFICIAL results, updated during election night\n")
cat("  • Re-run during elections for live updates\n\n")

cat("✅ Download complete!\n\n")
