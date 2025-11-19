# ============================================================
# NYC ENR - AD34 Specific Results Downloader
# ============================================================
# Downloads Assembly District 34 Election District level results
# from the ENR unofficial results system
# Source: https://enr.boenyc.gov
#
# Key Discovery: ENR has AD-specific pages with ED-level data!
# URL Pattern: [ContestCode]AD344.html for AD34

library(tidyverse)
library(rvest)
library(httr)

setwd("~/Mayoral Results AD34")

# Create directory
dir.create("data/raw/election/ad34_enr", showWarnings = FALSE, recursive = TRUE)

cat("\n🗳️  NYC ENR - AD34 Specific Results Downloader\n")
cat("===============================================\n\n")

# ============================================================
# CONFIGURATION
# ============================================================

ENR_BASE_URL <- "https://enr.boenyc.gov"
AD34_CODE <- "344"  # AD34 is coded as "344" in ENR system

# Define contests to download (citywide races, no judges)
CONTESTS <- list(
  list(name = "Mayor", code = "CD27286"),
  list(name = "Public_Advocate", code = "CD27287"),
  list(name = "Comptroller", code = "CD27293"),
  list(name = "Proposal_1", code = "CD27305"),
  list(name = "Proposal_2", code = "CD27306"),
  list(name = "Proposal_3", code = "CD27307"),
  list(name = "Proposal_4", code = "CD27308"),
  list(name = "Proposal_5", code = "CD27309"),
  list(name = "Proposal_6", code = "CD27310")
)

# ============================================================
# DOWNLOAD AD34-SPECIFIC PAGES
# ============================================================

cat("📥 Downloading AD34-specific results for", length(CONTESTS), "contests...\n\n")

downloaded_count <- 0
failed_count <- 0
all_data <- list()

for (contest in CONTESTS) {
  cat("  📊 Processing:", contest$name, "...\n")

  # Build AD34-specific URL
  contest_url <- paste0(ENR_BASE_URL, "/", contest$code, "AD", AD34_CODE, ".html")

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

    # Find the main results table (usually the largest one)
    main_table <- NULL
    max_rows <- 0

    for (i in seq_along(tables)) {
      if (nrow(tables[[i]]) > max_rows) {
        max_rows <- nrow(tables[[i]])
        main_table <- tables[[i]]
      }
    }

    if (!is.null(main_table) && nrow(main_table) > 0) {
      # Save the main results table
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
      filename <- paste0("AD34_", contest$name, "_", timestamp, ".csv")
      output_path <- file.path("data/raw/election/ad34_enr", filename)

      write_csv(main_table, output_path)
      cat("     → Saved:", filename,
          paste0("(", nrow(main_table), " rows, ", ncol(main_table), " cols)\n"))

      all_data[[contest$name]] <- list(
        contest = contest$name,
        code = contest$code,
        data = main_table,
        file = output_path,
        rows = nrow(main_table),
        cols = ncol(main_table)
      )

      downloaded_count <<- downloaded_count + 1
    }

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
# ANALYZE THE DATA
# ============================================================

cat("🔍 Analyzing downloaded AD34 data...\n\n")

for (contest_name in names(all_data)) {
  contest_data <- all_data[[contest_name]]
  data <- contest_data$data

  cat("  📊", contest_name, "\n")
  cat("     Dimensions:", contest_data$rows, "rows ×", contest_data$cols, "columns\n")

  # Try to identify Election District column
  ed_cols <- names(data)[str_detect(tolower(names(data)), "^ed$|election.*district|^x1$")]
  if (length(ed_cols) > 0) {
    cat("     ED column:", ed_cols[1], "\n")

    # Count unique EDs
    unique_eds <- data[[ed_cols[1]]] %>%
      as.character() %>%
      unique() %>%
      {.[. != "" & !is.na(.)]} %>%
      {.[!str_detect(., "ED|Election")]}  # Remove header rows

    cat("     Election Districts:", length(unique_eds), "\n")
  }

  # Try to identify candidate columns
  candidate_cols <- names(data)[!str_detect(tolower(names(data)),
                                             "^ed$|election|district|reported|total|x1|x2")]
  if (length(candidate_cols) > 0) {
    cat("     Candidates/Options:", length(candidate_cols), "\n")
    cat("     ", paste(head(candidate_cols, 3), collapse = ", "), "...\n")
  }

  cat("\n")
}

# ============================================================
# SUMMARY
# ============================================================

cat(rep("=", 60), "\n", sep = "")
cat("📊 DOWNLOAD SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat("Contests downloaded:", downloaded_count, "\n")
cat("Failed downloads:", failed_count, "\n")
cat("Location: data/raw/election/ad34_enr/\n\n")

if (downloaded_count > 0) {
  cat("📋 Files saved:\n")
  for (contest_name in names(all_data)) {
    cat("  •", basename(all_data[[contest_name]]$file), "\n")
  }
}

cat("\n💡 KEY INFORMATION:\n")
cat("  • These are UNOFFICIAL results from election night\n")
cat("  • Data is at Election District level within AD34\n")
cat("  • AD34 = Queens (Jackson Heights, Corona, East Elmhurst, Astoria)\n")
cat("  • Results will be certified on vote.nyc in 2-4 weeks\n")
cat("  • Re-run this script during elections for updated results\n\n")

cat("✅ Download complete!\n\n")

# ============================================================
# EXPORT SUMMARY TABLE
# ============================================================

if (length(all_data) > 0) {
  summary_df <- map_df(all_data, function(x) {
    tibble(
      Contest = x$contest,
      Code = x$code,
      Rows = x$rows,
      Columns = x$cols,
      File = basename(x$file)
    )
  })

  summary_path <- "data/raw/election/ad34_enr/DOWNLOAD_SUMMARY.csv"
  write_csv(summary_df, summary_path)
  cat("📄 Summary table saved to:", summary_path, "\n\n")
}
