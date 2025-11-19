# ============================================================
# Combine Candidate Votes Across Multiple Party Lines
# ============================================================
# Analyzes AD34 election data to identify candidates running on
# multiple ballot lines (e.g., Democratic + WFP) and combines their votes

library(tidyverse)
library(stringr)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🗳️  AD34 Party Line Vote Combination Analysis\n")
cat("=============================================\n\n")

# ============================================================
# LOAD ALL AD34 ELECTION DATA
# ============================================================

cat("📂 Loading AD34 election files...\n")

ad34_files <- list.files("data/raw/election/ad34", pattern = "\\.csv$", full.names = TRUE)

# Exclude judge races
ad34_files <- ad34_files[!str_detect(ad34_files, "Justice|Judge|Surrogate")]

cat("  Found", length(ad34_files), "non-judicial AD34 datasets\n\n")

# ============================================================
# ANALYZE EACH ELECTION FOR FUSION VOTING
# ============================================================

cat("🔍 Analyzing elections for candidates on multiple ballot lines...\n\n")

fusion_results <- list()

for (file in ad34_files) {
  tryCatch({
    # Read file
    data <- read_csv(file, show_col_types = FALSE)

    # Extract election info
    if (nrow(data) == 0) next
    election_date <- data$Event[1]
    office <- data$Office[1]

    # Filter for actual candidate votes (exclude counters, scattered, etc.)
    candidates <- data %>%
      filter(str_detect(Unit_Name, "\\(") & str_detect(Unit_Name, "\\)")) %>%
      filter(!str_detect(Unit_Name, "Public Counter|Absentee|Affidavit|Emergency|Scattered")) %>%
      mutate(
        # Extract candidate name (before parenthesis)
        candidate_name = str_trim(str_extract(Unit_Name, "^[^\\(]+")),
        # Extract party (inside parenthesis)
        party = str_extract(Unit_Name, "(?<=\\()[^\\)]+(?=\\))"),
        tally_num = as.numeric(Tally)
      ) %>%
      filter(!is.na(tally_num))

    if (nrow(candidates) == 0) next

    # Calculate votes by candidate+party
    by_party <- candidates %>%
      group_by(candidate_name, party) %>%
      summarize(votes = sum(tally_num, na.rm = TRUE), .groups = "drop")

    # Calculate total votes by candidate (across all party lines)
    by_candidate <- candidates %>%
      group_by(candidate_name) %>%
      summarize(
        total_votes = sum(tally_num, na.rm = TRUE),
        party_lines = n_distinct(party),
        parties = paste(unique(party), collapse = " + "),
        .groups = "drop"
      )

    # Identify fusion candidates (appear on multiple lines)
    fusion_candidates <- by_candidate %>%
      filter(party_lines > 1)

    if (nrow(fusion_candidates) > 0) {
      # Store results
      fusion_results[[length(fusion_results) + 1]] <- list(
        file = basename(file),
        election = election_date,
        office = office,
        fusion_candidates = fusion_candidates,
        all_candidates = by_candidate,
        by_party = by_party
      )

      cat("✓", office, "-", election_date, "\n")
      cat("  Fusion candidates found:", nrow(fusion_candidates), "\n")
      for (i in 1:nrow(fusion_candidates)) {
        cand <- fusion_candidates[i,]
        cat(sprintf("    • %s: %s (%s votes)\n",
                    cand$candidate_name,
                    cand$parties,
                    comma(cand$total_votes)))
      }
      cat("\n")
    }

  }, error = function(e) {
    # Skip files that can't be parsed
    NULL
  })
}

# ============================================================
# DETAILED ANALYSIS: SHOW TOP RACES
# ============================================================

if (length(fusion_results) > 0) {
  cat("\n📊 DETAILED FUSION VOTING ANALYSIS\n")
  cat(rep("=", 70), "\n\n")

  # Show top 5 races with most fusion candidates
  fusion_counts <- map_int(fusion_results, ~nrow(.x$fusion_candidates))
  top_races <- order(fusion_counts, decreasing = TRUE)[1:min(5, length(fusion_results))]

  for (idx in top_races) {
    result <- fusion_results[[idx]]

    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("RACE:", result$office, "\n")
    cat("DATE:", result$election, "\n")
    cat(rep("─", 70), "\n\n")

    # Show all candidates with combined totals
    all_cands <- result$all_candidates %>%
      arrange(desc(total_votes))

    cat("RESULTS (with party lines combined):\n\n")
    for (i in 1:nrow(all_cands)) {
      cand <- all_cands[i,]
      fusion_marker <- if_else(cand$party_lines > 1, " ★", "")
      cat(sprintf("  %d. %s%s\n", i, cand$candidate_name, fusion_marker))
      cat(sprintf("     %s votes (%s)\n",
                  comma(cand$total_votes),
                  cand$parties))

      # Show breakdown by party line if fusion candidate
      if (cand$party_lines > 1) {
        party_breakdown <- result$by_party %>%
          filter(candidate_name == cand$candidate_name) %>%
          arrange(desc(votes))

        cat("     Breakdown by party line:\n")
        for (j in 1:nrow(party_breakdown)) {
          pb <- party_breakdown[j,]
          pct <- round(100 * pb$votes / cand$total_votes, 1)
          cat(sprintf("       → %s: %s (%s%%)\n",
                      pb$party,
                      comma(pb$votes),
                      pct))
        }
      }
      cat("\n")
    }
  }

  # ============================================================
  # SUMMARY STATISTICS
  # ============================================================

  cat("\n\n")
  cat(rep("=", 70), "\n")
  cat("📈 FUSION VOTING STATISTICS\n")
  cat(rep("=", 70), "\n\n")

  cat("Total elections analyzed:", length(ad34_files), "\n")
  cat("Elections with fusion candidates:", length(fusion_results), "\n\n")

  # Count total fusion candidates
  total_fusion <- sum(map_int(fusion_results, ~nrow(.x$fusion_candidates)))
  cat("Total fusion candidates found:", total_fusion, "\n\n")

  # Most common fusion patterns
  all_fusion <- map_df(fusion_results, ~.x$fusion_candidates)

  if (nrow(all_fusion) > 0) {
    cat("Most common party line combinations:\n")
    party_combos <- all_fusion %>%
      count(parties, sort = TRUE) %>%
      head(10)

    for (i in 1:nrow(party_combos)) {
      cat(sprintf("  %d. %s (%d candidates)\n",
                  i,
                  party_combos$parties[i],
                  party_combos$n[i]))
    }
  }

  # ============================================================
  # SAVE COMBINED RESULTS
  # ============================================================

  cat("\n📁 Saving combined results...\n")

  # Create combined dataset
  combined_data <- map_df(fusion_results, function(result) {
    result$all_candidates %>%
      mutate(
        election = result$election,
        office = result$office,
        file = result$file,
        is_fusion = party_lines > 1
      )
  })

  write_csv(combined_data, "data/intermediate/ad34_candidates_combined_partylines.csv")
  cat("  ✓ Saved: data/intermediate/ad34_candidates_combined_partylines.csv\n")

  # Save detailed breakdown
  detailed_data <- map_df(fusion_results, function(result) {
    result$by_party %>%
      mutate(
        election = result$election,
        office = result$office,
        file = result$file
      )
  })

  write_csv(detailed_data, "data/intermediate/ad34_candidates_by_party_line.csv")
  cat("  ✓ Saved: data/intermediate/ad34_candidates_by_party_line.csv\n")

} else {
  cat("⚠️  No fusion candidates found in any elections\n")
  cat("   This is unusual - checking data format...\n")
}

cat("\n✅ Party line analysis complete!\n\n")
