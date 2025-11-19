# ============================================================
# Visualize Fusion Voting Impact
# ============================================================
# Creates visualizations showing how much votes candidates
# gained from running on multiple party lines

library(tidyverse)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📊 Fusion Voting Visualization\n")
cat("==============================\n\n")

# ============================================================
# LOAD COMBINED DATA
# ============================================================

cat("📂 Loading combined party line data...\n")

combined <- read_csv("data/intermediate/ad34_candidates_combined_partylines.csv",
                     show_col_types = FALSE)
by_party <- read_csv("data/intermediate/ad34_candidates_by_party_line.csv",
                     show_col_types = FALSE)

cat("  ✓ Loaded", nrow(combined), "candidates\n")
cat("  ✓", sum(combined$is_fusion), "fusion candidates\n\n")

# ============================================================
# CALCULATE FUSION VOTING IMPACT
# ============================================================

cat("🔍 Calculating fusion voting impact...\n")

# Focus on major races only
major_offices <- c("Mayor", "Governor/Lieutenant Governor",
                   "President/Vice President", "Attorney General",
                   "State Comptroller", "City Comptroller",
                   "United States Senator", "Public Advocate",
                   "Member of the Assembly", "State Senator")

fusion_major <- combined %>%
  filter(is_fusion, office %in% major_offices) %>%
  arrange(desc(total_votes))

# Get party breakdown for these candidates
fusion_details <- left_join(
  fusion_major,
  by_party,
  by = c("candidate_name", "election", "office")
) %>%
  mutate(
    # Calculate percentage from each party
    pct_of_total = votes / total_votes * 100,
    # Identify primary vs secondary lines
    is_primary = votes == max(votes),
    .groups = "drop"
  )

# Calculate bonus from secondary lines
fusion_impact <- fusion_major %>%
  left_join(
    by_party %>%
      group_by(candidate_name, election, office) %>%
      summarize(
        primary_line_votes = max(votes),
        secondary_lines_votes = sum(votes) - max(votes),
        .groups = "drop"
      ),
    by = c("candidate_name", "election", "office")
  ) %>%
  mutate(
    secondary_pct = secondary_lines_votes / total_votes * 100,
    # Clean labels
    office_short = str_replace(office, "/.*", ""),
    year = str_extract(election, "\\d{4}"),
    label = paste0(candidate_name, " (", office_short, " ", year, ")")
  )

# ============================================================
# VISUALIZATION 1: Top Fusion Candidates
# ============================================================

cat("📈 Creating fusion impact visualization...\n")

top_fusion <- fusion_impact %>%
  filter(secondary_lines_votes > 500) %>%  # At least 500 votes from secondary lines
  arrange(desc(secondary_lines_votes)) %>%
  head(15)

p1 <- ggplot(top_fusion, aes(x = reorder(label, secondary_lines_votes))) +
  geom_col(aes(y = primary_line_votes), fill = "#377EB8", alpha = 0.7) +
  geom_col(aes(y = secondary_lines_votes), fill = "#E41A1C", alpha = 0.8) +
  geom_text(aes(y = total_votes,
                label = paste0("+", comma(secondary_lines_votes), "\n(",
                               round(secondary_pct, 1), "%)")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Fusion Voting Impact in AD34",
    subtitle = "Top 15 candidates by votes gained from secondary party lines (2021-2024)",
    x = NULL,
    y = "Total Votes",
    caption = "Blue = Primary party line | Red = Secondary party line(s)\nSource: NYC Board of Elections"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "grey50", margin = margin(t = 15)),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("outputs/maps/AD34_fusion_voting_impact.png",
       p1, width = 12, height = 10, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_fusion_voting_impact.png\n\n")

# ============================================================
# VISUALIZATION 2: Party Line Contribution Patterns
# ============================================================

cat("📊 Creating party line contribution patterns...\n")

# Aggregate by party combination
party_patterns <- combined %>%
  filter(is_fusion, office %in% major_offices) %>%
  count(parties, wt = total_votes, name = "total_votes") %>%
  arrange(desc(total_votes)) %>%
  head(10) %>%
  mutate(parties_clean = str_replace_all(parties, " \\+ ", "\n"))

p2 <- ggplot(party_patterns, aes(x = reorder(parties_clean, total_votes), y = total_votes)) +
  geom_col(aes(fill = parties_clean), alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = comma(total_votes)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Most Common Fusion Voting Patterns in AD34",
    subtitle = "Total votes by party line combination (2021-2024)",
    x = NULL,
    y = "Total Votes Across All Candidates",
    caption = "Source: NYC Board of Elections | AD34: Jackson Heights, Corona, East Elmhurst, Astoria"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "grey50", margin = margin(t = 15)),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("outputs/maps/AD34_fusion_patterns.png",
       p2, width = 12, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_fusion_patterns.png\n\n")

# ============================================================
# VISUALIZATION 3: WFP vs Conservative Bonus Comparison
# ============================================================

cat("📈 Comparing Working Families vs Conservative party contributions...\n")

# Calculate average contribution by secondary party
party_contributions <- fusion_details %>%
  filter(is_primary == FALSE) %>%  # Only secondary lines
  mutate(
    party_type = case_when(
      party == "Working Families" ~ "Working Families",
      party == "Conservative" ~ "Conservative",
      TRUE ~ "Other"
    )
  ) %>%
  filter(party_type %in% c("Working Families", "Conservative")) %>%
  group_by(party_type) %>%
  summarize(
    n_candidates = n(),
    avg_pct = mean(pct_of_total),
    median_pct = median(pct_of_total),
    total_votes = sum(votes),
    .groups = "drop"
  )

cat("\n📊 PARTY LINE CONTRIBUTION STATISTICS\n")
cat(rep("=", 60), "\n\n")

for (i in 1:nrow(party_contributions)) {
  row <- party_contributions[i,]
  cat(row$party_type, "Party:\n")
  cat("  Candidates:", row$n_candidates, "\n")
  cat("  Average contribution:", round(row$avg_pct, 1), "%\n")
  cat("  Median contribution:", round(row$median_pct, 1), "%\n")
  cat("  Total votes contributed:", comma(row$total_votes), "\n\n")
}

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat(rep("=", 60), "\n")
cat("📈 FUSION VOTING SUMMARY\n")
cat(rep("=", 60), "\n\n")

total_fusion_votes <- sum(fusion_impact$secondary_lines_votes)
avg_fusion_bonus <- mean(fusion_impact$secondary_pct)

cat("Total votes from secondary party lines:", comma(total_fusion_votes), "\n")
cat("Average fusion bonus:", round(avg_fusion_bonus, 1), "% of total votes\n")
cat("Range:", round(min(fusion_impact$secondary_pct), 1), "% to",
    round(max(fusion_impact$secondary_pct), 1), "%\n\n")

# Biggest fusion winners
cat("Top 5 candidates who benefited most from fusion voting:\n")
top5 <- fusion_impact %>%
  arrange(desc(secondary_lines_votes)) %>%
  head(5)

for (i in 1:nrow(top5)) {
  row <- top5[i,]
  cat(sprintf("  %d. %s (%s %s)\n",
              i,
              row$candidate_name,
              row$office_short,
              row$year))
  cat(sprintf("     +%s votes from %s (%.1f%% boost)\n",
              comma(row$secondary_lines_votes),
              row$parties,
              row$secondary_pct))
}

cat("\n✅ Fusion voting analysis complete!\n")
cat("📂 Outputs:\n")
cat("  • outputs/maps/AD34_fusion_voting_impact.png\n")
cat("  • outputs/maps/AD34_fusion_patterns.png\n\n")
