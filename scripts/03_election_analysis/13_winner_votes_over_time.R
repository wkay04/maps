# ============================================================
# Winner Vote Totals Over Time in AD34
# ============================================================
# Creates a line chart showing total votes for the winner
# of each election (2021-2025) including 2025 mayoral primary

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🏆 Winner Vote Totals Over Time\n")
cat("================================\n\n")

# ============================================================
# LOAD ALL AD34 ELECTION DATA
# ============================================================

cat("📂 Loading AD34 election files...\n")

ad34_files <- list.files("data/raw/election/ad34", pattern = "\\.csv$", full.names = TRUE)

# Exclude judge races
ad34_files <- ad34_files[!str_detect(ad34_files, "Justice|Judge|Surrogate")]

cat("  Found", length(ad34_files), "non-judicial AD34 datasets\n\n")

# ============================================================
# FIND WINNER FOR EACH ELECTION
# ============================================================

cat("🔍 Identifying winners and vote totals...\n\n")

winner_data <- map_df(ad34_files, function(file) {
  tryCatch({
    # Read file
    data <- read_csv(file, show_col_types = FALSE)

    if (nrow(data) == 0) return(NULL)

    # Extract election info
    election_date <- data$Event[1]
    office <- data$Office[1]

    # Parse date
    date_match <- str_extract(election_date, "\\d{2}/\\d{2}/\\d{4}")
    if (!is.na(date_match)) {
      date_parsed <- mdy(date_match)
    } else {
      return(NULL)
    }

    # Filter for actual candidate votes
    candidates <- data %>%
      filter(str_detect(Unit_Name, "\\(") & str_detect(Unit_Name, "\\)")) %>%
      filter(!str_detect(Unit_Name, "Public Counter|Absentee|Affidavit|Emergency|Scattered")) %>%
      mutate(
        # Extract candidate name
        candidate_name = str_trim(str_extract(Unit_Name, "^[^\\(]+")),
        tally_num = as.numeric(Tally)
      ) %>%
      filter(!is.na(tally_num))

    if (nrow(candidates) == 0) return(NULL)

    # Calculate total votes by candidate (combining party lines)
    by_candidate <- candidates %>%
      group_by(candidate_name) %>%
      summarize(total_votes = sum(tally_num, na.rm = TRUE), .groups = "drop")

    # Find winner
    winner <- by_candidate %>%
      arrange(desc(total_votes)) %>%
      slice(1)

    # Return summary
    tibble(
      file = basename(file),
      election_date = election_date,
      date = date_parsed,
      office = office,
      winner_name = winner$candidate_name,
      winner_votes = winner$total_votes
    )

  }, error = function(e) {
    NULL
  })
})

# Clean and summarize
winner_summary <- winner_data %>%
  filter(!is.na(date)) %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE),
    election_type = if_else(month(date) %in% c(6), "Primary", "General"),
    # Simplify office names
    office_clean = case_when(
      str_detect(office, "Mayor") ~ "Mayor",
      str_detect(office, "President") & !str_detect(office, "Borough") ~ "President",
      str_detect(office, "Governor") ~ "Governor",
      str_detect(office, "Comptroller") & str_detect(office, "City") ~ "NYC Comptroller",
      str_detect(office, "Comptroller") ~ "State Comptroller",
      str_detect(office, "Advocate") ~ "Public Advocate",
      str_detect(office, "Assembly") ~ "Assembly",
      str_detect(office, "Congress") ~ "Congress",
      str_detect(office, "Senator") & str_detect(office, "State") ~ "State Senate",
      str_detect(office, "Senator") ~ "US Senate",
      str_detect(office, "Council") ~ "City Council",
      str_detect(office, "Borough President") ~ "Borough President",
      str_detect(office, "Attorney General") ~ "Attorney General",
      TRUE ~ office
    )
  ) %>%
  arrange(date)

cat("✓ Processed", nrow(winner_summary), "elections\n\n")

# Display summary
cat("Winners by election:\n")
print(winner_summary %>%
        select(date, office_clean, winner_name, winner_votes) %>%
        arrange(date),
      n = 100)

# ============================================================
# CREATE LINE CHART
# ============================================================

cat("\n📊 Creating winner vote totals chart...\n")

# Focus on major races for clarity
major_races <- winner_summary %>%
  filter(office_clean %in% c("Mayor", "President", "Governor",
                              "US Senate", "Attorney General",
                              "State Comptroller", "NYC Comptroller",
                              "Public Advocate", "Assembly"))

# Create the plot
winner_plot <- ggplot(major_races, aes(x = date, y = winner_votes)) +
  geom_line(aes(color = election_type), size = 1.2, alpha = 0.7) +
  geom_point(aes(color = election_type, size = winner_votes), alpha = 0.8) +
  geom_text(aes(label = paste0(office_clean, "\n", winner_name)),
            vjust = -0.8, size = 2.5, check_overlap = TRUE) +
  scale_color_manual(
    name = "Election Type",
    values = c("General" = "#E41A1C", "Primary" = "#377EB8")
  ) +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    limits = c(0, NA)
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months"
  ) +
  labs(
    title = "Winner Vote Totals in Assembly District 34 Over Time",
    subtitle = "Total votes for winning candidate in major elections (2021-2025)",
    x = "Election Date",
    y = "Votes for Winner",
    caption = "Source: NYC Board of Elections | AD34: Jackson Heights, Corona, East Elmhurst, Astoria"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "grey50", margin = margin(t = 15)),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save
ggsave("outputs/maps/AD34_winner_votes_over_time.png",
       winner_plot,
       width = 14, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_winner_votes_over_time.png\n\n")

# ============================================================
# STATISTICS
# ============================================================

cat("📊 Winner Vote Statistics:\n")
cat(rep("=", 60), "\n")

cat("Overall (2021-2025):\n")
cat("  Total elections analyzed:", nrow(major_races), "major races\n")
cat("  Average winner votes:", comma(round(mean(major_races$winner_votes))), "\n")
cat("  Median winner votes:", comma(median(major_races$winner_votes)), "\n")
cat("  Range:", comma(min(major_races$winner_votes)), "-",
    comma(max(major_races$winner_votes)), "\n\n")

# By election type
cat("By election type:\n")
major_races %>%
  group_by(election_type) %>%
  summarize(
    Elections = n(),
    Avg_Winner_Votes = comma(round(mean(winner_votes))),
    Min = comma(min(winner_votes)),
    Max = comma(max(winner_votes))
  ) %>%
  print()

cat("\n")

# Top 5 winners by vote total
cat("Top 5 winners by vote total:\n")
top5 <- major_races %>%
  arrange(desc(winner_votes)) %>%
  head(5)

for (i in 1:nrow(top5)) {
  cat(sprintf("  %d. %s - %s (%s %d) - %s votes\n",
              i,
              top5$winner_name[i],
              top5$office_clean[i],
              top5$election_type[i],
              top5$year[i],
              comma(top5$winner_votes[i])))
}

# Highlight 2025 Mayor race
cat("\n🗳️  2025 Mayoral Primary:\n")
mayor_2025 <- winner_summary %>%
  filter(year == 2025, str_detect(office, "Mayor"))

if (nrow(mayor_2025) > 0) {
  cat(sprintf("  Winner: %s\n", mayor_2025$winner_name[1]))
  cat(sprintf("  Votes: %s\n", comma(mayor_2025$winner_votes[1])))
  cat(sprintf("  Date: %s\n", format(mayor_2025$date[1], "%B %d, %Y")))
} else {
  cat("  No 2025 mayoral data found\n")
}

# ============================================================
# SAVE DATA
# ============================================================

write_csv(winner_summary, "data/intermediate/ad34_winner_votes_over_time.csv")

cat("\n✅ Analysis complete!\n")
cat("📂 Outputs:\n")
cat("  • Graph: outputs/maps/AD34_winner_votes_over_time.png\n")
cat("  • Data: data/intermediate/ad34_winner_votes_over_time.csv\n\n")
