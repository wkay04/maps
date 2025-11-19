# ============================================================
# AD34 Turnout Over Time Analysis
# ============================================================
# Analyzes turnout trends across multiple elections (2021-2025)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📈 AD34 Turnout Over Time Analysis\n")
cat("===================================\n\n")

# ============================================================
# LOAD ALL AD34 ELECTION DATA
# ============================================================

cat("📂 Loading AD34 election files...\n")

ad34_files <- list.files("data/raw/election/ad34", pattern = "AD34\\.csv$", full.names = TRUE)

cat("  Found", length(ad34_files), "AD34 datasets\n\n")

# ============================================================
# CALCULATE TURNOUT FOR EACH ELECTION
# ============================================================

cat("🔢 Calculating turnout for each election...\n\n")

turnout_data <- map_df(ad34_files, function(file) {
  tryCatch({
    # Read file
    data <- read_csv(file, show_col_types = FALSE)

    # Extract election info from first row
    election_date <- data$Event[1]
    office <- data$Office[1]

    # Parse date
    date_match <- str_extract(election_date, "\\d{2}/\\d{2}/\\d{4}")
    if (!is.na(date_match)) {
      date_parsed <- mdy(date_match)
    } else {
      date_parsed <- NA
    }

    # Calculate total votes
    # Sum all vote tallies (excluding non-vote rows like "Public Counter", etc.)
    vote_rows <- data %>%
      filter(!str_detect(Unit_Name, "Public Counter|Absentee|Affidavit|Emergency|Scattered|^$"))

    total_votes <- sum(as.numeric(vote_rows$Tally), na.rm = TRUE)

    # Return summary
    tibble(
      file = basename(file),
      election_date = election_date,
      date = date_parsed,
      office = office,
      total_votes = total_votes
    )
  }, error = function(e) {
    NULL
  })
})

# Clean and summarize
turnout_summary <- turnout_data %>%
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
      str_detect(office, "Comptroller") ~ "Comptroller",
      str_detect(office, "Advocate") ~ "Public Advocate",
      str_detect(office, "Assembly") ~ "State Assembly",
      str_detect(office, "Congress") ~ "U.S. Congress",
      str_detect(office, "Senator") ~ "State Senate",
      str_detect(office, "Council") ~ "City Council",
      str_detect(office, "Proposal|Amendment") ~ "Ballot Measure",
      TRUE ~ office
    )
  ) %>%
  # Group by election date and type
  group_by(date, year, election_type) %>%
  summarize(
    top_office = first(office_clean),
    avg_turnout = mean(total_votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("✓ Processed", nrow(turnout_summary), "elections\n\n")

# Display summary
cat("Elections in dataset:\n")
print(turnout_summary %>% select(date, year, election_type, top_office, avg_turnout), n = 50)

# ============================================================
# CREATE TURNOUT OVER TIME GRAPH
# ============================================================

cat("\n📊 Creating turnout over time graph...\n")

# Create the plot
turnout_plot <- ggplot(turnout_summary, aes(x = date, y = avg_turnout)) +
  geom_line(aes(color = election_type), size = 1.2, alpha = 0.7) +
  geom_point(aes(color = election_type, size = avg_turnout), alpha = 0.8) +
  geom_text(aes(label = top_office),
            vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_manual(
    name = "Election Type",
    values = c("General" = "#E41A1C", "Primary" = "#377EB8")
  ) +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months"
  ) +
  labs(
    title = "Voter Turnout in Assembly District 34 Over Time",
    subtitle = "Average turnout across all contests per election (2021-2025)",
    x = "Election Date",
    y = "Average Votes Cast",
    caption = "Source: NYC Board of Elections | AD34: Jackson Heights, Corona, East Elmhurst, Astoria (Queens)"
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
ggsave("outputs/maps/AD34_turnout_over_time.png",
       turnout_plot,
       width = 14, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_turnout_over_time.png\n\n")

# ============================================================
# STATISTICS
# ============================================================

cat("📊 Turnout Statistics:\n")
cat(rep("=", 60), "\n")

# Overall stats
cat("Overall (2021-2025):\n")
cat("  Total elections analyzed:", nrow(turnout_summary), "\n")
cat("  Average turnout:", comma(round(mean(turnout_summary$avg_turnout))), "\n")
cat("  Median turnout:", comma(median(turnout_summary$avg_turnout)), "\n")
cat("  Range:", comma(min(turnout_summary$avg_turnout)), "-",
    comma(max(turnout_summary$avg_turnout)), "\n\n")

# By election type
cat("By election type:\n")
turnout_summary %>%
  group_by(election_type) %>%
  summarize(
    Elections = n(),
    Avg_Turnout = comma(round(mean(avg_turnout))),
    Min = comma(min(avg_turnout)),
    Max = comma(max(avg_turnout))
  ) %>%
  print()

cat("\n")

# Top 5 elections by turnout
cat("Top 5 elections by turnout:\n")
top5 <- turnout_summary %>%
  arrange(desc(avg_turnout)) %>%
  head(5)

for (i in 1:nrow(top5)) {
  cat(sprintf("  %d. %s (%s %d) - %s\n",
              i,
              format(top5$date[i], "%b %d, %Y"),
              top5$election_type[i],
              top5$year[i],
              comma(round(top5$avg_turnout[i]))))
}

# ============================================================
# SAVE DATA
# ============================================================

write_csv(turnout_summary, "data/intermediate/ad34_turnout_over_time.csv")

cat("\n✅ Analysis complete!\n")
cat("📂 Outputs:\n")
cat("  • Graph: outputs/maps/AD34_turnout_over_time.png\n")
cat("  • Data: data/intermediate/ad34_turnout_over_time.csv\n\n")
