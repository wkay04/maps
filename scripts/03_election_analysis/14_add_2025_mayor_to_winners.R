# ============================================================
# Add 2025 Mayoral Results to Winner Timeline
# ============================================================
# Extracts 2025 mayoral winner from parsed ENR data and
# creates updated winner vote timeline chart

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🗳️  Adding 2025 Mayoral Results to Winner Timeline\n")
cat("==================================================\n\n")

# ============================================================
# EXTRACT 2025 MAYORAL WINNER FROM PARSED ENR DATA
# ============================================================

cat("📂 Loading parsed 2025 mayoral data...\n")

# Load the previously parsed ENR mayor data
mayor_data <- readRDS("data/intermediate/ad34_mayor_parsed.rds")
ed_data <- mayor_data$ed_data

# Row 3 has candidate names, rows 4+ have ED vote data
# Zohran Kwame Mamdani's votes are in column 4 (NA..2)
candidate_row <- ed_data[3,]
data_rows <- ed_data[4:nrow(ed_data),]

# Extract Zohran's votes (column 4 based on ENR table structure)
zohran_col <- 4
zohran_votes <- as.numeric(data_rows[[zohran_col]])
zohran_total <- sum(zohran_votes, na.rm = TRUE)

cat("  🏆 2025 Winner: Zohran Kwame Mamdani\n")
cat("     Votes:", comma(zohran_total), "\n\n")

# ============================================================
# ADD TO WINNER TIMELINE
# ============================================================

cat("📊 Updating winner timeline data...\n")

# Load existing winner data
winner_history <- read_csv("data/intermediate/ad34_winner_votes_over_time.csv",
                           show_col_types = FALSE)

# Create 2025 mayoral result row
winner_2025 <- tibble(
  file = "AD34_Mayor_ENR_2025",
  election_date = "General Election - 11/04/2025",
  date = ymd("2025-11-04"),
  office = "Mayor",
  winner_name = "Zohran Kwame Mamdani",
  winner_votes = zohran_total,
  year = 2025,
  month = factor("Nov", levels = month.abb),
  election_type = "General",
  office_clean = "Mayor"
)

# Combine with historical data
winner_updated <- winner_history %>%
  filter(!(year == 2025 & office_clean == "Mayor")) %>%  # Remove any existing 2025 mayor
  bind_rows(winner_2025) %>%
  arrange(date)

# Save updated data
write_csv(winner_updated, "data/intermediate/ad34_winner_votes_over_time.csv")

cat("  ✓ Added 2025 mayoral result to timeline\n\n")

# ============================================================
# CREATE UPDATED VISUALIZATION
# ============================================================

cat("📈 Creating updated winner timeline chart...\n")

# Focus on major races for clarity
major_races <- winner_updated %>%
  filter(office_clean %in% c("Mayor", "President", "Governor",
                              "State Senate", "Attorney General",
                              "State Comptroller", "NYC Comptroller",
                              "Public Advocate", "Assembly"))

# Create line chart
winner_plot <- ggplot(major_races, aes(x = date, y = winner_votes)) +
  geom_line(aes(color = election_type), linewidth = 1.2, alpha = 0.7) +
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

# Save chart
ggsave("outputs/maps/AD34_winner_votes_over_time.png",
       winner_plot,
       width = 14, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_winner_votes_over_time.png\n\n")

# ============================================================
# DISPLAY MAYORAL RACE COMPARISON
# ============================================================

cat("📊 Mayoral Winners in AD34:\n")
cat(rep("=", 60), "\n")

mayor_races <- winner_updated %>%
  filter(office_clean == "Mayor") %>%
  arrange(date)

for (i in 1:nrow(mayor_races)) {
  row <- mayor_races[i,]
  cat(sprintf("  %s (%d):\n", format(row$date, "%B %d, %Y"), row$year))
  cat(sprintf("    Winner: %s\n", row$winner_name))
  cat(sprintf("    Votes: %s\n", comma(row$winner_votes)))

  if (i > 1) {
    prev_votes <- mayor_races$winner_votes[i-1]
    change <- row$winner_votes - prev_votes
    pct_change <- (change / prev_votes) * 100
    cat(sprintf("    Change: %s votes (%+.1f%%)\n",
                comma(change), pct_change))
  }
  cat("\n")
}

cat("✅ Analysis complete!\n")
cat("📂 Outputs:\n")
cat("  • Chart: outputs/maps/AD34_winner_votes_over_time.png\n")
cat("  • Data: data/intermediate/ad34_winner_votes_over_time.csv\n\n")
