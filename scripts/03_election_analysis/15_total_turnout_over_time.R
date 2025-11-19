# ============================================================
# Total Turnout Over Time in AD34
# ============================================================
# Shows total votes cast in each election (all races combined)
# Cleaner visualization than individual winner totals

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📊 Total Turnout Analysis - AD34\n")
cat("=================================\n\n")

# ============================================================
# LOAD TURNOUT DATA FROM PREVIOUS ANALYSIS
# ============================================================

cat("📂 Loading turnout data...\n")

# Use the turnout over time data we already calculated
turnout_data <- read_csv("data/intermediate/ad34_turnout_over_time.csv",
                         show_col_types = FALSE)

cat("  ✓ Loaded", nrow(turnout_data), "elections\n\n")

# Display the data
cat("Elections and turnout:\n")
print(turnout_data %>%
        select(date, year, election_type, top_office, avg_turnout) %>%
        arrange(date),
      n = 50)

# ============================================================
# CREATE CLEAN TURNOUT CHART
# ============================================================

cat("\n📈 Creating total turnout chart...\n")

# Create a cleaner label for each election
turnout_clean <- turnout_data %>%
  mutate(
    # Create concise labels
    election_label = case_when(
      top_office == "Mayor" ~ "Mayoral",
      top_office == "President" ~ "Presidential",
      top_office == "Governor" ~ "Gubernatorial",
      TRUE ~ str_replace(top_office, "State ", "")
    ),
    # Combine type and label
    full_label = paste0(election_label, "\n", election_type)
  )

# Create the plot
turnout_plot <- ggplot(turnout_clean, aes(x = date, y = avg_turnout)) +
  geom_line(aes(color = election_type), linewidth = 1.5, alpha = 0.8) +
  geom_point(aes(color = election_type, size = avg_turnout), alpha = 0.9) +
  geom_text(aes(label = election_label),
            vjust = -1.2, size = 3.5, fontface = "bold") +
  scale_color_manual(
    name = "Election Type",
    values = c("General" = "#E41A1C", "Primary" = "#377EB8")
  ) +
  scale_size_continuous(range = c(4, 10), guide = "none") +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months"
  ) +
  labs(
    title = "Voter Turnout in Assembly District 34 Over Time",
    subtitle = "Average total votes cast across all contests per election (2021-2025)",
    x = "Election Date",
    y = "Average Votes Cast",
    caption = "Source: NYC Board of Elections | AD34: Jackson Heights, Corona, East Elmhurst, Astoria"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "grey50", margin = margin(t = 15)),
    legend.position = "top",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save
ggsave("outputs/maps/AD34_total_turnout_over_time.png",
       turnout_plot,
       width = 14, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_total_turnout_over_time.png\n\n")

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("📊 TURNOUT STATISTICS\n")
cat(rep("=", 60), "\n\n")

# Overall stats
cat("Overall (2021-2025):\n")
cat("  Elections analyzed:", nrow(turnout_clean), "\n")
cat("  Average turnout:", comma(round(mean(turnout_clean$avg_turnout))), "votes\n")
cat("  Median turnout:", comma(round(median(turnout_clean$avg_turnout))), "votes\n")
cat("  Highest turnout:", comma(round(max(turnout_clean$avg_turnout))),
    "(", turnout_clean$election_label[which.max(turnout_clean$avg_turnout)],
    year(turnout_clean$date[which.max(turnout_clean$avg_turnout)]), ")\n")
cat("  Lowest turnout:", comma(round(min(turnout_clean$avg_turnout))),
    "(", turnout_clean$top_office[which.min(turnout_clean$avg_turnout)],
    year(turnout_clean$date[which.min(turnout_clean$avg_turnout)]), ")\n\n")

# By election type
cat("By Election Type:\n")
type_stats <- turnout_clean %>%
  group_by(election_type) %>%
  summarize(
    Elections = n(),
    Avg_Turnout = round(mean(avg_turnout)),
    Min_Turnout = round(min(avg_turnout)),
    Max_Turnout = round(max(avg_turnout))
  )

for (i in 1:nrow(type_stats)) {
  cat(sprintf("  %s Elections:\n", type_stats$election_type[i]))
  cat(sprintf("    Count: %d\n", type_stats$Elections[i]))
  cat(sprintf("    Average: %s votes\n", comma(type_stats$Avg_Turnout[i])))
  cat(sprintf("    Range: %s - %s votes\n\n",
              comma(type_stats$Min_Turnout[i]),
              comma(type_stats$Max_Turnout[i])))
}

# Turnout trends
cat("Turnout Trends:\n")
general_elections <- turnout_clean %>% filter(election_type == "General") %>% arrange(date)
if (nrow(general_elections) >= 2) {
  first_general <- general_elections$avg_turnout[1]
  last_general <- general_elections$avg_turnout[nrow(general_elections)]
  change_pct <- ((last_general - first_general) / first_general) * 100

  cat(sprintf("  General Elections: %+.1f%% change from %d to %d\n",
              change_pct,
              year(general_elections$date[1]),
              year(general_elections$date[nrow(general_elections)])))
}

primary_elections <- turnout_clean %>% filter(election_type == "Primary") %>% arrange(date)
if (nrow(primary_elections) >= 2) {
  first_primary <- primary_elections$avg_turnout[1]
  last_primary <- primary_elections$avg_turnout[nrow(primary_elections)]
  change_pct <- ((last_primary - first_primary) / first_primary) * 100

  cat(sprintf("  Primary Elections: %+.1f%% change from %d to %d\n",
              change_pct,
              year(primary_elections$date[1]),
              year(primary_elections$date[nrow(primary_elections)])))
}

cat("\n✅ Total turnout analysis complete!\n")
cat("📂 Output: outputs/maps/AD34_total_turnout_over_time.png\n\n")
