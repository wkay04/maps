# ============================================================
# Turnout Percentage Analysis - AD34
# ============================================================
# Calculates voter turnout as % of registered voters
# Shows both registration trends and participation rates

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📊 Turnout Percentage Analysis - AD34\n")
cat("======================================\n\n")

# ============================================================
# LOAD REGISTERED VOTER DATA
# ============================================================

cat("📂 Loading registered voter enrollment data...\n")

# Use estimated registration numbers for now
# These are based on 2020 Census CVAP (~107k) and typical NYC registration rate (70%)
enrollment_data <- read_csv("data/raw/enrollment/ad34_enrollment_estimates.csv",
                            show_col_types = FALSE)

cat("  Note: Using estimated registration numbers based on Census CVAP data\n")
cat("  and typical NYC registration rates (~70% of CVAP)\n\n")

cat("✓ Loaded AD34 registration estimates:\n")
print(enrollment_data %>% select(year, total_registered, notes), n = 50)
cat("\n")

# ============================================================
# LOAD TURNOUT DATA
# ============================================================

cat("📂 Loading turnout data...\n")

turnout_data <- read_csv("data/intermediate/ad34_turnout_over_time.csv",
                         show_col_types = FALSE)

cat("  ✓ Loaded", nrow(turnout_data), "elections\n\n")

# ============================================================
# MATCH TURNOUT TO REGISTRATION
# ============================================================

cat("🔗 Matching turnout to registration data...\n")

# For each election, use the enrollment data from the same year
turnout_with_reg <- turnout_data %>%
  left_join(enrollment_data, by = "year") %>%
  mutate(
    turnout_pct = (avg_turnout / total_registered) * 100,
    election_label = case_when(
      top_office == "Mayor" ~ "Mayoral",
      top_office == "President" ~ "Presidential",
      top_office == "Governor" ~ "Gubernatorial",
      TRUE ~ str_replace(top_office, "State ", "")
    )
  ) %>%
  filter(!is.na(total_registered))

cat("✓ Matched", nrow(turnout_with_reg), "elections to registration data\n\n")

print(turnout_with_reg %>%
        select(date, election_label, election_type, avg_turnout,
               total_registered, turnout_pct) %>%
        arrange(date),
      n = 50)

# ============================================================
# CREATE DUAL VISUALIZATION
# ============================================================

cat("\n📈 Creating turnout percentage visualization...\n")

# Prepare data for plotting
plot_data <- turnout_with_reg %>%
  arrange(date)

# Create plot with dual y-axes
p <- ggplot(plot_data, aes(x = date)) +
  # Turnout percentage (primary axis)
  geom_line(aes(y = turnout_pct, color = election_type),
            linewidth = 1.5, alpha = 0.8) +
  geom_point(aes(y = turnout_pct, color = election_type, size = turnout_pct),
             alpha = 0.9) +
  geom_text(aes(y = turnout_pct, label = election_label),
            vjust = -1.2, size = 3, fontface = "bold") +

  # Registered voters (secondary axis)
  geom_line(aes(y = total_registered / 1000),
            color = "gray30", linewidth = 1.2, linetype = "dashed", alpha = 0.6) +
  geom_point(aes(y = total_registered / 1000),
             color = "gray30", size = 3, alpha = 0.6) +

  scale_color_manual(
    name = "Election Type",
    values = c("General" = "#E41A1C", "Primary" = "#377EB8")
  ) +
  scale_size_continuous(range = c(4, 10), guide = "none") +

  # Primary y-axis (turnout %)
  scale_y_continuous(
    name = "Turnout (% of Registered Voters)",
    labels = function(x) paste0(x, "%"),
    breaks = pretty_breaks(n = 8),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15)),

    # Secondary y-axis (registered voters in thousands)
    sec.axis = sec_axis(
      ~ . * 1000,
      name = "Total Registered Voters",
      labels = comma,
      breaks = pretty_breaks(n = 8)
    )
  ) +

  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "6 months"
  ) +

  labs(
    title = "Voter Turnout Rates in Assembly District 34",
    subtitle = "Turnout as % of registered voters (solid line) and total registration (dashed line), 2021-2025",
    x = "Election Date",
    caption = "Source: NYC Board of Elections & NY State Board of Elections | AD34: Jackson Heights, Corona, East Elmhurst, Astoria"
  ) +

  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "grey50", margin = margin(t = 15)),
    legend.position = "top",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    axis.title.y.right = element_text(color = "gray30"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("outputs/maps/AD34_turnout_percentage.png",
       p, width = 14, height = 8, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_turnout_percentage.png\n\n")

# ============================================================
# STATISTICS
# ============================================================

cat("📊 TURNOUT RATE STATISTICS\n")
cat(rep("=", 60), "\n\n")

# Overall stats
cat("Overall (2021-2025):\n")
cat("  Average turnout rate:", sprintf("%.1f%%", mean(plot_data$turnout_pct)), "\n")
cat("  Highest turnout:", sprintf("%.1f%%", max(plot_data$turnout_pct)),
    "(",  plot_data$election_label[which.max(plot_data$turnout_pct)],
    year(plot_data$date[which.max(plot_data$turnout_pct)]), ")\n")
cat("  Lowest turnout:", sprintf("%.1f%%", min(plot_data$turnout_pct)),
    "(", plot_data$election_label[which.min(plot_data$turnout_pct)],
    year(plot_data$date[which.min(plot_data$turnout_pct)]), ")\n\n")

# By election type
cat("By Election Type:\n")
type_stats <- plot_data %>%
  group_by(election_type) %>%
  summarize(
    Elections = n(),
    Avg_Turnout_Pct = mean(turnout_pct),
    Min_Pct = min(turnout_pct),
    Max_Pct = max(turnout_pct),
    .groups = "drop"
  )

for (i in 1:nrow(type_stats)) {
  cat(sprintf("  %s Elections:\n", type_stats$election_type[i]))
  cat(sprintf("    Average: %.1f%%\n", type_stats$Avg_Turnout_Pct[i]))
  cat(sprintf("    Range: %.1f%% - %.1f%%\n\n",
              type_stats$Min_Pct[i], type_stats$Max_Pct[i]))
}

# Registration trends
cat("Registered Voter Trends:\n")
reg_first <- enrollment_data$total_registered[which.min(enrollment_data$year)]
reg_last <- enrollment_data$total_registered[which.max(enrollment_data$year)]
reg_change <- ((reg_last - reg_first) / reg_first) * 100

cat(sprintf("  %d: %s registered voters\n",
            min(enrollment_data$year), comma(reg_first)))
cat(sprintf("  %d: %s registered voters\n",
            max(enrollment_data$year), comma(reg_last)))
cat(sprintf("  Change: %+.1f%% (%s voters)\n\n",
            reg_change, comma(reg_last - reg_first)))

# Save combined dataset
write_csv(turnout_with_reg,
          "data/intermediate/ad34_turnout_with_registration.csv")

cat("✅ Turnout percentage analysis complete!\n")
cat("📂 Outputs:\n")
cat("  • Chart: outputs/maps/AD34_turnout_percentage.png\n")
cat("  • Data: data/intermediate/ad34_turnout_with_registration.csv\n\n")
