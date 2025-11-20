library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

setwd("~/Mayoral Results AD34")

cat("Creating Enhanced HTML Demographics Dashboard...\n\n")

# Load the data
correlations <- read.csv("outputs/tables/census_election_correlations.csv")
demographic_profiles <- read.csv("outputs/tables/demographic_profiles_by_category.csv")
ed_data <- read.csv("outputs/tables/ed_level_census_election_data.csv")

# Create output directories
dir.create("outputs/charts", showWarnings = FALSE, recursive = TRUE)

cat("Creating visualization 1: Correlation bars...\n")

# Correlation bars - Support
support_plot <- correlations %>%
  filter(!is.na(Support.Correlation)) %>%
  mutate(Variable = reorder(Variable, Support.Correlation)) %>%
  ggplot(aes(x = Variable, y = Support.Correlation,
             fill = ifelse(Support.Correlation > 0, "Positive", "Negative"))) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dashed", color = "gray50", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2166ac", "Negative" = "#b2182b"),
                    guide = "none") +
  scale_y_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.6, 0.6, 0.2)) +
  labs(title = "Correlation with Zohran Support",
       subtitle = "Which demographics predict higher vote share?",
       x = NULL,
       y = "Correlation Coefficient") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/charts/correlation_support.png", support_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

# Correlation bars - Turnout
turnout_plot <- correlations %>%
  filter(!is.na(Turnout.Correlation)) %>%
  mutate(Variable = reorder(Variable, Turnout.Correlation)) %>%
  ggplot(aes(x = Variable, y = Turnout.Correlation,
             fill = ifelse(Turnout.Correlation > 0, "Positive", "Negative"))) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dashed", color = "gray50", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2166ac", "Negative" = "#b2182b"),
                    guide = "none") +
  scale_y_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.6, 0.6, 0.2)) +
  labs(title = "Correlation with Voter Turnout",
       subtitle = "Which demographics predict higher turnout?",
       x = NULL,
       y = "Correlation Coefficient") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/charts/correlation_turnout.png", turnout_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Creating visualization 2: Quadrant comparison charts...\n")

# Quadrant demographic comparison - stacked bar
quadrant_demo <- demographic_profiles %>%
  select(classification, avg_hispanic, avg_white, avg_asian, avg_black) %>%
  pivot_longer(cols = starts_with("avg_"),
               names_to = "group",
               values_to = "percentage") %>%
  mutate(
    group = recode(group,
                  "avg_hispanic" = "Hispanic",
                  "avg_white" = "White (NH)",
                  "avg_asian" = "Asian (NH)",
                  "avg_black" = "Black (NH)"),
    classification = factor(classification,
                           levels = c("High Support, High Turnout",
                                     "High Support, Low Turnout",
                                     "Low Support, High Turnout",
                                     "Low Support, Low Turnout"))
  )

demo_comp_plot <- ggplot(quadrant_demo, aes(x = classification, y = percentage, fill = group)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_fill_manual(values = c("Hispanic" = "#e08214", "White (NH)" = "#7fbc41",
                               "Asian (NH)" = "#8073ac", "Black (NH)" = "#542788"),
                    name = "Race/Ethnicity") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Racial/Ethnic Composition by Strategic Quadrant",
       subtitle = "Percentage of population",
       x = NULL,
       y = "Population %") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("outputs/charts/quadrant_demographics.png", demo_comp_plot,
       width = 11, height = 7, dpi = 300, bg = "white")

cat("Creating visualization 3: Education and income comparison...\n")

# Education & Income by quadrant
edu_income_data <- demographic_profiles %>%
  mutate(
    classification = factor(classification,
                           levels = c("High Support, High Turnout",
                                     "High Support, Low Turnout",
                                     "Low Support, High Turnout",
                                     "Low Support, Low Turnout")),
    class_short = c("High Supp\nHigh Turn", "High Supp\nLow Turn",
                   "Low Supp\nHigh Turn", "Low Supp\nLow Turn")
  )

edu_plot <- ggplot(edu_income_data, aes(x = class_short, y = avg_college)) +
  geom_col(fill = "#3b82f6", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", avg_college)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 55)) +
  labs(title = "College Education Rates",
       x = NULL,
       y = "% with Bachelor's Degree or Higher") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 10)
  )

income_plot <- ggplot(edu_income_data, aes(x = class_short, y = avg_income)) +
  geom_col(fill = "#10b981", alpha = 0.8) +
  geom_text(aes(label = dollar(avg_income, accuracy = 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(), limits = c(0, 100000)) +
  labs(title = "Median Household Income",
       x = NULL,
       y = "Median Income") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 10)
  )

combined_plot <- grid.arrange(edu_plot, income_plot, ncol = 2)
ggsave("outputs/charts/education_income_comparison.png", combined_plot,
       width = 12, height = 5, dpi = 300, bg = "white")

cat("Creating visualization 4: Scatterplots...\n")

# Scatterplot: Education vs Turnout
scatter_edu_turnout <- ed_data %>%
  filter(!is.na(pct_college) & !is.na(turnout_pct)) %>%
  ggplot(aes(x = pct_college, y = turnout_pct)) +
  geom_point(size = 3, alpha = 0.6, color = "#3b82f6") +
  geom_smooth(method = "lm", se = TRUE, color = "#1e3a8a", linewidth = 1.2) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Education Strongly Predicts Turnout (r = +0.68)",
       subtitle = "Each point is one Election District",
       x = "% with College Degree",
       y = "Voter Turnout %") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave("outputs/charts/scatter_education_turnout.png", scatter_edu_turnout,
       width = 8, height = 6, dpi = 300, bg = "white")

# Scatterplot: Hispanic % vs Turnout
scatter_hisp_turnout <- ed_data %>%
  filter(!is.na(pct_hispanic) & !is.na(turnout_pct)) %>%
  ggplot(aes(x = pct_hispanic, y = turnout_pct)) +
  geom_point(size = 3, alpha = 0.6, color = "#ea580c") +
  geom_smooth(method = "lm", se = TRUE, color = "#9a3412", linewidth = 1.2) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Hispanic Areas Have Lower Turnout (r = -0.62)",
       subtitle = "The mobilization opportunity",
       x = "% Hispanic",
       y = "Voter Turnout %") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave("outputs/charts/scatter_hispanic_turnout.png", scatter_hisp_turnout,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Creating visualization 5: Turnout by quadrant...\n")

# Turnout comparison by quadrant
turnout_comp <- demographic_profiles %>%
  mutate(
    classification = factor(classification,
                           levels = c("High Support, High Turnout",
                                     "High Support, Low Turnout",
                                     "Low Support, High Turnout",
                                     "Low Support, Low Turnout")),
    turnout_rate = (total_votes / total_registered) * 100,
    class_short = c("High Supp\nHigh Turn", "High Supp\nLow Turn",
                   "Low Supp\nHigh Turn", "Low Supp\nLow Turn"),
    priority = c("Base", "TOP PRIORITY", "Persuade", "Long-term")
  )

turnout_comp_plot <- ggplot(turnout_comp, aes(x = class_short, y = turnout_rate, fill = priority)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", turnout_rate)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Base" = "#3b82f6", "TOP PRIORITY" = "#ea580c",
                               "Persuade" = "#8b5cf6", "Long-term" = "#ef4444"),
                    name = "Strategy") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 55)) +
  labs(title = "Actual Turnout Rates by Strategic Quadrant",
       subtitle = "Gap between High Support groups shows mobilization opportunity",
       x = NULL,
       y = "Turnout Rate") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("outputs/charts/turnout_by_quadrant.png", turnout_comp_plot,
       width = 11, height = 7, dpi = 300, bg = "white")

cat("Creating visualization 6: Foreign-born comparison...\n")

# Foreign born by quadrant
foreign_plot <- demographic_profiles %>%
  mutate(
    classification = factor(classification,
                           levels = c("High Support, High Turnout",
                                     "High Support, Low Turnout",
                                     "Low Support, High Turnout",
                                     "Low Support, Low Turnout")),
    class_short = c("High Supp\nHigh Turn", "High Supp\nLow Turn",
                   "Low Supp\nHigh Turn", "Low Supp\nLow Turn")
  ) %>%
  ggplot(aes(x = class_short, y = avg_foreign_born)) +
  geom_col(fill = "#f59e0b", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", avg_foreign_born)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 70)) +
  labs(title = "Foreign-Born Population by Quadrant",
       subtitle = "Highest in low-turnout areas (correlates with Hispanic population)",
       x = NULL,
       y = "% Foreign Born") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    axis.text.x = element_text(size = 10)
  )

ggsave("outputs/charts/foreign_born_comparison.png", foreign_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("\nAll charts created successfully!\n")
cat("Files saved to outputs/charts/\n\n")

# List all chart files
chart_files <- list.files("outputs/charts", pattern = "\\.png$", full.names = FALSE)
cat("Created charts:\n")
for (f in chart_files) {
  cat(sprintf("  - %s\n", f))
}
