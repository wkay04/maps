# ============================================================
# Comprehensive Multi-Ethnic Comparison - Mamdani Support
# ============================================================
# Compare and visualize ALL ethnic subgroups together

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🌍 COMPREHENSIVE ETHNIC COMPARISON\n")
cat("===================================\n\n")

# ============================================================
# LOAD ALL SUBGROUP RESULTS
# ============================================================

cat("📥 Loading subgroup analysis results...\n")

# Load Asian results
asian <- read_csv("outputs/analysis/subgroup_correlations_detailed.csv",
                  show_col_types = FALSE) %>%
  mutate(
    ethnicity_category = "Asian",
    group_clean = str_to_title(str_replace_all(group, "_", " "))
  )

# Load Hispanic results
hispanic <- read_csv("outputs/analysis/hispanic_subgroup_correlations.csv",
                     show_col_types = FALSE) %>%
  mutate(ethnicity_category = "Hispanic/Latino")

# Load White ethnic results
white <- read_csv("outputs/analysis/white_ethnic_subgroup_correlations.csv",
                  show_col_types = FALSE) %>%
  mutate(ethnicity_category = "White Ethnic")

# Combine all
all_groups <- bind_rows(asian, hispanic, white) %>%
  filter(n_eds_nonzero >= 5, !is.na(cor_mamdani_nonzero)) %>%
  arrange(desc(cor_mamdani_nonzero))

cat("✓ Loaded", nrow(all_groups), "ethnic subgroups\n")
cat("  - Asian:", nrow(asian), "groups\n")
cat("  - Hispanic:", nrow(hispanic), "groups\n")
cat("  - White:", nrow(white), "groups\n\n")

# ============================================================
# TOP SUPPORTERS VS OPPONENTS
# ============================================================

cat("🎯 TOP MAMDANI SUPPORTERS (All Ethnicities)\n")
cat("============================================\n\n")

top_supporters <- all_groups %>%
  arrange(desc(cor_mamdani_nonzero)) %>%
  head(15)

cat(sprintf("%-30s %-20s %10s %10s\n", "Group", "Category", "Mamdani r", "N EDs"))
cat(rep("-", 72), "\n")
for (i in 1:nrow(top_supporters)) {
  cat(sprintf("%-30s %-20s %10.3f %10d\n",
              top_supporters$group_clean[i],
              top_supporters$ethnicity_category[i],
              top_supporters$cor_mamdani_nonzero[i],
              top_supporters$n_eds_nonzero[i]))
}

cat("\n")
cat("🎯 TOP MAMDANI OPPONENTS (All Ethnicities)\n")
cat("==========================================\n\n")

top_opponents <- all_groups %>%
  arrange(cor_mamdani_nonzero) %>%
  head(15)

cat(sprintf("%-30s %-20s %10s %10s\n", "Group", "Category", "Mamdani r", "N EDs"))
cat(rep("-", 72), "\n")
for (i in 1:nrow(top_opponents)) {
  cat(sprintf("%-30s %-20s %10.3f %10d\n",
              top_opponents$group_clean[i],
              top_opponents$ethnicity_category[i],
              top_opponents$cor_mamdani_nonzero[i],
              top_opponents$n_eds_nonzero[i]))
}

cat("\n")

# ============================================================
# MOBILIZATION PROFILE
# ============================================================

cat("🎯 MOBILIZED BASE (High Support + High Turnout)\n")
cat("================================================\n\n")

mobilized_base <- all_groups %>%
  filter(cor_mamdani_nonzero > 0.2, cor_turnout_nonzero > 0.2) %>%
  arrange(desc(mobilization_score))

cat(sprintf("%-30s %-20s %10s %10s %10s\n",
            "Group", "Category", "Mamdani r", "Turnout r", "Combined"))
cat(rep("-", 85), "\n")
for (i in 1:nrow(mobilized_base)) {
  cat(sprintf("%-30s %-20s %10.3f %10.3f %10.3f\n",
              mobilized_base$group_clean[i],
              mobilized_base$ethnicity_category[i],
              mobilized_base$cor_mamdani_nonzero[i],
              mobilized_base$cor_turnout_nonzero[i],
              mobilized_base$mobilization_score[i]))
}

cat("\n")

# ============================================================
# SUMMARY BY ETHNICITY CATEGORY
# ============================================================

cat("📊 SUMMARY BY BROAD ETHNICITY CATEGORY\n")
cat("=======================================\n\n")

summary_stats <- all_groups %>%
  group_by(ethnicity_category) %>%
  summarize(
    n_groups = n(),
    avg_mamdani_cor = mean(cor_mamdani_nonzero, na.rm = TRUE),
    median_mamdani_cor = median(cor_mamdani_nonzero, na.rm = TRUE),
    n_positive = sum(cor_mamdani_nonzero > 0.1, na.rm = TRUE),
    n_negative = sum(cor_mamdani_nonzero < -0.1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mamdani_cor))

cat(sprintf("%-20s %8s %12s %14s %10s %10s\n",
            "Category", "N", "Mean r", "Median r", "Positive", "Negative"))
cat(rep("-", 80), "\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("%-20s %8d %12.3f %14.3f %10d %10d\n",
              summary_stats$ethnicity_category[i],
              summary_stats$n_groups[i],
              summary_stats$avg_mamdani_cor[i],
              summary_stats$median_mamdani_cor[i],
              summary_stats$n_positive[i],
              summary_stats$n_negative[i]))
}

cat("\n")

# ============================================================
# COMPREHENSIVE VISUALIZATIONS
# ============================================================

cat("📊 Creating comprehensive visualizations...\n")

# 1. Overall comparison - all groups ranked
p1 <- ggplot(all_groups, aes(x = reorder(group_clean, cor_mamdani_nonzero),
                              y = cor_mamdani_nonzero,
                              fill = ethnicity_category)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(
    values = c("Asian" = "#e41a1c",
               "Hispanic/Latino" = "#377eb8",
               "White Ethnic" = "#4daf4a"),
    name = "Ethnicity"
  ) +
  labs(
    title = "Mamdani Support by Ethnic Subgroup",
    subtitle = "All groups with n≥5 EDs (non-zero population)",
    x = NULL,
    y = "Correlation with Mamdani Vote %"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    axis.text.y = element_text(size = 7)
  )

# 2. Box plot by ethnicity category
p2 <- ggplot(all_groups, aes(x = ethnicity_category, y = cor_mamdani_nonzero, fill = ethnicity_category)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(
    values = c("Asian" = "#e41a1c",
               "Hispanic/Latino" = "#377eb8",
               "White Ethnic" = "#4daf4a"),
    guide = "none"
  ) +
  labs(
    title = "Distribution of Mamdani Support by Ethnicity",
    subtitle = "Each point is one ethnic subgroup",
    x = NULL,
    y = "Correlation with Mamdani %"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 0)
  )

# 3. Mobilization quadrant plot (all groups)
p3 <- ggplot(all_groups, aes(x = cor_mamdani_nonzero, y = cor_turnout_nonzero)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(size = n_eds_nonzero, color = ethnicity_category), alpha = 0.6) +
  geom_text(data = all_groups %>% filter(abs(cor_mamdani_nonzero) > 0.3 | abs(cor_turnout_nonzero) > 0.4),
            aes(label = group_clean, color = ethnicity_category),
            hjust = -0.1, vjust = 0.5, size = 2.5, show.legend = FALSE) +
  scale_size_continuous(name = "N EDs", range = c(1, 8)) +
  scale_color_manual(
    values = c("Asian" = "#e41a1c",
               "Hispanic/Latino" = "#377eb8",
               "White Ethnic" = "#4daf4a"),
    name = "Ethnicity"
  ) +
  labs(
    title = "Electoral Mobilization Patterns by Ethnicity",
    subtitle = "Mamdani support vs turnout correlation",
    x = "Correlation with Mamdani Support",
    y = "Correlation with Turnout",
    caption = "Upper right = Mobilized Mamdani base | Lower right = Mamdani supporters, low turnout\nUpper left = High turnout, oppose Mamdani | Lower left = Low turnout, oppose Mamdani"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right"
  )

# 4. Top supporters and opponents side-by-side
top_15_each <- bind_rows(
  all_groups %>% arrange(desc(cor_mamdani_nonzero)) %>% head(15) %>% mutate(type = "Supporters"),
  all_groups %>% arrange(cor_mamdani_nonzero) %>% head(15) %>% mutate(type = "Opponents")
)

p4 <- ggplot(top_15_each, aes(x = reorder(group_clean, cor_mamdani_nonzero),
                               y = cor_mamdani_nonzero,
                               fill = ethnicity_category)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c("Asian" = "#e41a1c",
               "Hispanic/Latino" = "#377eb8",
               "White Ethnic" = "#4daf4a"),
    name = "Ethnicity"
  ) +
  labs(
    title = "Top 15 Supporters and Opponents",
    x = NULL,
    y = "Correlation with Mamdani %"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

# Combine plots
dir.create("outputs/analysis", showWarnings = FALSE, recursive = TRUE)

# Save individual plots
ggsave("outputs/analysis/ethnic_comparison_all_groups.png", p1,
       width = 12, height = 16, dpi = 300)
cat("  ✓ Saved: ethnic_comparison_all_groups.png\n")

ggsave("outputs/analysis/ethnic_comparison_distribution.png", p2,
       width = 10, height = 8, dpi = 300)
cat("  ✓ Saved: ethnic_comparison_distribution.png\n")

ggsave("outputs/analysis/ethnic_comparison_mobilization.png", p3,
       width = 14, height = 10, dpi = 300)
cat("  ✓ Saved: ethnic_comparison_mobilization.png\n")

ggsave("outputs/analysis/ethnic_comparison_top_groups.png", p4,
       width = 12, height = 10, dpi = 300)
cat("  ✓ Saved: ethnic_comparison_top_groups.png\n")

# Save combined multi-panel figure
combined <- (p2 | p3) / p4 +
  plot_annotation(
    title = "Comprehensive Ethnic Analysis: Mamdani Support in AD34",
    subtitle = "2025 Mayoral Election",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  )

ggsave("outputs/analysis/ethnic_comparison_combined.png", combined,
       width = 18, height = 16, dpi = 300)
cat("  ✓ Saved: ethnic_comparison_combined.png\n\n")

# ============================================================
# SAVE SUMMARY TABLES
# ============================================================

cat("💾 Saving summary tables...\n")

# All groups ranked
write_csv(all_groups %>% select(group_clean, ethnicity_category, n_eds_nonzero,
                                 cor_mamdani_nonzero, cor_turnout_nonzero, mobilization_score),
          "outputs/analysis/ethnic_all_groups_summary.csv")
cat("  ✓ Saved: ethnic_all_groups_summary.csv\n")

# Summary by category
write_csv(summary_stats, "outputs/analysis/ethnic_category_summary.csv")
cat("  ✓ Saved: ethnic_category_summary.csv\n")

# Top supporters
write_csv(top_supporters %>% select(group_clean, ethnicity_category, n_eds_nonzero,
                                     cor_mamdani_nonzero, cor_turnout_nonzero),
          "outputs/analysis/ethnic_top_supporters.csv")
cat("  ✓ Saved: ethnic_top_supporters.csv\n")

# Top opponents
write_csv(top_opponents %>% select(group_clean, ethnicity_category, n_eds_nonzero,
                                    cor_mamdani_nonzero, cor_turnout_nonzero),
          "outputs/analysis/ethnic_top_opponents.csv")
cat("  ✓ Saved: ethnic_top_opponents.csv\n")

# Mobilized base
write_csv(mobilized_base %>% select(group_clean, ethnicity_category, n_eds_nonzero,
                                     cor_mamdani_nonzero, cor_turnout_nonzero, mobilization_score),
          "outputs/analysis/ethnic_mobilized_base.csv")
cat("  ✓ Saved: ethnic_mobilized_base.csv\n\n")

cat("✅ Comprehensive ethnic comparison complete!\n\n")
cat("🎯 KEY TAKEAWAYS:\n")
cat("=================\n")
cat("1. Top Mamdani supporters:", top_supporters$group_clean[1], "(", top_supporters$ethnicity_category[1], ", r=", round(top_supporters$cor_mamdani_nonzero[1], 3), ")\n")
cat("2. Strongest opponents:", top_opponents$group_clean[1], "(", top_opponents$ethnicity_category[1], ", r=", round(top_opponents$cor_mamdani_nonzero[1], 3), ")\n")
cat("3. Most mobilized:", ifelse(nrow(mobilized_base) > 0, paste0(mobilized_base$group_clean[1], " (", mobilized_base$ethnicity_category[1], ")"), "None"), "\n")
cat("4. Overall: ", summary_stats$ethnicity_category[1], " groups show strongest avg support (r=", round(summary_stats$avg_mamdani_cor[1], 3), ")\n")
