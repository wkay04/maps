# ============================================================
# Correlate Demographics with Mamdani Vote Share
# ============================================================
# Find which demographic factors predict Zohran Mamdani support

library(tidyverse)
library(corrplot)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n📊 Demographic Correlation Analysis\n")
cat("====================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📂 Loading data...\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Zohran_Mamdani, Andrew_Cuomo, Curtis_Sliwa,
         total_votes, Mamdani_pct, Cuomo_pct, Sliwa_pct, winner)

# Load demographics
demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE)

cat("  ✓ Loaded election results for", nrow(election_results), "EDs\n")
cat("  ✓ Loaded demographics for", nrow(demographics), "EDs\n\n")

# ============================================================
# MERGE DATA
# ============================================================

cat("🔗 Merging datasets...\n")

merged_data <- election_results %>%
  left_join(demographics, by = c("ED", "ED_num")) %>%
  filter(!is.na(Mamdani_pct), !is.na(pct_hispanic))  # Remove any missing data

cat("  ✓ Merged data for", nrow(merged_data), "EDs\n\n")

# ============================================================
# CALCULATE CORRELATIONS
# ============================================================

cat("📈 Calculating correlations with Mamdani vote share...\n\n")

# Select demographic variables for correlation
demo_vars <- merged_data %>%
  select(
    Mamdani_pct,
    pct_white, pct_black, pct_asian, pct_hispanic,
    median_age, median_income, pct_poverty,
    pct_bachelors_plus, pct_renter, median_rent,
    pct_spanish_home, pct_asian_lang_home
  ) %>%
  rename(
    `Mamdani Vote %` = Mamdani_pct,
    `% White` = pct_white,
    `% Black` = pct_black,
    `% Asian` = pct_asian,
    `% Hispanic` = pct_hispanic,
    `Median Age` = median_age,
    `Median Income` = median_income,
    `% in Poverty` = pct_poverty,
    `% Bachelor's+` = pct_bachelors_plus,
    `% Renters` = pct_renter,
    `Median Rent` = median_rent,
    `% Spanish at Home` = pct_spanish_home,
    `% Asian Language at Home` = pct_asian_lang_home
  )

# Remove columns with zero variance (causes NA correlations)
demo_vars_clean <- demo_vars %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

# Calculate correlation matrix
cor_matrix <- cor(demo_vars_clean, use = "pairwise.complete.obs")

# Extract correlations with Mamdani vote share
mamdani_correlations <- cor_matrix[, "Mamdani Vote %", drop = FALSE] %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(correlation = `Mamdani Vote %`) %>%
  filter(variable != "Mamdani Vote %") %>%
  arrange(desc(abs(correlation)))

cat("🔝 TOP CORRELATIONS WITH MAMDANI VOTE SHARE:\n")
cat(rep("=", 60), "\n\n")

for (i in 1:nrow(mamdani_correlations)) {
  var <- mamdani_correlations$variable[i]
  cor_val <- mamdani_correlations$correlation[i]
  direction <- ifelse(cor_val > 0, "↑", "↓")

  cat(sprintf("%2d. %s %s: %.3f\n", i, direction, var, cor_val))
}

cat("\n")

# Save correlations
write_csv(mamdani_correlations, "outputs/analysis/mamdani_demographic_correlations.csv")
cat("✅ Saved: outputs/analysis/mamdani_demographic_correlations.csv\n\n")

# ============================================================
# CREATE CORRELATION PLOT
# ============================================================

cat("📊 Creating correlation heatmap...\n")

png("outputs/analysis/demographic_correlation_heatmap.png",
    width = 12, height = 10, units = "in", res = 300)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#C1121F", "white", "#2E86AB"))(200),
         title = "Demographic Correlations with Mamdani Vote Share - AD34",
         mar = c(0,0,2,0))

dev.off()

cat("  ✓ Saved: outputs/analysis/demographic_correlation_heatmap.png\n\n")

# ============================================================
# CREATE SCATTER PLOTS FOR TOP CORRELATIONS
# ============================================================

cat("📊 Creating scatter plots for top correlations...\n")

# Get top 6 strongest correlations
top_vars <- head(mamdani_correlations, 6)

# Map display names to column names
var_mapping <- list(
  "% Asian" = "pct_asian",
  "Median Age" = "median_age",
  "% Renters" = "pct_renter",
  "% Black" = "pct_black",
  "Median Rent" = "median_rent",
  "Median Income" = "median_income",
  "% Hispanic" = "pct_hispanic",
  "% Bachelor's+" = "pct_bachelors_plus",
  "% White" = "pct_white"
)

# Create scatter plots
scatter_plots <- list()

for (i in 1:nrow(top_vars)) {
  var_name <- top_vars$variable[i]
  orig_col <- var_mapping[[var_name]]

  if (!is.null(orig_col) && orig_col %in% names(merged_data)) {
    p <- ggplot(merged_data, aes(x = .data[[orig_col]], y = Mamdani_pct)) +
      geom_point(aes(size = total_votes), alpha = 0.6, color = "#2E86AB") +
      geom_smooth(method = "lm", se = TRUE, color = "#C1121F", linewidth = 1.5) +
      labs(
        title = paste0("Mamdani Support vs ", var_name),
        subtitle = paste0("Correlation: ", round(top_vars$correlation[i], 3)),
        x = var_name,
        y = "Mamdani Vote %",
        size = "Total Votes"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "bottom"
      )

    scatter_plots[[i]] <- p
  }
}

# Save combined plot
library(patchwork)

combined_plot <- wrap_plots(scatter_plots, ncol = 2) +
  plot_annotation(
    title = "Demographic Predictors of Mamdani Support - AD34",
    subtitle = "2025 Mayoral Election",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  )

ggsave("outputs/analysis/mamdani_demographic_scatterplots.png",
       combined_plot,
       width = 16, height = 12, dpi = 300)

cat("  ✓ Saved: outputs/analysis/mamdani_demographic_scatterplots.png\n\n")

# ============================================================
# STATISTICAL SUMMARY
# ============================================================

cat("📊 STATISTICAL SUMMARY:\n")
cat(rep("=", 60), "\n\n")

# Run a simple linear regression with top 3 predictors
model <- lm(Mamdani_pct ~ pct_asian + median_age + pct_renter, data = merged_data)

cat("Multiple Regression Model (Top 3 Predictors):\n")
print(summary(model))

cat("\n✅ Analysis complete!\n")
