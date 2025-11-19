# ============================================================
# TURNOUT-WEIGHTED ETHNIC ANALYSIS
# ============================================================
# Adjusts ethnic population correlations by turnout strength
# to measure actual electoral impact, not just population size

library(tidyverse)

setwd("~/Mayoral Results AD34")

cat("\n⚖️  TURNOUT-WEIGHTED ETHNIC ANALYSIS\n")
cat("====================================\n\n")

# ============================================================
# LOAD DATA
# ============================================================

cat("📥 Loading corrected ethnic data...\n")

asian_data <- read_csv("outputs/analysis/corrected/asian_by_ed_b02015.csv",
                       col_types = cols(ED = col_character()),
                       show_col_types = FALSE)

hispanic_data <- read_csv("outputs/analysis/corrected/hispanic_by_ed_b03001.csv",
                          col_types = cols(ED = col_character()),
                          show_col_types = FALSE)

white_data <- read_csv("outputs/analysis/corrected/white_by_ed_b04006.csv",
                       col_types = cols(ED = col_character()),
                       show_col_types = FALSE)

cat("  ✓ Loaded ethnic data\n\n")

# ============================================================
# CALCULATE TURNOUT METRICS
# ============================================================

cat("📊 CALCULATING TURNOUT METRICS\n")
cat("===============================\n\n")

# Calculate turnout rates and z-scores for each ED
turnout_metrics <- asian_data %>%
  select(ED, ED_num, total_votes, total_pop, Mamdani_pct, Cuomo_pct) %>%
  mutate(
    turnout_rate = total_votes / total_pop,
    # Calculate turnout relative to district average
    district_avg_turnout = mean(turnout_rate, na.rm = TRUE),
    turnout_ratio = turnout_rate / district_avg_turnout,
    # Z-score for turnout (how many SDs above/below average)
    turnout_z = (turnout_rate - mean(turnout_rate, na.rm = TRUE)) / sd(turnout_rate, na.rm = TRUE),
    # Turnout strength multiplier (centered at 1.0)
    turnout_strength = turnout_ratio
  )

cat("District average turnout:", round(mean(turnout_metrics$turnout_rate, na.rm = TRUE) * 100, 1), "%\n")
cat("Turnout range:",
    round(min(turnout_metrics$turnout_rate, na.rm = TRUE) * 100, 1), "% to",
    round(max(turnout_metrics$turnout_rate, na.rm = TRUE) * 100, 1), "%\n\n")

# Show EDs with highest/lowest turnout
cat("HIGHEST TURNOUT EDs:\n")
cat(sprintf("%-8s %10s %12s\n", "ED", "Turnout %", "Strength"))
cat(rep("-", 35), "\n")
top_turnout <- turnout_metrics %>% arrange(desc(turnout_rate)) %>% head(5)
for (i in 1:nrow(top_turnout)) {
  cat(sprintf("%-8s %10.1f %12.2fx\n",
              top_turnout$ED[i],
              top_turnout$turnout_rate[i] * 100,
              top_turnout$turnout_strength[i]))
}

cat("\nLOWEST TURNOUT EDs:\n")
cat(sprintf("%-8s %10s %12s\n", "ED", "Turnout %", "Strength"))
cat(rep("-", 35), "\n")
bottom_turnout <- turnout_metrics %>% arrange(turnout_rate) %>% head(5)
for (i in 1:nrow(bottom_turnout)) {
  cat(sprintf("%-8s %10.1f %12.2fx\n",
              bottom_turnout$ED[i],
              bottom_turnout$turnout_rate[i] * 100,
              bottom_turnout$turnout_strength[i]))
}

cat("\n")

# ============================================================
# TURNOUT-WEIGHTED CORRELATION FUNCTION
# ============================================================

calc_turnout_weighted_cors <- function(data, turnout_data, group_vars, category_name) {

  # Merge turnout metrics
  data_with_turnout <- data %>%
    left_join(turnout_data %>% select(ED, turnout_strength, turnout_rate), by = "ED")

  results <- tibble()

  for (var in group_vars) {
    if (!var %in% names(data_with_turnout)) next
    if (sd(data_with_turnout[[var]], na.rm = TRUE) == 0) next

    n_nonzero <- sum(data_with_turnout[[var]] > 0, na.rm = TRUE)
    if (n_nonzero < 3) next

    # Raw population metrics
    total_pop <- sum(data_with_turnout[[var]], na.rm = TRUE)

    # Create turnout-weighted population
    # This represents "electoral strength" not just population
    data_with_turnout <- data_with_turnout %>%
      mutate(
        weighted_pop = .data[[var]] * turnout_strength,
        weighted_pop_simple = .data[[var]] * turnout_rate
      )

    # Correlations - UNWEIGHTED (original)
    cor_unweighted <- cor(data_with_turnout[[var]],
                          data_with_turnout$Mamdani_pct,
                          use = "complete.obs")

    # Correlations - WEIGHTED by turnout strength
    cor_weighted <- cor(data_with_turnout$weighted_pop,
                       data_with_turnout$Mamdani_pct,
                       use = "complete.obs")

    # Non-zero EDs only
    nonzero <- data_with_turnout %>% filter(.data[[var]] > 0)

    cor_unweighted_nz <- NA
    cor_weighted_nz <- NA

    if (nrow(nonzero) >= 3) {
      cor_unweighted_nz <- cor(nonzero[[var]], nonzero$Mamdani_pct, use = "complete.obs")
      cor_weighted_nz <- cor(nonzero$weighted_pop, nonzero$Mamdani_pct, use = "complete.obs")
    }

    # Calculate electoral impact score
    # = population × support correlation × avg turnout in their areas
    avg_turnout_in_group_eds <- mean(nonzero$turnout_rate, na.rm = TRUE)
    electoral_impact <- abs(cor_weighted_nz) * total_pop * avg_turnout_in_group_eds

    results <- bind_rows(results, tibble(
      group = var,
      category = category_name,
      n_eds_nonzero = n_nonzero,
      total_population = total_pop,
      avg_turnout_rate = avg_turnout_in_group_eds,
      cor_unweighted = cor_unweighted,
      cor_weighted = cor_weighted,
      cor_unweighted_nz = cor_unweighted_nz,
      cor_weighted_nz = cor_weighted_nz,
      correlation_change = cor_weighted_nz - cor_unweighted_nz,
      electoral_impact_score = electoral_impact
    ))
  }

  return(results)
}

# ============================================================
# CALCULATE WEIGHTED CORRELATIONS
# ============================================================

cat("🔗 CALCULATING TURNOUT-WEIGHTED CORRELATIONS\n")
cat("=============================================\n\n")

asian_groups <- c("chinese", "japanese", "korean", "taiwanese", "filipino", "vietnamese",
                  "thai", "cambodian", "burmese", "hmong",
                  "asian_indian", "bangladeshi", "pakistani", "nepali", "sri_lankan")

hispanic_groups <- c("mexican", "puerto_rican", "cuban", "dominican",
                     "guatemalan", "honduran", "nicaraguan", "salvadoran",
                     "colombian", "ecuadorian", "peruvian", "venezuelan", "bolivian")

white_groups <- c("italian", "irish", "german", "polish", "russian", "greek",
                  "ukrainian", "czech", "norwegian")

cat("  Processing Asian groups...\n")
asian_results <- calc_turnout_weighted_cors(asian_data, turnout_metrics, asian_groups, "Asian")

cat("  Processing Hispanic groups...\n")
hispanic_results <- calc_turnout_weighted_cors(hispanic_data, turnout_metrics, hispanic_groups, "Hispanic/Latino")

cat("  Processing White ethnic groups...\n")
white_results <- calc_turnout_weighted_cors(white_data, turnout_metrics, white_groups, "White Ethnic")

# Combine all results
all_results <- bind_rows(asian_results, hispanic_results, white_results) %>%
  mutate(
    group_clean = str_replace_all(group, "_", " ") %>% str_to_title()
  )

cat("  ✓ Calculated weighted correlations for", nrow(all_results), "groups\n\n")

# ============================================================
# COMPARE WEIGHTED VS UNWEIGHTED
# ============================================================

cat("📊 COMPARISON: TURNOUT-WEIGHTED VS UNWEIGHTED\n")
cat("==============================================\n\n")

cat("GROUPS WHERE TURNOUT ADJUSTMENT INCREASES CORRELATION:\n")
cat("(Higher electoral impact than raw population suggests)\n\n")
cat(sprintf("%-25s %-20s %10s %10s %10s\n",
            "Group", "Category", "Unwght", "Wght", "Change"))
cat(rep("-", 80), "\n")

increased <- all_results %>%
  filter(!is.na(correlation_change), correlation_change > 0.05) %>%
  arrange(desc(correlation_change))

if (nrow(increased) > 0) {
  for (i in 1:min(15, nrow(increased))) {
    cat(sprintf("%-25s %-20s %10.3f %10.3f %10.3f\n",
                increased$group_clean[i],
                increased$category[i],
                increased$cor_unweighted_nz[i],
                increased$cor_weighted_nz[i],
                increased$correlation_change[i]))
  }
} else {
  cat("  (None with change > 0.05)\n")
}

cat("\n")
cat("GROUPS WHERE TURNOUT ADJUSTMENT DECREASES CORRELATION:\n")
cat("(Lower electoral impact than raw population suggests)\n\n")
cat(sprintf("%-25s %-20s %10s %10s %10s\n",
            "Group", "Category", "Unwght", "Wght", "Change"))
cat(rep("-", 80), "\n")

decreased <- all_results %>%
  filter(!is.na(correlation_change), correlation_change < -0.05) %>%
  arrange(correlation_change)

if (nrow(decreased) > 0) {
  for (i in 1:min(15, nrow(decreased))) {
    cat(sprintf("%-25s %-20s %10.3f %10.3f %10.3f\n",
                decreased$group_clean[i],
                decreased$category[i],
                decreased$cor_unweighted_nz[i],
                decreased$cor_weighted_nz[i],
                decreased$correlation_change[i]))
  }
} else {
  cat("  (None with change < -0.05)\n")
}

cat("\n")

# ============================================================
# ELECTORAL IMPACT RANKING
# ============================================================

cat("🗳️  ELECTORAL IMPACT RANKING\n")
cat("============================\n")
cat("(Population × Support × Turnout)\n\n")

cat("HIGHEST ELECTORAL IMPACT (Pro-Mamdani):\n")
cat(sprintf("%-25s %-20s %10s %12s %10s\n",
            "Group", "Category", "Pop", "Turnout%", "Impact"))
cat(rep("-", 85), "\n")

top_impact <- all_results %>%
  filter(cor_weighted_nz > 0) %>%
  arrange(desc(electoral_impact_score)) %>%
  head(15)

for (i in 1:nrow(top_impact)) {
  cat(sprintf("%-25s %-20s %10.0f %12.1f %10.0f\n",
              top_impact$group_clean[i],
              top_impact$category[i],
              top_impact$total_population[i],
              top_impact$avg_turnout_rate[i] * 100,
              top_impact$electoral_impact_score[i]))
}

cat("\nHIGHEST ELECTORAL IMPACT (Anti-Mamdani):\n")
cat(sprintf("%-25s %-20s %10s %12s %10s\n",
            "Group", "Category", "Pop", "Turnout%", "Impact"))
cat(rep("-", 85), "\n")

anti_impact <- all_results %>%
  filter(cor_weighted_nz < 0) %>%
  arrange(desc(electoral_impact_score)) %>%
  head(15)

for (i in 1:nrow(anti_impact)) {
  cat(sprintf("%-25s %-20s %10.0f %12.1f %10.0f\n",
              anti_impact$group_clean[i],
              anti_impact$category[i],
              anti_impact$total_population[i],
              anti_impact$avg_turnout_rate[i] * 100,
              anti_impact$electoral_impact_score[i]))
}

cat("\n")

# ============================================================
# SAVE RESULTS
# ============================================================

cat("💾 SAVING RESULTS\n")
cat("==================\n\n")

dir.create("outputs/analysis/corrected", showWarnings = FALSE, recursive = TRUE)

write_csv(all_results, "outputs/analysis/corrected/turnout_weighted_correlations.csv")
cat("  ✓ Saved: turnout_weighted_correlations.csv\n")

write_csv(turnout_metrics, "outputs/analysis/corrected/turnout_metrics_by_ed.csv")
cat("  ✓ Saved: turnout_metrics_by_ed.csv\n")

# Create comparison plot data
comparison_plot_data <- all_results %>%
  filter(!is.na(cor_unweighted_nz), !is.na(cor_weighted_nz)) %>%
  select(group_clean, category, cor_unweighted_nz, cor_weighted_nz,
         total_population, electoral_impact_score)

write_csv(comparison_plot_data, "outputs/analysis/corrected/weighted_vs_unweighted_comparison.csv")
cat("  ✓ Saved: weighted_vs_unweighted_comparison.csv\n")

cat("\n✅ TURNOUT-WEIGHTED ANALYSIS COMPLETE!\n")
cat("   This adjusts for electoral impact, not just population size\n")
