# ============================================================
# Detailed Asian Ethnic Group Analysis
# ============================================================
# Break down Asian population by specific ethnic groups
# to see which correlate most strongly with Mamdani support

library(tidyverse)
library(tidycensus)
library(sf)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🌏 Detailed Asian Ethnic Group Analysis - AD34\n")
cat("===============================================\n\n")

# ============================================================
# LOAD DETAILED ASIAN ANCESTRY DATA
# ============================================================

cat("📥 Downloading detailed Asian ancestry data...\n")

# Get detailed Asian alone population by ancestry
# Using table B02015 (Asian Alone Population)
asian_vars <- c(
  total_asian = "B02015_001",

  # South Asian
  asian_indian = "B02015_002",
  bangladeshi = "B02015_003",
  pakistani = "B02015_006",
  sri_lankan = "B02015_007",

  # East Asian
  chinese = "B02015_009",
  japanese = "B02015_013",
  korean = "B02015_014",

  # Southeast Asian
  filipino = "B02015_010",
  vietnamese = "B02015_018",
  thai = "B02015_016",

  # Other
  other_asian = "B02015_019"
)

# Get data for Queens County at TRACT level (block group data is mostly NA)
queens_asian <- get_acs(
  geography = "tract",
  variables = asian_vars,
  state = "NY",
  county = "Queens",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

cat("  ✓ Downloaded data for", nrow(queens_asian), "census tracts\n\n")

# ============================================================
# SPATIAL JOIN WITH ELECTION DISTRICTS
# ============================================================

cat("🗺️  Joining with Election Districts...\n")

shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED = Election_D,
    ED_num = as.numeric(str_sub(Election_D, 3, -1))
  ) %>%
  st_transform(st_crs(queens_asian))

# Get block group geometries and calculate original areas
queens_asian <- queens_asian %>%
  mutate(bg_area = as.numeric(st_area(geometry)))

# Spatial intersection
ed_asian <- st_intersection(
  ad34_shp %>% select(ED, ED_num, geometry),
  queens_asian
) %>%
  mutate(
    intersection_area = as.numeric(st_area(geometry)),
    # Calculate proportion of block group in this ED
    area_weight = intersection_area / bg_area
  ) %>%
  st_drop_geometry()

# Aggregate to ED level (area-weighted)
ed_asian_agg <- ed_asian %>%
  group_by(ED, ED_num) %>%
  summarize(
    total_asian_pop = sum(total_asian * area_weight, na.rm = TRUE),
    asian_indian = sum(asian_indian * area_weight, na.rm = TRUE),
    bangladeshi = sum(bangladeshi * area_weight, na.rm = TRUE),
    pakistani = sum(pakistani * area_weight, na.rm = TRUE),
    sri_lankan = sum(sri_lankan * area_weight, na.rm = TRUE),
    chinese = sum(chinese * area_weight, na.rm = TRUE),
    japanese = sum(japanese * area_weight, na.rm = TRUE),
    korean = sum(korean * area_weight, na.rm = TRUE),
    filipino = sum(filipino * area_weight, na.rm = TRUE),
    vietnamese = sum(vietnamese * area_weight, na.rm = TRUE),
    thai = sum(thai * area_weight, na.rm = TRUE),
    other_asian = sum(other_asian * area_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate grouped totals
    south_asian = asian_indian + bangladeshi + pakistani + sri_lankan,
    east_asian = chinese + japanese + korean,
    southeast_asian = filipino + vietnamese + thai
  )

cat("  ✓ Aggregated for", nrow(ed_asian_agg), "EDs\n\n")

# ============================================================
# MERGE WITH ELECTION RESULTS
# ============================================================

cat("🔗 Merging with election results...\n")

# Load election results
election_results <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                             col_types = cols(ED = col_character()),
                             show_col_types = FALSE) %>%
  select(ED, ED_num, Mamdani_pct, total_votes)

# Also need total population for percentages
demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE) %>%
  select(ED, total_pop)

merged <- election_results %>%
  left_join(ed_asian_agg, by = c("ED", "ED_num")) %>%
  left_join(demographics, by = "ED") %>%
  filter(!is.na(Mamdani_pct)) %>%
  mutate(
    # Calculate percentages
    pct_asian_indian = 100 * asian_indian / total_pop,
    pct_bangladeshi = 100 * bangladeshi / total_pop,
    pct_pakistani = 100 * pakistani / total_pop,
    pct_chinese = 100 * chinese / total_pop,
    pct_korean = 100 * korean / total_pop,
    pct_filipino = 100 * filipino / total_pop,
    pct_south_asian = 100 * south_asian / total_pop,
    pct_east_asian = 100 * east_asian / total_pop,
    pct_southeast_asian = 100 * southeast_asian / total_pop
  )

cat("  ✓ Merged data for", nrow(merged), "EDs\n\n")

# DEBUG: Check what we have
cat("🔍 DEBUG: Checking Asian ethnicity data...\n")
cat("  Sample percentages (first 5 EDs):\n")
debug_sample <- merged %>%
  select(ED_num, pct_bangladeshi, pct_asian_indian, pct_chinese, pct_south_asian) %>%
  head(5)
print(debug_sample)

cat("\n  Summary statistics:\n")
cat(sprintf("    Bangladeshi: mean=%.2f%%, sd=%.2f%%\n",
            mean(merged$pct_bangladeshi, na.rm=TRUE),
            sd(merged$pct_bangladeshi, na.rm=TRUE)))
cat(sprintf("    Indian: mean=%.2f%%, sd=%.2f%%\n",
            mean(merged$pct_asian_indian, na.rm=TRUE),
            sd(merged$pct_asian_indian, na.rm=TRUE)))
cat(sprintf("    Chinese: mean=%.2f%%, sd=%.2f%%\n",
            mean(merged$pct_chinese, na.rm=TRUE),
            sd(merged$pct_chinese, na.rm=TRUE)))
cat(sprintf("    South Asian: mean=%.2f%%, sd=%.2f%%\n\n",
            mean(merged$pct_south_asian, na.rm=TRUE),
            sd(merged$pct_south_asian, na.rm=TRUE)))

# ============================================================
# CALCULATE CORRELATIONS
# ============================================================

cat("📈 Calculating correlations with Mamdani support...\n\n")

# Use absolute counts instead of percentages (since % calculation failed)
# Correlate with raw population counts
asian_cors <- merged %>%
  select(
    Mamdani_pct,
    south_asian, asian_indian, bangladeshi, pakistani,
    east_asian, chinese, korean,
    southeast_asian, filipino
  ) %>%
  # Remove zero variance columns
  select(where(~ sd(., na.rm = TRUE) > 0))

# Calculate correlations
cor_matrix <- cor(asian_cors, use = "pairwise.complete.obs")

# Extract Mamdani correlations
asian_correlations <- cor_matrix[, "Mamdani_pct", drop = FALSE] %>%
  as.data.frame() %>%
  rownames_to_column("group") %>%
  rename(correlation = `Mamdani_pct`) %>%
  filter(group != "Mamdani_pct") %>%
  arrange(desc(abs(correlation))) %>%
  mutate(
    group_clean = str_replace_all(group, "pct_", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

cat("🔝 CORRELATIONS WITH MAMDANI SUPPORT BY ASIAN ETHNIC GROUP:\n")
cat(rep("=", 70), "\n\n")

for (i in 1:nrow(asian_correlations)) {
  group_name <- asian_correlations$group_clean[i]
  cor_val <- asian_correlations$correlation[i]
  direction <- ifelse(cor_val > 0, "↑", "↓")

  cat(sprintf("%2d. %s %s: %.3f\n", i, direction, group_name, cor_val))
}

cat("\n")

# Save
write_csv(asian_correlations, "outputs/analysis/asian_ethnic_correlations.csv")
cat("✅ Saved: outputs/analysis/asian_ethnic_correlations.csv\n\n")

# ============================================================
# CREATE VISUALIZATIONS
# ============================================================

cat("📊 Creating visualizations...\n")

# Bar chart of correlations
cor_plot <- ggplot(asian_correlations,
                   aes(x = reorder(group_clean, correlation), y = correlation)) +
  geom_col(aes(fill = correlation > 0), alpha = 0.8) +
  scale_fill_manual(values = c("#C1121F", "#2E86AB"), guide = "none") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Asian Ethnic Groups - Correlation with Mamdani Support",
    subtitle = "AD34 2025 Mayoral Election",
    x = NULL,
    y = "Correlation Coefficient"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/analysis/asian_ethnic_correlations_bar.png",
       cor_plot, width = 10, height = 8, dpi = 300)

cat("  ✓ Saved: outputs/analysis/asian_ethnic_correlations_bar.png\n")

# Scatter plots for top groups
library(patchwork)

create_scatter <- function(data, var, var_name, cor_val) {
  ggplot(data, aes(x = .data[[var]], y = Mamdani_pct)) +
    geom_point(aes(size = total_votes), alpha = 0.6, color = "#2E86AB") +
    geom_smooth(method = "lm", se = TRUE, color = "#C1121F", linewidth = 1.5) +
    labs(
      title = var_name,
      subtitle = paste0("r = ", round(cor_val, 3)),
      x = paste0(var_name, " Population"),
      y = "Mamdani Vote %",
      size = "Total Votes"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

# Create plots for top 6 (only if we have results)
if (nrow(asian_correlations) > 0) {
  top_groups <- head(asian_correlations, min(6, nrow(asian_correlations)))
  scatter_list <- list()

  for (i in 1:nrow(top_groups)) {
    var <- top_groups$group[i]
    var_name <- top_groups$group_clean[i]
    cor_val <- top_groups$correlation[i]

    if (!is.null(var) && length(var) > 0 && var %in% names(merged)) {
      scatter_list[[i]] <- create_scatter(merged, var, var_name, cor_val)
    }
  }

  # Remove NULL entries
  scatter_list <- scatter_list[!sapply(scatter_list, is.null)]
} else {
  cat("  ⚠️  No valid correlations to plot\n")
  scatter_list <- list()
}

if (length(scatter_list) > 0) {
  combined <- wrap_plots(scatter_list, ncol = 2) +
    plot_annotation(
      title = "Mamdani Support by Asian Ethnic Group - AD34",
      subtitle = "Larger points = higher turnout EDs",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
    )

  ggsave("outputs/analysis/asian_ethnic_scatterplots.png",
         combined, width = 14, height = 12, dpi = 300)

  cat("  ✓ Saved: outputs/analysis/asian_ethnic_scatterplots.png\n\n")
} else {
  cat("  ⚠️  No scatter plots to save\n\n")
}

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("📊 SUMMARY STATISTICS:\n")
cat(rep("=", 70), "\n\n")

cat("Average population by group (across all EDs):\n")
summary_stats <- merged %>%
  summarize(
    `South Asian` = mean(south_asian, na.rm = TRUE),
    `- Bangladeshi` = mean(bangladeshi, na.rm = TRUE),
    `- Indian` = mean(asian_indian, na.rm = TRUE),
    `- Pakistani` = mean(pakistani, na.rm = TRUE),
    `East Asian` = mean(east_asian, na.rm = TRUE),
    `- Chinese` = mean(chinese, na.rm = TRUE),
    `- Korean` = mean(korean, na.rm = TRUE),
    `Southeast Asian` = mean(southeast_asian, na.rm = TRUE),
    `- Filipino` = mean(filipino, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Group", values_to = "Avg_Population")

for (i in 1:nrow(summary_stats)) {
  cat(sprintf("  %s: %.0f people per ED\n",
              summary_stats$Group[i],
              summary_stats$Avg_Population[i]))
}

cat("\n✅ Detailed Asian ethnic analysis complete!\n")
