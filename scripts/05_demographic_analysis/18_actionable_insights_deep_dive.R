# ============================================================
# ACTIONABLE INSIGHTS DEEP DIVE
# ============================================================
# Cross-tabulating all demographic and electoral data to find:
# 1. High-opportunity EDs (low turnout + favorable demographics)
# 2. At-risk EDs (high support but vulnerable to opposition)
# 3. Persuasion targets (swing demographics)
# 4. Efficiency gaps (where support doesn't match turnout)
# ============================================================

library(tidyverse)
library(sf)

setwd("~/Mayoral Results AD34")

cat("\n🎯 ACTIONABLE INSIGHTS DEEP DIVE\n")
cat("=================================\n\n")

# ============================================================
# LOAD ALL AVAILABLE DATA
# ============================================================

cat("📥 Loading all analysis data...\n")

# Election results
election <- read_csv("data/intermediate/ad34_mayor_2025_with_cuomo.csv",
                     col_types = cols(ED = col_character()),
                     show_col_types = FALSE)

# Demographics
demographics <- read_csv("data/intermediate/ad34_ed_demographics.csv",
                        col_types = cols(ED = col_character()),
                        show_col_types = FALSE)

# Ethnic data (corrected)
asian <- read_csv("outputs/analysis/corrected/asian_by_ed_b02015.csv",
                  col_types = cols(ED = col_character()),
                  show_col_types = FALSE)

hispanic <- read_csv("outputs/analysis/corrected/hispanic_by_ed_b03001.csv",
                     col_types = cols(ED = col_character()),
                     show_col_types = FALSE)

white <- read_csv("outputs/analysis/corrected/white_by_ed_b04006.csv",
                  col_types = cols(ED = col_character()),
                  show_col_types = FALSE)

# Turnout metrics
turnout <- read_csv("outputs/analysis/corrected/turnout_metrics_by_ed.csv",
                    col_types = cols(ED = col_character()),
                    show_col_types = FALSE)

# Religious proxies
religious_proxy <- read_csv("outputs/analysis/corrected/religious_proxy_by_ed.csv",
                           col_types = cols(ED = col_character()),
                           show_col_types = FALSE)

# Religious proximity
religious_proximity <- read_csv("outputs/analysis/corrected/religious_proximity_by_ed.csv",
                               col_types = cols(ED = col_character()),
                               show_col_types = FALSE)

cat("  ✓ Loaded all datasets\n\n")

# ============================================================
# MERGE INTO MASTER DATASET
# ============================================================

cat("🔗 Creating master analysis dataset...\n")

master <- election %>%
  left_join(demographics, by = "ED") %>%
  left_join(
    asian %>% select(ED, bangladeshi, pakistani, chinese, korean, filipino,
                    asian_indian, vietnamese),
    by = "ED"
  ) %>%
  left_join(
    hispanic %>% select(ED, mexican, puerto_rican, dominican, ecuadorian,
                       colombian, salvadoran),
    by = "ED"
  ) %>%
  left_join(
    white %>% select(ED, italian, irish, polish, russian, greek),
    by = "ED"
  ) %>%
  left_join(
    turnout %>% select(ED, turnout_rate, turnout_strength),
    by = "ED"
  ) %>%
  left_join(
    religious_proxy %>% select(ED, muslim_proxy, hindu_proxy, jewish_proxy),
    by = "ED"
  ) %>%
  left_join(
    religious_proximity %>% select(ED, dist_to_mosque, mosques_nearby,
                                  dist_to_synagogue, synagogues_nearby),
    by = "ED"
  ) %>%
  filter(!is.na(Mamdani_pct), !is.na(total_pop))

cat("  ✓ Master dataset created:", nrow(master), "EDs\n\n")

# ============================================================
# CALCULATE DERIVED METRICS
# ============================================================

cat("📊 Calculating strategic metrics...\n")

master <- master %>%
  mutate(
    # Vote totals by candidate
    Mamdani_votes = round(Mamdani_pct / 100 * total_votes),
    Cuomo_votes = round(Cuomo_pct / 100 * total_votes),
    Sliwa_votes = round(Sliwa_pct / 100 * total_votes),

    # Margin
    margin = Mamdani_pct - Cuomo_pct,
    margin_votes = Mamdani_votes - Cuomo_votes,

    # Turnout potential
    potential_voters = total_pop - total_votes,
    turnout_gap = 1 - turnout_rate,

    # Support categories
    support_category = case_when(
      Mamdani_pct >= 60 ~ "Strong Support (60%+)",
      Mamdani_pct >= 50 ~ "Moderate Support (50-60%)",
      Mamdani_pct >= 45 ~ "Competitive (45-50%)",
      TRUE ~ "Opposition (< 45%)"
    ),

    # Turnout categories
    turnout_category = case_when(
      turnout_rate >= 0.30 ~ "High Turnout (30%+)",
      turnout_rate >= 0.20 ~ "Medium Turnout (20-30%)",
      TRUE ~ "Low Turnout (<20%)"
    ),

    # Strategic classification
    strategic_type = case_when(
      Mamdani_pct >= 60 & turnout_rate < 0.25 ~ "HIGH OPPORTUNITY",
      Mamdani_pct >= 55 & turnout_rate < 0.20 ~ "MEDIUM OPPORTUNITY",
      Mamdani_pct >= 50 & Mamdani_pct < 55 & turnout_rate >= 0.25 ~ "PROTECT & EXPAND",
      Mamdani_pct >= 45 & Mamdani_pct < 50 ~ "SWING - PERSUADE",
      Mamdani_pct < 45 & Mamdani_pct >= 40 ~ "UPHILL PERSUASION",
      TRUE ~ "OPPOSITION TERRITORY"
    ),

    # South Asian combined
    south_asian_total = bangladeshi + pakistani + asian_indian,
    south_asian_pct = 100 * south_asian_total / total_pop
  )

cat("  ✓ Strategic metrics calculated\n\n")

# ============================================================
# STRATEGIC OPPORTUNITY ANALYSIS
# ============================================================

cat("🎯 STRATEGIC OPPORTUNITY ANALYSIS\n")
cat("==================================\n\n")

cat("HIGH OPPORTUNITY EDs (Strong Support + Low Turnout):\n")
cat("Strategy: Maximize turnout to convert support into votes\n\n")
cat(sprintf("%-8s %12s %12s %15s %15s\n",
            "ED", "Mamdani %", "Turnout %", "Potential", "Margin"))
cat(rep("-", 70), "\n")

high_opp <- master %>%
  filter(strategic_type == "HIGH OPPORTUNITY") %>%
  arrange(desc(potential_voters))

if (nrow(high_opp) > 0) {
  for (i in 1:min(10, nrow(high_opp))) {
    cat(sprintf("%-8s %12.1f %12.1f %15.0f %15.1f\n",
                high_opp$ED[i],
                high_opp$Mamdani_pct[i],
                high_opp$turnout_rate[i] * 100,
                high_opp$potential_voters[i],
                high_opp$margin[i]))
  }
} else {
  cat("  (None identified)\n")
}

cat("\n")

cat("PROTECT & EXPAND EDs (Moderate Support + High Turnout):\n")
cat("Strategy: Maintain support, focus on persuasion at margins\n\n")
cat(sprintf("%-8s %12s %12s %15s %15s\n",
            "ED", "Mamdani %", "Turnout %", "Total Votes", "Margin"))
cat(rep("-", 70), "\n")

protect <- master %>%
  filter(strategic_type == "PROTECT & EXPAND") %>%
  arrange(desc(total_votes))

if (nrow(protect) > 0) {
  for (i in 1:min(10, nrow(protect))) {
    cat(sprintf("%-8s %12.1f %12.1f %15.0f %15.1f\n",
                protect$ED[i],
                protect$Mamdani_pct[i],
                protect$turnout_rate[i] * 100,
                protect$total_votes[i],
                protect$margin[i]))
  }
} else {
  cat("  (None identified)\n")
}

cat("\n")

cat("SWING EDs (Competitive + Persuadable):\n")
cat("Strategy: Intensive persuasion, door-to-door, retail politics\n\n")
cat(sprintf("%-8s %12s %12s %15s %15s\n",
            "ED", "Mamdani %", "Cuomo %", "Margin", "Total Votes"))
cat(rep("-", 70), "\n")

swing <- master %>%
  filter(strategic_type == "SWING - PERSUADE") %>%
  arrange(margin)

if (nrow(swing) > 0) {
  for (i in 1:min(10, nrow(swing))) {
    cat(sprintf("%-8s %12.1f %12.1f %15.1f %15.0f\n",
                swing$ED[i],
                swing$Mamdani_pct[i],
                swing$Cuomo_pct[i],
                swing$margin[i],
                swing$total_votes[i]))
  }
} else {
  cat("  (None identified)\n")
}

cat("\n")

# ============================================================
# DEMOGRAPHIC TARGETING OPPORTUNITIES
# ============================================================

cat("👥 DEMOGRAPHIC TARGETING OPPORTUNITIES\n")
cat("=======================================\n\n")

cat("EDs WITH HIGH BANGLADESHI/PAKISTANI POPULATION:\n")
cat("(Muslim-proxy, strong Mamdani support base)\n\n")
cat(sprintf("%-8s %15s %12s %12s %12s\n",
            "ED", "South Asian", "Mamdani %", "Turnout %", "Opportunity"))
cat(rep("-", 70), "\n")

south_asian_eds <- master %>%
  filter(south_asian_total > 100) %>%
  arrange(desc(south_asian_total)) %>%
  head(15)

for (i in 1:nrow(south_asian_eds)) {
  cat(sprintf("%-8s %15.0f %12.1f %12.1f %12s\n",
              south_asian_eds$ED[i],
              south_asian_eds$south_asian_total[i],
              south_asian_eds$Mamdani_pct[i],
              south_asian_eds$turnout_rate[i] * 100,
              south_asian_eds$strategic_type[i]))
}

cat("\n")

cat("EDs WITH HIGH HISPANIC POPULATION (MIXED SUPPORT):\n")
cat("(Dominican/Ecuadorian tend opposing, Mexican/Central Am mixed)\n\n")
cat(sprintf("%-8s %12s %12s %12s %12s\n",
            "ED", "Dominican", "Ecuadorian", "Mamdani %", "Opportunity"))
cat(rep("-", 65), "\n")

hispanic_eds <- master %>%
  filter(dominican > 100 | ecuadorian > 100) %>%
  arrange(desc(dominican + ecuadorian)) %>%
  head(10)

for (i in 1:nrow(hispanic_eds)) {
  cat(sprintf("%-8s %12.0f %12.0f %12.1f %12s\n",
              hispanic_eds$ED[i],
              hispanic_eds$dominican[i],
              hispanic_eds$ecuadorian[i],
              hispanic_eds$Mamdani_pct[i],
              if_else(hispanic_eds$Mamdani_pct[i] < 50, "PERSUADE", "MAINTAIN")))
}

cat("\n")

# ============================================================
# EFFICIENCY ANALYSIS
# ============================================================

cat("⚡ VOTE EFFICIENCY ANALYSIS\n")
cat("============================\n\n")

cat("OVER-PERFORMING EDs (High support relative to base demographics):\n")
cat("(Effective persuasion or strong local organizing)\n\n")

# Calculate expected support based on demographics
master <- master %>%
  mutate(
    expected_mamdani = 50 + # baseline
      (south_asian_pct * 0.3) + # South Asian bonus
      (if_else(dist_to_mosque < 500 & !is.na(dist_to_mosque), 10, 0)) + # Mosque proximity
      (if_else(dominican > 200, -5, 0)) - # Dominican penalty
      (if_else(jewish_proxy > 50, -8, 0)), # Jewish penalty
    performance_gap = Mamdani_pct - expected_mamdani
  )

cat(sprintf("%-8s %15s %15s %12s\n",
            "ED", "Actual Mamdani", "Expected", "Gap"))
cat(rep("-", 55), "\n")

overperform <- master %>%
  filter(!is.na(performance_gap)) %>%
  arrange(desc(performance_gap)) %>%
  head(10)

for (i in 1:nrow(overperform)) {
  cat(sprintf("%-8s %15.1f %15.1f %12.1f\n",
              overperform$ED[i],
              overperform$Mamdani_pct[i],
              overperform$expected_mamdani[i],
              overperform$performance_gap[i]))
}

cat("\n")

cat("UNDER-PERFORMING EDs (Low support relative to favorable demographics):\n")
cat("(Opportunity for improvement or demographic misread)\n\n")

cat(sprintf("%-8s %15s %15s %12s\n",
            "ED", "Actual Mamdani", "Expected", "Gap"))
cat(rep("-", 55), "\n")

underperform <- master %>%
  filter(!is.na(performance_gap)) %>%
  arrange(performance_gap) %>%
  head(10)

for (i in 1:nrow(underperform)) {
  cat(sprintf("%-8s %15.1f %15.1f %12.1f\n",
              underperform$ED[i],
              underperform$Mamdani_pct[i],
              underperform$expected_mamdani[i],
              underperform$performance_gap[i]))
}

cat("\n")

# ============================================================
# VOTE MAXIMIZATION SCENARIOS
# ============================================================

cat("📈 VOTE MAXIMIZATION SCENARIOS\n")
cat("===============================\n\n")

# Scenario 1: Increase turnout in high-support EDs
cat("SCENARIO 1: Boost turnout to 30% in strong support EDs (60%+ Mamdani)\n")
cat("------------------------------------------------------------------------\n")

strong_support <- master %>%
  filter(Mamdani_pct >= 60, turnout_rate < 0.30)

if (nrow(strong_support) > 0) {
  scenario1_gains <- strong_support %>%
    mutate(
      new_turnout = 0.30,
      new_total_votes = total_pop * new_turnout,
      new_mamdani_votes = new_total_votes * (Mamdani_pct / 100),
      vote_gain = new_mamdani_votes - Mamdani_votes
    )

  total_gain_s1 <- sum(scenario1_gains$vote_gain, na.rm = TRUE)

  cat(sprintf("  EDs targeted: %d\n", nrow(strong_support)))
  cat(sprintf("  Estimated net vote gain: %.0f votes\n", total_gain_s1))
  cat(sprintf("  Avg per ED: %.0f votes\n\n", total_gain_s1 / nrow(strong_support)))

  cat("Top opportunities:\n")
  cat(sprintf("%-8s %15s %15s %12s\n",
              "ED", "Current Turnout", "Potential Gain", "Mamdani %"))
  cat(rep("-", 55), "\n")

  top_s1 <- scenario1_gains %>%
    arrange(desc(vote_gain)) %>%
    head(5)

  for (i in 1:nrow(top_s1)) {
    cat(sprintf("%-8s %15.1f %15.0f %12.1f\n",
                top_s1$ED[i],
                top_s1$turnout_rate[i] * 100,
                top_s1$vote_gain[i],
                top_s1$Mamdani_pct[i]))
  }
} else {
  cat("  No EDs qualify for this scenario\n")
}

cat("\n")

# Scenario 2: Flip swing EDs
cat("SCENARIO 2: Flip competitive EDs from 45-50% to 50%+ Mamdani\n")
cat("-------------------------------------------------------------\n")

competitive <- master %>%
  filter(Mamdani_pct >= 45, Mamdani_pct < 50)

if (nrow(competitive) > 0) {
  scenario2_gains <- competitive %>%
    mutate(
      persuasion_target = 52, # Modest goal
      new_mamdani_pct = persuasion_target,
      new_mamdani_votes = total_votes * (new_mamdani_pct / 100),
      vote_gain = new_mamdani_votes - Mamdani_votes,
      voters_to_persuade = vote_gain
    )

  total_gain_s2 <- sum(scenario2_gains$vote_gain, na.rm = TRUE)

  cat(sprintf("  EDs targeted: %d\n", nrow(competitive)))
  cat(sprintf("  Estimated net vote gain: %.0f votes\n", total_gain_s2))
  cat(sprintf("  Voters to persuade: %.0f\n\n", sum(scenario2_gains$voters_to_persuade)))

  cat("Top opportunities:\n")
  cat(sprintf("%-8s %15s %15s %12s\n",
              "ED", "Current Mam %", "Voters Needed", "Total Votes"))
  cat(rep("-", 60), "\n")

  top_s2 <- scenario2_gains %>%
    arrange(desc(vote_gain)) %>%
    head(5)

  for (i in 1:nrow(top_s2)) {
    cat(sprintf("%-8s %15.1f %15.0f %12.0f\n",
                top_s2$ED[i],
                top_s2$Mamdani_pct[i],
                top_s2$voters_to_persuade[i],
                top_s2$total_votes[i]))
  }
} else {
  cat("  No EDs qualify for this scenario\n")
}

cat("\n")

# ============================================================
# SAVE ACTIONABLE DATASETS
# ============================================================

cat("💾 SAVING ACTIONABLE INSIGHTS\n")
cat("==============================\n\n")

dir.create("outputs/analysis/actionable", showWarnings = FALSE, recursive = TRUE)

# Master dataset with all strategic metrics
write_csv(master, "outputs/analysis/actionable/master_strategic_dataset.csv")
cat("  ✓ Saved: master_strategic_dataset.csv\n")

# High priority targeting list
priority_eds <- master %>%
  filter(strategic_type %in% c("HIGH OPPORTUNITY", "PROTECT & EXPAND", "SWING - PERSUADE")) %>%
  select(ED, ED_num, Mamdani_pct, turnout_rate, total_votes,
         margin, strategic_type, south_asian_total, muslim_proxy,
         dist_to_mosque, potential_voters) %>%
  arrange(desc(potential_voters))

write_csv(priority_eds, "outputs/analysis/actionable/priority_targeting_list.csv")
cat("  ✓ Saved: priority_targeting_list.csv\n")

# South Asian community EDs
south_asian_list <- master %>%
  filter(south_asian_total > 50) %>%
  select(ED, ED_num, south_asian_total, bangladeshi, pakistani, asian_indian,
         Mamdani_pct, turnout_rate, muslim_proxy, dist_to_mosque) %>%
  arrange(desc(south_asian_total))

write_csv(south_asian_list, "outputs/analysis/actionable/south_asian_communities.csv")
cat("  ✓ Saved: south_asian_communities.csv\n")

cat("\n✅ ACTIONABLE INSIGHTS ANALYSIS COMPLETE!\n")
cat("   Check outputs/analysis/actionable/ for targeting lists\n")
