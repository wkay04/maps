# ============================================================
# Re-parse 2025 Mayoral Data to Include Andrew Cuomo
# ============================================================
# Cuomo received 9,659 votes, making him 2nd place after Mamdani

library(tidyverse)

setwd("~/Mayoral Results AD34")

cat("\n📊 Re-parsing 2025 Mayoral Results with Cuomo\n")
cat("=============================================\n\n")

# Read the raw text file
all_text <- readLines("data/raw/election/ad34_enr/AD34_Mayor_20251116_1457.csv")
combined_text <- paste(all_text, collapse = " ")

# The ENR data has candidates in this order after percentage:
# 1. Mamdani (Democratic)
# 2. Sliwa (Republican)
# 3. Estrada (Conservative)
# 4. Mamdani (Working Families)
# 5. Sliwa (Protect Animals)
# 6. Adams (Safe&Affordable)
# 7. Hernandez (Quality of Life)
# 8. Cuomo (Fight and Deliver)  ← WE NEED THIS!
# 9. Walden (Integrity)
# 10. Write-in

# Extract ED data using regex
# Pattern: ED + number + percentage + 10 vote counts
ed_pattern <- "ED\\s+(\\d+)\\s+([\\d.]+)%\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)"

matches <- str_match_all(combined_text, ed_pattern)[[1]]

cat("Found", nrow(matches), "EDs\n\n")

if (nrow(matches) > 0) {
  ed_data <- tibble(
    ED_num = as.numeric(matches[,2]),
    pct_reported = as.numeric(matches[,3]),
    mamdani_dem = as.numeric(matches[,4]),
    sliwa_rep = as.numeric(matches[,5]),
    estrada_con = as.numeric(matches[,6]),
    mamdani_wfp = as.numeric(matches[,7]),
    sliwa_protect = as.numeric(matches[,8]),
    adams = as.numeric(matches[,9]),
    hernandez = as.numeric(matches[,10]),
    cuomo = as.numeric(matches[,11]),  # ← CUOMO!
    walden = as.numeric(matches[,12]),
    writein = as.numeric(matches[,13])
  ) %>%
    mutate(
      ED = paste0("34", str_pad(ED_num, 3, pad = "0")),
      # Combine party lines for fusion candidates
      Zohran_Mamdani = mamdani_dem + mamdani_wfp,
      Curtis_Sliwa = sliwa_rep + sliwa_protect,
      Irene_Estrada = estrada_con,
      Eric_Adams = adams,
      Andrew_Cuomo = cuomo  # ← ADD CUOMO!
    ) %>%
    select(ED, ED_num, Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada, Curtis_Sliwa, Eric_Adams) %>%
    mutate(
      total_votes = Zohran_Mamdani + Andrew_Cuomo + Irene_Estrada + Curtis_Sliwa + Eric_Adams,
      # Calculate percentages
      Mamdani_pct = round(100 * Zohran_Mamdani / total_votes, 1),
      Cuomo_pct = round(100 * Andrew_Cuomo / total_votes, 1),
      Estrada_pct = round(100 * Irene_Estrada / total_votes, 1),
      Sliwa_pct = round(100 * Curtis_Sliwa / total_votes, 1),
      Adams_pct = round(100 * Eric_Adams / total_votes, 1),
      # Determine winner
      winner = case_when(
        Zohran_Mamdani == pmax(Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada, Curtis_Sliwa, Eric_Adams) ~ "Mamdani",
        Andrew_Cuomo == pmax(Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada, Curtis_Sliwa, Eric_Adams) ~ "Cuomo",
        Irene_Estrada == pmax(Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada, Curtis_Sliwa, Eric_Adams) ~ "Estrada",
        Curtis_Sliwa == pmax(Zohran_Mamdani, Andrew_Cuomo, Irene_Estrada, Curtis_Sliwa, Eric_Adams) ~ "Sliwa",
        TRUE ~ "Adams"
      )
    )

  cat("✅ Successfully parsed data\n\n")

  cat("📊 TOTALS ACROSS ALL EDs:\n")
  cat(sprintf("  Zohran Mamdani: %s votes\n", format(sum(ed_data$Zohran_Mamdani, na.rm=TRUE), big.mark=",")))
  cat(sprintf("  Andrew Cuomo:   %s votes\n", format(sum(ed_data$Andrew_Cuomo, na.rm=TRUE), big.mark=",")))
  cat(sprintf("  Irene Estrada:  %s votes\n", format(sum(ed_data$Irene_Estrada, na.rm=TRUE), big.mark=",")))
  cat(sprintf("  Curtis Sliwa:   %s votes\n", format(sum(ed_data$Sliwa, na.rm=TRUE), big.mark=",")))
  cat(sprintf("  Eric Adams:     %s votes\n", format(sum(ed_data$Eric_Adams, na.rm=TRUE), big.mark=",")))

  cat("\n📊 WINNER BREAKDOWN:\n")
  winner_summary <- ed_data %>%
    count(winner) %>%
    arrange(desc(n))
  print(winner_summary)

  # Save
  write_csv(ed_data, "data/intermediate/ad34_mayor_2025_with_cuomo.csv")
  cat("\n✅ Saved: data/intermediate/ad34_mayor_2025_with_cuomo.csv\n")
}
