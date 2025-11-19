# ============================================================
# Clean AD34 Turnout Data and Create Improved Map
# ============================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(scales)

setwd("~/Mayoral Results AD34")

cat("\n🔧 Cleaning and mapping AD34 turnout data\n")
cat("==========================================\n\n")

# ============================================================
# LOAD AND CLEAN TURNOUT DATA
# ============================================================

cat("📥 Loading turnout data...\n")

turnout_raw <- read_csv("data/intermediate/ad34_turnout_2025.csv", show_col_types = FALSE)

cat("  Raw data:", nrow(turnout_raw), "rows\n")

# Remove obvious outliers/summary rows
# Real EDs typically have 100-1000 votes
# Values over 10,000 are likely summary/total rows

turnout_clean <- turnout_raw %>%
  filter(total_votes < 10000) %>%  # Remove summary rows
  filter(ED_num <= 100) %>%         # Real ED numbers are typically 1-100
  group_by(ED_num) %>%
  slice_max(total_votes, n = 1) %>%  # If duplicate ED, keep higher count
  ungroup() %>%
  mutate(ED = paste0("34", str_pad(ED_num, 3, pad = "0")))

cat("  Cleaned data:", nrow(turnout_clean), "Election Districts\n")
cat("  Total votes:", comma(sum(turnout_clean$total_votes)), "\n\n")

# ============================================================
# LOAD SHAPEFILES
# ============================================================

cat("🗺️  Loading shapefiles...\n")

ed_shp <- st_read(
  "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp",
  quiet = TRUE
)

ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34"))

cat("  Found", nrow(ad34_shp), "AD34 shapes\n\n")

# ============================================================
# JOIN AND CREATE MAP
# ============================================================

cat("🔗 Joining data...\n")

ad34_map <- ad34_shp %>%
  left_join(turnout_clean, by = c("Election_D" = "ED"))

matched <- sum(!is.na(ad34_map$total_votes))
cat("  Matched:", matched, "out of", nrow(ad34_shp), "shapes\n\n")

# Transform to NYC projection
ad34_map <- st_transform(ad34_map, 3857)

# ============================================================
# CREATE IMPROVED MAP
# ============================================================

cat("📊 Creating turnout map...\n")

turnout_map <- ggplot(ad34_map) +
  geom_sf(aes(fill = total_votes), color = "white", size = 0.2) +
  scale_fill_viridis(
    name = "Total Votes",
    option = "plasma",
    na.value = "grey90",
    labels = comma,
    breaks = pretty_breaks(n = 6)
  ) +
  labs(
    title = "Voter Turnout by Election District - Assembly District 34",
    subtitle = "November 4, 2025 General Election (Unofficial Results)",
    caption = "Source: NYC Board of Elections ENR System | AD34: Jackson Heights, Corona, East Elmhurst, Astoria"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "grey50", margin = margin(t = 15)),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("outputs/maps/AD34_turnout_map_2025_clean.png",
       turnout_map,
       width = 14, height = 11, dpi = 300, bg = "white")

cat("  ✓ Saved: outputs/maps/AD34_turnout_map_2025_clean.png\n\n")

# ============================================================
# STATISTICS
# ============================================================

cat("📈 Turnout Statistics:\n")
cat(rep("=", 60), "\n")
cat("Total votes:", comma(sum(turnout_clean$total_votes)), "\n")
cat("Average per ED:", comma(round(mean(turnout_clean$total_votes))), "\n")
cat("Median per ED:", comma(median(turnout_clean$total_votes)), "\n")
cat("Min:", comma(min(turnout_clean$total_votes)), "\n")
cat("Max:", comma(max(turnout_clean$total_votes)), "\n")
cat("Standard deviation:", comma(round(sd(turnout_clean$total_votes))), "\n\n")

# Distribution
cat("Turnout distribution:\n")
quantiles <- quantile(turnout_clean$total_votes, probs = c(0.25, 0.5, 0.75))
cat("  25th percentile:", comma(quantiles[1]), "\n")
cat("  50th percentile (median):", comma(quantiles[2]), "\n")
cat("  75th percentile:", comma(quantiles[3]), "\n\n")

# Top 10 EDs
cat("Top 10 EDs by turnout:\n")
top10 <- turnout_clean %>%
  arrange(desc(total_votes)) %>%
  head(10)

for(i in 1:nrow(top10)) {
  cat(sprintf("  %2d. ED %3d: %s votes\n",
              i, top10$ED_num[i], comma(top10$total_votes[i])))
}

# ============================================================
# SAVE CLEAN DATA
# ============================================================

write_csv(turnout_clean, "data/intermediate/ad34_turnout_2025_clean.csv")

cat("\n✅ Complete!\n")
cat("📂 Outputs:\n")
cat("  • Map: outputs/maps/AD34_turnout_map_2025_clean.png\n")
cat("  • Data: data/intermediate/ad34_turnout_2025_clean.csv\n\n")
