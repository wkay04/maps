# =====================================================
# 3. Join geometry to turnout and derive metrics safely
# =====================================================

# Ensure join key formats match
turn_2025 <- turn_2025 %>%
  mutate(ed_code = str_trim(ed_code),
         turnout = as.numeric(turnout),
         Zohran_Kwame_Mamdani = as.numeric(Zohran_Kwame_Mamdani),
         Curtis_A_Sliwa = as.numeric(Curtis_A_Sliwa),
         Irene_Estrada = as.numeric(Irene_Estrada),
         Eric_L_Adams = as.numeric(Eric_L_Adams),
         Joseph_Hernandez = as.numeric(Joseph_Hernandez),
         Andrew_M_Cuomo = as.numeric(Andrew_M_Cuomo),
         Jim_Walden = as.numeric(Jim_Walden),
         WRITE_IN = as.numeric(WRITE_IN))

# =====================================================
# Correctly rebuild ed_ad34 join keys
# =====================================================

ed_ad34 <- ed_shp %>%
  mutate(
    AD = "34",  # we know this shapefile is for Assembly 34 only
    ED = str_pad(as.character(Election_D), 3, pad = "0"),  # this is the real ED field
    ed_code = paste0(AD, "-", ED)
  ) %>%
  st_transform(2263)

# Quick check
cat("Unique ADs in shapefile:", unique(ed_ad34$AD), "\n")
cat("Sample ed_codes:\n")
print(head(ed_ad34$ed_code))

# Check join potential
cat("Intersection count:", length(intersect(turn_2025$ed_code, ed_ad34$ed_code)), "\n")

# =====================================================
# 4. Compute Zohran % and winner
# =====================================================

turn_geo <- turn_geo %>%
  mutate(
    turnout = as.numeric(turnout),
    zohran_pct = if_else(!is.na(turnout) & turnout > 0,
                         100 * Zohran_Kwame_Mamdani / turnout, NA_real_),
    winner = case_when(
      is.na(turnout) ~ "No data",
      Zohran_Kwame_Mamdani >= Curtis_A_Sliwa &
        Zohran_Kwame_Mamdani >= pmax(WRITE_IN, Irene_Estrada, Eric_L_Adams,
                                     Joseph_Hernandez, Andrew_M_Cuomo, Jim_Walden,
                                     na.rm = TRUE) ~ "Mamdani",
      Curtis_A_Sliwa >= Zohran_Kwame_Mamdani &
        Curtis_A_Sliwa >= pmax(WRITE_IN, Irene_Estrada, Eric_L_Adams,
                               Joseph_Hernandez, Andrew_M_Cuomo, Jim_Walden,
                               na.rm = TRUE) ~ "Sliwa",
      TRUE ~ "Other"
    )
  )

# =====================================================
# 5. Quick check before mapping
# =====================================================
print(turn_geo %>% st_drop_geometry() %>% select(ed_code, turnout, winner, zohran_pct) %>% head(10))

# =====================================================
# 6. Map — Total Turnout
# =====================================================
p_turnout <- ggplot(turn_geo) +
  geom_sf(aes(fill = turnout), color = "white", size = 0.15) +
  scale_fill_viridis(option = "turbo", direction = -1, na.value = "grey90",
                     name = "Total votes") +
  labs(
    title   = "Assembly District 34 — 2025 Turnout by Election District",
    caption = "Source: NYC Board of Elections"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.2),
        legend.position = "right")

ggsave("AD34_outputs/AD34_2025_Turnout_Map.png", p_turnout, width = 7, height = 6, dpi = 300)

# =====================================================
# 7. Map — Winner
# =====================================================
winner_levels <- c("Mamdani", "Sliwa", "Other", "No data")
turn_geo$winner <- factor(turn_geo$winner, levels = winner_levels)

p_winner <- ggplot(turn_geo) +
  geom_sf(aes(fill = winner), color = "white", size = 0.15) +
  scale_fill_viridis_d(option = "turbo", end = 0.9, name = "Winner", na.value = "grey90") +
  labs(
    title   = "Assembly District 34 — 2025 Winner by Election District",
    caption = "Source: NYC Board of Elections"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.2),
        legend.position = "right")

ggsave("AD34_outputs/AD34_2025_Winner_Map.png", p_winner, width = 7, height = 6, dpi = 300)

# =====================================================
# 8. Map — Zohran Vote Share %
# =====================================================
p_share <- ggplot(turn_geo) +
  geom_sf(aes(fill = zohran_pct), color = "white", size = 0.15) +
  scale_fill_viridis(option = "plasma", direction = -1, na.value = "grey90",
                     name = "% for Mamdani", labels = function(x) paste0(round(x), "%")) +
  labs(
    title   = "Assembly District 34 — 2025 Mamdani Vote Share by Election District",
    caption = "Source: NYC Board of Elections"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.2),
        legend.position = "right")

ggsave("AD34_outputs/AD34_2025_Mamdani_Share_Map.png", p_share, width = 7, height = 6, dpi = 300)

cat("✅ Maps saved:\n  • Turnout\n  • Winner\n  • Mamdani %\n")
