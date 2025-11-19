# ================================
# Queens AD34 Snapshot — ACS 2023 (5-year, DETAIL TABLES)
# ================================

# ================================
# Queens AD34 + AD36 Snapshot — ACS 2023 (5-year, DETAIL TABLES)
# ================================

library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6", install = FALSE)
vars <- load_variables(2023, "acs5", cache = TRUE)

TARGETS <- c("Assembly District 34", "Assembly District 36")

# -------------------------------
# Helper Function
# -------------------------------
pull_districts <- function(vars) {
  get_acs(
    geography = "state legislative district (lower chamber)",
    state     = "NY",
    variables = vars,
    year      = 2023,
    survey    = "acs5",
    output    = "wide"
  ) %>%
    filter(str_detect(NAME, paste(TARGETS, collapse = "|"))) %>%
    mutate(District = str_extract(NAME, "Assembly District \\d+"))
}

# -------------------------------
# 1. Population / Age / Gender
# -------------------------------
basic <- pull_districts(c(
  total_pop = "B01003_001E",
  male      = "B01001_002E",
  female    = "B01001_026E",
  median_age = "B01002_001E"
))

# -------------------------------
# 1b. Age Distribution (18–64 %)
# -------------------------------
age <- pull_districts(setNames(paste0("B01001_", sprintf("%03d", 1:49), "E"),
                               paste0("a", sprintf("%02d", 1:49))))
age <- age %>%
  mutate(
    age_total = a01,
    age18_64  = rowSums(across(c(a10:a18, a34:a42)), na.rm = TRUE),
    pct18_64  = 100 * age18_64 / age_total
  ) %>%
  select(NAME, District, pct18_64)

# -------------------------------
# 2. Race / Ethnicity
# -------------------------------
race <- pull_districts(c(
  tot_race = "B02001_001E",
  white    = "B02001_002E",
  black    = "B02001_003E",
  asian    = "B02001_005E"
))

hisp <- pull_districts(c(
  tot_hisp = "B03002_001E",
  hisp     = "B03002_012E"
))

# -------------------------------
# 3. Education (B15003)
# -------------------------------
edu <- pull_districts(setNames(paste0("B15003_", sprintf("%03d", 1:25), "E"),
                               paste0("v", sprintf("%02d", 1:25))))
edu <- edu %>%
  mutate(
    edu_total = v01,
    hs_plus   = rowSums(across(v17:v25), na.rm = TRUE),
    ba_plus   = rowSums(across(v22:v25), na.rm = TRUE)
  )

# -------------------------------
# 4. Language (C16001, C16002)
# -------------------------------
lang1 <- pull_districts(c(
  pop5plus      = "C16001_001E",
  english_only  = "C16001_002E"
))

lang2 <- pull_districts(c(
  pop5plus_c     = "C16002_001E",
  lep_spanish    = "C16002_004E",
  lep_indo_euro  = "C16002_006E",
  lep_asian_pac  = "C16002_008E",
  lep_other      = "C16002_010E"
)) %>%
  mutate(limited_english = rowSums(across(c(lep_spanish, lep_indo_euro, lep_asian_pac, lep_other)), na.rm = TRUE))

# -------------------------------
# 5. Nativity (B05002)
# -------------------------------
nat <- pull_districts(c(
  nat_total     = "B05002_001E",
  foreign_born  = "B05002_013E"
))

# -------------------------------
# 6. Housing (B25002, B25003)
# -------------------------------
housing <- pull_districts(c(
  hu_total  = "B25002_001E",
  vac_total = "B25002_003E"
))

tenure <- pull_districts(c(
  occ_units    = "B25003_001E",
  owner_units  = "B25003_002E",
  renter_units = "B25003_003E"
))

# -------------------------------
# 7. Income (B19001)
# -------------------------------
income <- pull_districts(setNames(paste0("B19001_", sprintf("%03d", 1:17), "E"),
                                  paste0("i", sprintf("%02d", 1:17))))
income <- income %>%
  mutate(
    hh_total    = i01,
    under50k    = rowSums(across(i02:i10), na.rm = TRUE),
    p50_100k    = rowSums(across(i11:i13), na.rm = TRUE),
    p100_200k   = rowSums(across(i14:i16), na.rm = TRUE),
    over200k    = i17
  )
# -------------------------------
# 8. Commuting (B08006)
# -------------------------------
commute <- pull_districts(c(
  total               = "B08006_001E",
  drove_alone         = "B08006_003E",
  carpooled           = "B08006_004E",
  bus                 = "B08006_009E",
  subway              = "B08006_010E",
  commuter_rail       = "B08006_011E",
  light_rail          = "B08006_012E",
  ferryboat           = "B08006_013E",
  taxicab             = "B08006_014E",
  motorcycle          = "B08006_015E",
  bicycle             = "B08006_016E",
  walked              = "B08006_017E",
  other_means         = "B08006_018E",
  worked_home         = "B08006_019E"
)) %>%
  mutate(
    public_transport = bus + subway + commuter_rail + light_rail + ferryboat,
    drove_alone_pct      = 100 * drove_alone / total,
    carpooled_pct        = 100 * carpooled / total,
    public_transport_pct = 100 * public_transport / total,
    taxicab_pct          = 100 * taxicab / total,
    motorcycle_pct       = 100 * motorcycle / total,
    bicycle_pct          = 100 * bicycle / total,
    walked_pct           = 100 * walked / total,
    other_means_pct      = 100 * other_means / total,
    worked_home_pct      = 100 * worked_home / total
  )


commute <- commute %>%
  mutate(
    drove_alone_pct      = 100 * drove_alone / total,
    carpooled_pct        = 100 * carpooled / total,
    public_transport_pct = 100 * public_transport / total,
    taxicab_pct          = 100 * taxicab / total,
    motorcycle_pct       = 100 * motorcycle / total,
    bicycle_pct          = 100 * bicycle / total,
    walked_pct           = 100 * walked / total,
    other_means_pct      = 100 * other_means / total,
    worked_home_pct      = 100 * worked_home / total
  )

# --- robust most common mode calculation ---
commute_modes <- commute %>%
  select(NAME, District, drove_alone_pct, carpooled_pct, public_transport_pct,
         taxicab_pct, motorcycle_pct, bicycle_pct, walked_pct,
         other_means_pct, worked_home_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "mode",
    values_to = "pct"
  ) %>%
  group_by(NAME, District) %>%
  filter(!is.na(pct)) %>%
  mutate(pct = round(pct, 2)) %>%  # round to consistent decimals to avoid float ties
  filter(pct == max(pct, na.rm = TRUE)) %>%
  summarise(
    Most_Common_Mode = if (n() > 1) paste("Tie:", paste(unique(str_remove(mode, "_pct")), collapse = ", ")) 
    else str_remove(mode, "_pct"),
    Most_Common_Pct  = max(pct, na.rm = TRUE),
    .groups = "drop"
  )


# --- add back to commuting summary ---
commute <- commute %>%
  left_join(commute_modes, by = c("NAME", "District"))

# -------------------------------
# Compute derived metrics per district
# -------------------------------
compare <- basic %>%
  filter(District == "Assembly District 34") %>%
  select(NAME, District, total_pop, male, female, median_age) %>%
  left_join(age, by = c("NAME", "District")) %>%
  left_join(race %>% select(NAME, District, tot_race, white, black, asian), by = c("NAME", "District")) %>%
  left_join(hisp %>% select(NAME, District, tot_hisp, hisp), by = c("NAME", "District")) %>%
  left_join(edu %>% select(NAME, District, edu_total, hs_plus, ba_plus), by = c("NAME", "District")) %>%
  left_join(lang1 %>% select(NAME, District, pop5plus, english_only), by = c("NAME", "District")) %>%
  left_join(lang2 %>% select(NAME, District, pop5plus_c, limited_english), by = c("NAME", "District")) %>%
  left_join(nat %>% select(NAME, District, nat_total, foreign_born), by = c("NAME", "District")) %>%
  left_join(housing %>% select(NAME, District, hu_total, vac_total), by = c("NAME", "District")) %>%
  left_join(tenure %>% select(NAME, District, occ_units, owner_units, renter_units), by = c("NAME", "District")) %>%
  left_join(income %>% select(NAME, District, hh_total, under50k, p50_100k, p100_200k, over200k), by = c("NAME", "District")) %>%
  left_join(commute %>% select(NAME, District, total, drove_alone_pct, carpooled_pct,  public_transport_pct, walked_pct, worked_home_pct, Most_Common_Mode, Most_Common_Pct),by = c("NAME", "District")) %>%
  mutate(
    male_pct = 100 * male / (male + female),
    female_pct = 100 * female / (male + female),
    white_pct = 100 * white / tot_race,
    black_pct = 100 * black / tot_race,
    asian_pct = 100 * asian / tot_race,
    hispanic_pct = 100 * hisp / tot_hisp,
    hs_plus_pct = 100 * hs_plus / edu_total,
    bach_plus_pct = 100 * ba_plus / edu_total,
    english_only_pct = 100 * english_only / pop5plus,
    other_lang_pct = 100 * (pop5plus - english_only) / pop5plus,
    ltd_english_pct = 100 * limited_english / pop5plus_c,
    foreign_born_pct = 100 * foreign_born / nat_total,
    owner_occ_pct = 100 * owner_units / occ_units,
    renter_occ_pct = 100 * renter_units / occ_units,
    vacancy_rate = 100 * vac_total / hu_total,
    under50k_pct = 100 * under50k / hh_total,
    p50_100k_pct = 100 * p50_100k / hh_total,
    p100_200k_pct = 100 * p100_200k / hh_total,
    over200k_pct = 100 * over200k / hh_total
  )

# -------------------------------
# Output clean comparison table
# -------------------------------
# -------------------------------
# Output comparison table (pivoted)
# -------------------------------
# -------------------------------
# Output comparison table (pivoted)
# -------------------------------
snapshot <- compare %>%
  select(
    District,
    Population = total_pop,
    `Median Age` = median_age,
    `% Age 18–64` = pct18_64,
    `Male %` = male_pct,
    `Female %` = female_pct,
    `White %` = white_pct,
    `Black %` = black_pct,
    `Asian %` = asian_pct,
    `Hispanic %` = hispanic_pct,
    `HS+ %` = hs_plus_pct,
    `BA+ %` = bach_plus_pct,
    `English Only %` = english_only_pct,
    `Other Lang %` = other_lang_pct,
    `Limited English %` = ltd_english_pct,
    `Foreign Born %` = foreign_born_pct,
    `Income <50k %` = under50k_pct,
    `50–100k %` = p50_100k_pct,
    `100–200k %` = p100_200k_pct,
    `>200k %` = over200k_pct,
    `Drive Alone %`      = drove_alone_pct,
    `Carpooled %`        = carpooled_pct,
    `Public Transit %`   = public_transport_pct,
    `Walked %`           = walked_pct,
    `Worked from Home %` = worked_home_pct,
    `Most Common Commute` = Most_Common_Mode,
    `Housing Units` = hu_total,
    `Owner %` = owner_occ_pct,
    `Renter %` = renter_occ_pct,
    `Vacancy %` = vacancy_rate
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) %>%
  mutate(across(-District, as.character)) %>%   # <-- NEW LINE HERE
  pivot_longer(
    -District,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = District,
    values_from = Value
  ) %>%
  arrange(Variable)

# -------------------------------
# Save & view
# -------------------------------
print(snapshot, n = Inf)
write_csv(snapshot, "outputs/tables/Queens_AD34_AD36_Pivoted_Comparison_2023.csv")
cat("\n✅ Saved: outputs/tables/Queens_AD34_AD36_Pivoted_Comparison_2023.csv\n")
