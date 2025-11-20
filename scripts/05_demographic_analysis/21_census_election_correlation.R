library(sf)
library(tidycensus)
library(tidyverse)
library(scales)
library(readxl)
library(tigris)

setwd("~/Mayoral Results AD34")

# Set Census API key
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")

cat("========================================\n")
cat("Census-Election Correlation Analysis\n")
cat("AD34 - 2025 Democratic Mayoral Primary\n")
cat("========================================\n\n")

# Load registration data
cat("Loading voter registration data...\n")
registration <- read_excel("queensed_nov25.xlsx", skip = 4) %>%
  filter(!is.na(STATUS) & !is.na(`ELECTION DIST`)) %>%
  mutate(
    ed_code = str_extract(`ELECTION DIST`, "\\d{5}"),
    AD = as.numeric(substr(ed_code, 1, 2)),
    ED = as.numeric(substr(ed_code, 3, 5)),
    ED_ID = sprintf("%02d%03d", AD, ED)
  ) %>%
  filter(AD == 34 & STATUS == "Active") %>%
  mutate(registered_voters = TOTAL)

# Load election results
cat("Loading election results...\n")
Precinct.results <- read.csv("data/raw/election/Precinct results.csv")

# Clean election results
results <- data.frame(Precinct.results) %>%
  Filter(function(x) !all(is.na(x)), .) %>%
  .[-1,]

results$X <- sprintf("34%03d", as.numeric(sub("ED ", "", results$X)))

vote_columns <- c("Zohran.Kwame.Mamdani", "Curtis.A..Sliwa", "Irene.Estrada",
                  "Zohran.Kwame.Mamdani.1", "Curtis.A..Sliwa.1","Eric.L..Adams",
                  "Joseph.Hernandez", "Andrew.M..Cuomo", "Jim.Walden", "WRITE.IN")

results <- results %>%
  mutate(across(all_of(vote_columns), ~ as.numeric(.))) %>%
  mutate(
    Zohran.Total = as.numeric(Zohran.Kwame.Mamdani) + as.numeric(Zohran.Kwame.Mamdani.1),
    total_votes = rowSums(across(all_of(vote_columns)), na.rm = TRUE),
    pct_Zohran = (Zohran.Total / total_votes) * 100
  ) %>%
  left_join(registration %>% select(ED_ID, registered_voters),
            by = c("X" = "ED_ID")) %>%
  mutate(turnout_pct = (total_votes / registered_voters) * 100)

# Load AD34 boundary and filter
cat("Loading AD34 boundary...\n")
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")
ad34_mask <- st_union(st_transform(ad34, 3857)) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Filter function for tracts/block groups to AD34
filter_to_ad34 <- function(sf_obj) {
  centroids <- suppressWarnings(st_centroid(sf_obj))
  ad34_local <- st_transform(ad34_mask, st_crs(sf_obj))
  sf_obj$in_ad34 <- st_within(centroids, ad34_local, sparse = FALSE)[,1]
  sf_obj %>% filter(in_ad34)
}

# Load Election District boundaries
cat("Loading Election District boundaries...\n")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
ED_shapefile <- st_read(shp_path, quiet = TRUE) %>%
  filter(County == "Queens") %>%
  filter(str_sub(Election_D, 1, 2) == "34") %>%
  mutate(Election_D = as.character(Election_D))

# Join results to ED shapefile
ED_data <- ED_shapefile %>%
  left_join(results, by = c("Election_D" = "X")) %>%
  filter(!is.na(total_votes) & total_votes > 0) %>%
  st_transform(3857)

cat("\n==== PULLING CENSUS DATA ====\n\n")

# Pull key demographic variables at TRACT level (better spatial alignment)
cat("Pulling demographics (race/ethnicity)...\n")
demographics <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = c(
    total_pop = "B03002_001",
    hispanic = "B03002_012",
    white_nh = "B03002_003",
    black_nh = "B03002_004",
    asian_nh = "B03002_006"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  filter_to_ad34()

cat("Pulling income data...\n")
income <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = c(
    median_income = "B19013_001"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  filter_to_ad34()

cat("Pulling nativity data...\n")
nativity <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = c(
    total_pop_nat = "B05002_001",
    foreign_born = "B05002_013"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  filter_to_ad34()

cat("Pulling education data...\n")
education <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = c(
    total_25plus = "B15003_001",
    bachelors = "B15003_022",
    masters = "B15003_023",
    professional = "B15003_024",
    doctorate = "B15003_025"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  filter_to_ad34()

cat("Pulling age data...\n")
age <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Queens",
  variables = c(
    median_age = "B01002_001"
  ),
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  st_transform(3857) %>%
  filter_to_ad34()

cat("\n==== CALCULATING DEMOGRAPHIC PERCENTAGES ====\n\n")

# Calculate percentages
demographics <- demographics %>%
  mutate(
    pct_hispanic = (hispanicE / total_popE) * 100,
    pct_white = (white_nhE / total_popE) * 100,
    pct_black = (black_nhE / total_popE) * 100,
    pct_asian = (asian_nhE / total_popE) * 100
  )

nativity <- nativity %>%
  mutate(pct_foreign_born = (foreign_bornE / total_pop_natE) * 100)

education <- education %>%
  mutate(
    college_plus = bachelorsE + mastersE + professionalE + doctorateE,
    pct_college = (college_plus / total_25plusE) * 100
  )

cat("\n==== SPATIAL JOIN: Allocating Census Data to EDs ====\n\n")

# Spatial join: For each ED, calculate weighted average of overlapping block groups
# Weight by area of intersection

# Function to aggregate census data to EDs
aggregate_to_eds <- function(census_sf, value_col) {
  cat(sprintf("  Aggregating %s...\n", value_col))

  # Calculate intersection
  intersection <- st_intersection(
    ED_data %>% select(Election_D, geometry),
    census_sf %>% select(GEOID, all_of(value_col), geometry)
  )

  # Calculate area of each intersection piece
  intersection$area <- as.numeric(st_area(intersection))

  # For each ED, calculate weighted mean
  result <- intersection %>%
    st_drop_geometry() %>%
    group_by(Election_D) %>%
    summarise(
      !!value_col := weighted.mean(.data[[value_col]], area, na.rm = TRUE)
    )

  return(result)
}

# Aggregate each demographic variable to ED level
ed_hispanic <- aggregate_to_eds(demographics, "pct_hispanic")
ed_white <- aggregate_to_eds(demographics, "pct_white")
ed_black <- aggregate_to_eds(demographics, "pct_black")
ed_asian <- aggregate_to_eds(demographics, "pct_asian")
ed_foreign <- aggregate_to_eds(nativity, "pct_foreign_born")
ed_college <- aggregate_to_eds(education, "pct_college")
ed_income <- aggregate_to_eds(income, "median_incomeE")
ed_age <- aggregate_to_eds(age, "median_ageE")

# Join all demographics to ED election data
cat("\n==== CREATING COMBINED DATASET ====\n\n")

ED_analysis <- ED_data %>%
  st_drop_geometry() %>%
  left_join(ed_hispanic, by = "Election_D") %>%
  left_join(ed_white, by = "Election_D") %>%
  left_join(ed_black, by = "Election_D") %>%
  left_join(ed_asian, by = "Election_D") %>%
  left_join(ed_foreign, by = "Election_D") %>%
  left_join(ed_college, by = "Election_D") %>%
  left_join(ed_income, by = "Election_D") %>%
  left_join(ed_age, by = "Election_D") %>%
  filter(!is.na(pct_Zohran) & !is.na(turnout_pct))

cat(sprintf("Complete dataset: %d EDs with full data\n\n", nrow(ED_analysis)))

cat("========================================\n")
cat("CORRELATION ANALYSIS\n")
cat("========================================\n\n")

# Check data availability
cat("Data availability check:\n")
cat(sprintf("  Hispanic: %d complete\n", sum(!is.na(ED_analysis$pct_hispanic))))
cat(sprintf("  White: %d complete\n", sum(!is.na(ED_analysis$pct_white))))
cat(sprintf("  Foreign Born: %d complete\n", sum(!is.na(ED_analysis$pct_foreign_born))))
cat(sprintf("  College: %d complete\n", sum(!is.na(ED_analysis$pct_college))))
cat(sprintf("  Income: %d complete\n", sum(!is.na(ED_analysis$median_incomeE))))
cat("\n")

# Safe correlation function
safe_cor <- function(x, y) {
  if (sum(!is.na(x) & !is.na(y)) < 3) return(NA_real_)
  cor(x, y, use = "complete.obs")
}

# Calculate correlations
correlations <- tibble(
  Variable = c("% Hispanic", "% White (Non-Hispanic)", "% Black (Non-Hispanic)",
               "% Asian (Non-Hispanic)", "% Foreign Born", "% College Degree+",
               "Median Income", "Median Age"),
  `Support Correlation` = c(
    safe_cor(ED_analysis$pct_hispanic, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$pct_white, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$pct_black, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$pct_asian, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$pct_foreign_born, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$pct_college, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$median_incomeE, ED_analysis$pct_Zohran),
    safe_cor(ED_analysis$median_ageE, ED_analysis$pct_Zohran)
  ),
  `Turnout Correlation` = c(
    safe_cor(ED_analysis$pct_hispanic, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$pct_white, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$pct_black, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$pct_asian, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$pct_foreign_born, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$pct_college, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$median_incomeE, ED_analysis$turnout_pct),
    safe_cor(ED_analysis$median_ageE, ED_analysis$turnout_pct)
  )
) %>%
  mutate(
    `Support Direction` = case_when(
      `Support Correlation` > 0.3 ~ "Strong Positive",
      `Support Correlation` > 0.1 ~ "Positive",
      `Support Correlation` > -0.1 ~ "Neutral",
      `Support Correlation` > -0.3 ~ "Negative",
      TRUE ~ "Strong Negative"
    ),
    `Turnout Direction` = case_when(
      `Turnout Correlation` > 0.3 ~ "Strong Positive",
      `Turnout Correlation` > 0.1 ~ "Positive",
      `Turnout Correlation` > -0.1 ~ "Neutral",
      `Turnout Correlation` > -0.3 ~ "Negative",
      TRUE ~ "Strong Negative"
    )
  )

print(correlations)

cat("\n========================================\n")
cat("DEMOGRAPHIC PROFILE BY CLASSIFICATION\n")
cat("========================================\n\n")

# Calculate median splits
median_support <- median(ED_analysis$pct_Zohran, na.rm = TRUE)
median_turnout <- median(ED_analysis$turnout_pct, na.rm = TRUE)

ED_analysis <- ED_analysis %>%
  mutate(
    classification = case_when(
      pct_Zohran >= median_support & turnout_pct >= median_turnout ~ "High Support, High Turnout",
      pct_Zohran >= median_support & turnout_pct < median_turnout ~ "High Support, Low Turnout",
      pct_Zohran < median_support & turnout_pct >= median_turnout ~ "Low Support, High Turnout",
      pct_Zohran < median_support & turnout_pct < median_turnout ~ "Low Support, Low Turnout"
    )
  )

demographic_profiles <- ED_analysis %>%
  group_by(classification) %>%
  summarise(
    n_EDs = n(),
    avg_hispanic = mean(pct_hispanic, na.rm = TRUE),
    avg_white = mean(pct_white, na.rm = TRUE),
    avg_black = mean(pct_black, na.rm = TRUE),
    avg_asian = mean(pct_asian, na.rm = TRUE),
    avg_foreign_born = mean(pct_foreign_born, na.rm = TRUE),
    avg_college = mean(pct_college, na.rm = TRUE),
    avg_income = mean(median_incomeE, na.rm = TRUE),
    avg_age = mean(median_ageE, na.rm = TRUE),
    total_registered = sum(registered_voters, na.rm = TRUE),
    total_votes = sum(total_votes, na.rm = TRUE),
    zohran_votes = sum(Zohran.Total, na.rm = TRUE)
  )

print(demographic_profiles)

cat("\n========================================\n")
cat("SAVING ANALYSIS\n")
cat("========================================\n\n")

# Save detailed data
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(correlations, "outputs/tables/census_election_correlations.csv", row.names = FALSE)
write.csv(demographic_profiles, "outputs/tables/demographic_profiles_by_category.csv", row.names = FALSE)
write.csv(ED_analysis, "outputs/tables/ed_level_census_election_data.csv", row.names = FALSE)

cat("Saved:\n")
cat("  - outputs/tables/census_election_correlations.csv\n")
cat("  - outputs/tables/demographic_profiles_by_category.csv\n")
cat("  - outputs/tables/ed_level_census_election_data.csv\n")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")
