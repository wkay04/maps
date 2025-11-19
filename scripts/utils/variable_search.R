library(tidycensus)
library(dplyr)

# Load ACS 5-year variables (2022)
vars <- load_variables(2022, "acs5", cache = TRUE)

# Keep only concepts compatible with block groups
vars_bg <- vars %>%
  filter(geography %in% c("block group", "bg", "bg5"))

# Search for concepts containing keywords
keywords <- c("poverty", "percent in poverty")

# Create a single grep pattern
pattern <- paste("poverty status", collapse = "|")

# Filter concepts matching any of the keywords
national_origin_concepts <- vars_bg %>%
  filter(grepl("poverty status", concept, ignore.case = TRUE)) %>%
  arrange(concept)

# Inspect
national_origin_concepts %>% dplyr::select(name, label, concept) %>% head(20)


# Optional: save to CSV
write.csv(national_origin_concepts, "ACS_blockgroup_national_origin.csv", row.names = FALSE)
