# ---------------------------------------------------------
#  ACS Variable Explorer
# ---------------------------------------------------------

     # for interactive tables (optional)

library(dplyr)  # reload it last to take precedence
library(tidycensus)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)   

# 1. Parameters
year <- 2023
survey <- "acs5"

# 2. Load variable metadata
v <- load_variables(year, survey, cache = TRUE)

# 3. Parse and clean
v_clean <- v %>%
  dplyr::mutate(
    # Split "!!" hierarchy into up to 6 columns
    split = str_split(label, "!!"),
    split = lapply(split, function(x) str_replace_all(x, ":", "")),
    top_level = sapply(split, `[`, 1),
    level1    = sapply(split, `[`, 2),
    level2    = sapply(split, `[`, 3),
    level3    = sapply(split, `[`, 4),
    level4    = sapply(split, `[`, 5),
    level5    = sapply(split, `[`, 6),
    full_label = str_replace_all(label, "!!", " → "),
    short_label = str_replace_all(full_label, "Estimate → Total: → ", "")
  ) %>%
  select(name, concept, short_label, full_label, everything())

# 4. Define search helper
search_vars <- function(keyword, data = v_clean) {
  data %>%
    dplyr::filter(str_detect(tolower(concept), tolower(keyword)) |
                    str_detect(tolower(short_label), tolower(keyword))) %>%
    dplyr::select(name, concept, short_label)
}

# 5. Example searches
search_vars("foreign-born")
search_vars("ancestry")
search_vars("poverty")

# 6. Optional: Interactive browser
datatable(
  v_clean %>% select(name, concept, short_label, full_label),
  options = list(pageLength = 25, searchHighlight = TRUE),
  filter = "top",
  caption = paste("ACS", year, survey, "Variable Explorer")
)
