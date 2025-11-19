library(tidyverse)
library(sf)

setwd("~/Mayoral Results AD34")

# Load shapefile
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499/Election_Districts.shp"
ed_shp <- st_read(shp_path, quiet = TRUE)
ad34_shp <- ed_shp %>%
  filter(County == "Queens", str_starts(Election_D, "34")) %>%
  mutate(
    ED_num = as.numeric(str_extract(Election_D, "\\d+$")),
    ED = paste0("34", str_pad(ED_num, 3, pad = "0"))
  )
cat("Shapefile ED values (first 10):\n")
print(head(ad34_shp %>% st_drop_geometry() %>% select(Election_D, ED_num, ED), 10))

# Load candidate data
mayor_data <- readRDS("data/intermediate/ad34_mayor_parsed.rds")
ed_data <- mayor_data$ed_data
data_rows <- ed_data[4:nrow(ed_data),]
candidate_data <- tibble(
  ED_raw = data_rows[[1]],
  Zohran_Mamdani = as.numeric(data_rows[[4]])
) %>%
  filter(!is.na(ED_raw), str_detect(ED_raw, "^ED")) %>%
  mutate(
    ED_num = as.numeric(str_extract(ED_raw, "\\d+")),
    ED = paste0("34", str_pad(ED_num, 3, pad = "0"))
  )
cat("\nCandidate data ED values (first 10):\n")
print(head(candidate_data %>% select(ED_raw, ED_num, ED, Zohran_Mamdani), 10))

# Try merge
merged <- ad34_shp %>%
  left_join(candidate_data, by = "ED") %>%
  st_drop_geometry()

cat("\nMerged data (first 10):\n")
print(head(merged %>% select(Election_D, ED, ED_num.x, ED_num.y, Zohran_Mamdani), 10))

cat("\nNumber of rows with candidate data:\n")
print(sum(!is.na(merged$Zohran_Mamdani)))
