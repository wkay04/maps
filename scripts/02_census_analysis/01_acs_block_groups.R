library(readr)
library(sf)
library(dplyr)   
library(stringr)
library(ggplot2)
library(sf)
library(viridis)
library (ggspatial)
library(prettymapr)
library(scales)
library(censusr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(dplyr)
library(raster)
library(stringr)
library(readr)


setwd("~/Mayoral Results AD34")
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")
hisp <- get_acs(geography = "block group", state="NY",county= "QUEENS", variables = c(total_pop = "B03002_001", hispanic_pop = "B03002_012") , year = 2023, geometry = FALSE, survey = "acs5")
tracts <- tracts(state = "NY", county= "QUEENS", class = "sf",  year = 2023)

SLD <- read_delim("data/raw/crosswalks/tab20_sldl202420_tract20_st36.txt",
                  delim = "|", escape_double = FALSE, trim_ws = TRUE)


hisp  <- hisp  %>%
  # Reshape the data from long to wide format
  pivot_wider(
    id_cols = c("GEOID", "NAME"),
    names_from = "variable",
    values_from = "estimate"
  ) %>%
  # Calculate the percentage
  mutate(
    hispanic_pct = (hispanic_pop / total_pop) * 100
  ) %>%
  # Filter out block groups with zero population to avoid `NaN` results
  filter(total_pop > 0)

tracts$GEOID <-as.numeric(tracts$GEOID)
hisp$GEOID <- as.numeric(hisp$GEOID)
tracts$GEOID <- as.numeric(tracts$GEOID)
SLD$GEOID_TRACT_20 <-as.numeric(SLD$GEOID_TRACT_20)
NY_Join <- left_join(tracts, SLD ,by =c("GEOID" = "GEOID_TRACT_20"))
NY_Join <- left_join(NY_Join, hisp ,by = "GEOID")

NY_Join$GEOID20 <- as.numeric(NY_Join$GEOID20)
NY_Join$BLOCKCE20 <- as.numeric(NY_Join$BLOCKCE20)

NY_Join$GEOID <-substr(as.character(NY_Join$GEOID20), 1, 12)

NY_Join$GEOID <- as.numeric(NY_Join$GEOID)
NY_Join <- left_join(NY_Join, hisp ,by ="GEOID")

NY_Join <- filter(NY_Join, SLDLST=="034")
NY_SF <- st_as_sf(NY_Join, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326) 



  ggplot(NY_SF, aes(fill = hispanic_pct)) +
    annotation_map_tile(type = "osm", zoom = 17, "toner-lite")+
    geom_sf(data = NY_SF, size = 0.1, color = "black") +
  scale_fill_viridis_c(
    option = "magma",
    labels = scales::percent_format(scale = 1),
    na.value = "grey50" # Handle any NA values from zero-pop block groups
  ) +
  labs(
    title = "Percent Hispanic AD 34",
    fill = "Percent Hispanic"
  ) 


