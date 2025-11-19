library(sf)
library(stringr)
library(ggplot2)
library(sf)
library(viridis)
library (ggspatial)
library(prettymapr)
library(scales)
library(dplyr)  

setwd("~/Mayoral Results AD34")

Precinct.results <- read.csv("data/raw/election/Precinct results.csv")
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
sldl <- state_legislative_districts(state = "NY", house = "lower", year = 2025)

ad34 <- sldl %>% dplyr::filter(NAMELSAD == "Assembly District 34")
ad36 <- sldl %>% dplyr::filter(NAMELSAD == "Assembly District 34")

bg   <- block_groups(state = "NY", county = "Queens", year = 2022, filter_by=st_as_sf(ad34))
tracts <- tracts(state = "NY", county = "Queens", year = 2022, filter_by=st_as_sf(ad34))

results <- data.frame(Precinct.results)
results <- Filter(function(x) !all(is.na(x)), results)
results <- results[-1,]
Precinct.results$Zohran.Total <- as.numeric(Precinct.results$Zohran.Kwame.Mamdani) + as.numeric(Precinct.results$Zohran.Kwame.Mamdani.1)
Precinct.results$Sliwa.Total <- as.numeric(Precinct.results$Curtis.A..Sliwa) + as.numeric(Precinct.results$Curtis.A..Sliwa.1)

Precinct.results <- Filter(function(x) !all(is.na(x)), results)
Precinct.results <- results[-1,]
Precinct.results$X <- sprintf("34%03d", as.numeric(sub("ED ", "", Precinct.results$X)))

vote_columns <- c("Zohran.Kwame.Mamdani", "Curtis.A..Sliwa", "Irene.Estrada", "Zohran.Kwame.Mamdani.1", "Curtis.A..Sliwa.1","Eric.L..Adams", "Joseph.Hernandez", "Andrew.M..Cuomo", "Jim.Walden", "WRITE.IN")


Precinct.results$X <- as.character(Precinct.results$X)

Precinct.results$Zohran.Total <- as.numeric(Precinct.results$Zohran.Kwame.Mamdani) + as.numeric(Precinct.results$Zohran.Kwame.Mamdani.1)
Precinct.results$Sliwa.Total <-as.numeric (Precinct.results$Curtis.A..Sliwa) + as.numeric( Precinct.results$Curtis.A..Sliwa.1)
Precinct.results$Cuomo.Total <-as.numeric(Precinct.results$Andrew.M..Cuomo)

Precinct.results <- Precinct.results %>%
  mutate(across(all_of(vote_columns), ~ as.numeric(.)))


Precinct.results <- Precinct.results %>%
  mutate(total_votes = rowSums(across(all_of(vote_columns)), na.rm= TRUE))

Precinct.results <- Precinct.results %>%
  mutate(
    pct_Zohran = Zohran.Total / total_votes,
    pct_Zohran_100 = pct_Zohran   * 100
  )



vote_cols <- c("Zohran.Total", "Sliwa.Total", "Andrew.M..Cuomo")

Precinct.results <- Precinct.results %>%
  mutate(
    max_votes = pmax(!!!syms(vote_cols), na.rm = TRUE),
    winner_Mamdani = if_else(Zohran.Total == max_votes, 1L, 0L)
  ) %>%
  dplyr::select(-max_votes)

Precinct.results <- Precinct.results %>%
  mutate(
    max_votes = pmax(!!!syms(vote_cols), na.rm = TRUE),
    winner_Sliwa = if_else(Sliwa.Total == max_votes, 1L, 0L)
  ) %>%
  dplyr::select(-max_votes)


Precinct.results <- Precinct.results %>%
  mutate(
    max_votes = pmax(!!!syms(vote_cols), na.rm = TRUE),
    winner_Cuomo = if_else(Andrew.M..Cuomo == max_votes, 1L, 0L)
  ) %>%
  dplyr::select(-max_votes)



Precinct.results <- Precinct.results %>%
  mutate(
    winner = case_when(
      winner_Mamdani == 1 ~ "Mamdani",
      winner_Sliwa == 1 ~ "Sliwa",
      winner_Cuomo == 1 ~ "Cuomo",
      TRUE ~ "No Winner"  # handles ties or missing
    )
  )


ED_shapefile <- st_read(shp_path)
ED_shapefile <- ED_shapefile %>% filter(County == "Queens")
ED_shapefile <-ED_shapefile %>% 
  filter(str_sub(Election_D, 1, 2) == "34")

ED_shapefile$Election_D <- as.character(ED_shapefile$Election_D )
Precinct.results$X <- as.character(Precinct.results$X )

ED_shapefile <- ED_shapefile %>%
  left_join(Precinct.results, by = c("Election_D" = "X"))




ggplot() +
  annotation_map_tile(type = "osm", zoom = 17, "toner-lite") +
  geom_sf(
    data = ED_shapefile,
    aes(fill = winner),
    color = "white",
    size = 0.4,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Mamdani" = "#F94144",  # red
      "Sliwa" = "#43AA8B",  # green
      "Cuomo" = "#4928EB",  # blue
      "No Winner" = "grey80"
    ),
    name = "Winner"
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Winning Candidate by Election District",
    subtitle = "Assembly District 34",
    caption = "Source: Election Data"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

