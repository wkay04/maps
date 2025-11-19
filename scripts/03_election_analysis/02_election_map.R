
shp_path <- "data/raw/election/NYS_Elections_Districts_and_Polling_Locations_-4537597403999295499"
url <- "https://enr.boenyc.gov/CD27305AD344.html" 

election.map <- function(url, shp_path)
{
  library(sf)
  library(stringr)
  library(ggplot2)
  library(sf)
  library(viridis)
  library (ggspatial)
  library(prettymapr)
  library(scales)
  library(dplyr)  
  library(rvest)

setwd("~/Mayoral Results AD34")

webpage <- read_html(url)
all_tables <- webpage %>% html_table(fill = TRUE)

question <- all_tables[3]

question<- as.data.frame(question)
question$X1 <- sprintf("34%03d", as.numeric(sub("ED ", "", question$X1)))

ED_shapefile <- st_read(shp_path)
ED_shapefile <- ED_shapefile %>% filter(County == "Queens")
ED_shapefile <-ED_shapefile %>% 
  filter(str_sub(Election_D, 1, 2) == "34")

question <- question %>%
  mutate(
    result = case_when(
      X6 > X4 ~ "NO",
      X6 < X4 ~ "YES",
      X6 == X4 ~ "TIE"
    )
  )

ED_shapefile$Election_D <- as.character(ED_shapefile$Election_D )
ED_shapefile <- ED_shapefile %>%
  left_join(question, by = c("Election_D" = "X1"))

ggplot() +
  annotation_map_tile(type = "osm", zoom = 17, "toner-lite") +
  geom_sf(
    data = ED_shapefile,
    aes(fill = result),
    color = "white",
    size = 0.4,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "YES" = "#F94144",  # red
      "NO" = "#43AA8B"  # green
      ),
    name = "Winner"
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "2025 Result by ED",
    subtitle = "Assembly District 34",
    caption = "Source:BOE Unoffical"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

}






