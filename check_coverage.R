library(sf)
library(tigris)
library(dplyr)

# Load AD34
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34") %>% st_transform(4326)

# Get bbox
bbox <- st_bbox(ad34)

cat("AD34 Bounding Box:\n")
cat(sprintf("West:  %.6f\n", bbox[1]))
cat(sprintf("South: %.6f\n", bbox[2]))
cat(sprintf("East:  %.6f\n", bbox[3]))
cat(sprintf("North: %.6f\n", bbox[4]))

# Check some known addresses
known_mosques <- data.frame(
  name = c("ICJH", "Jackson Heights Islamic Center", "Corona Mosque"),
  lat = c(40.760267, 40.746376, 40.747),
  lon = c(-73.876198, -73.894067, -73.867)
)

cat("\nKnown mosque locations vs AD34 bbox:\n")
for (i in 1:nrow(known_mosques)) {
  in_bbox <- known_mosques$lon[i] >= bbox[1] && 
             known_mosques$lon[i] <= bbox[3] &&
             known_mosques$lat[i] >= bbox[2] && 
             known_mosques$lat[i] <= bbox[4]
  cat(sprintf("%s: (%.6f, %.6f) - %s\n", 
              known_mosques$name[i],
              known_mosques$lon[i], 
              known_mosques$lat[i],
              ifelse(in_bbox, "IN BBOX", "OUTSIDE BBOX")))
}
