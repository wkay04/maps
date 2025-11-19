# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based analysis project focused on NYC Assembly District 34 (Queens) using Census data (American Community Survey) and election results. The project generates demographic chartbooks, election maps, and comparative analyses using spatial data visualization.

## Project Structure

```
/Mayoral Results AD34/
├── data/
│   ├── raw/
│   │   ├── election/
│   │   │   ├── ed_level/      # Election District level CSVs from vote.nyc
│   │   │   ├── ad34/          # AD34-filtered election data (cleaned and filtered)
│   │   │   ├── unofficial/    # Unofficial results from ENR system
│   │   │   └── downloads/     # Other election downloads
│   │   ├── census/            # SLDL data, geodatabases, TIGER/Line files, ACS metadata
│   │   └── crosswalks/        # Tract-to-district mapping files
│   └── intermediate/          # Processed analysis outputs (CSVs)
├── scripts/
│   ├── 01_data_prep/         # Data loading and preparation (election data download scripts)
│   ├── 02_census_analysis/   # ACS demographic analysis scripts
│   ├── 03_election_analysis/ # Election results processing and mapping
│   ├── 04_visualization/     # Chart/map compilation and PDF generation
│   └── utils/                # Helper utilities for variable search
├── outputs/
│   ├── reports/              # Final PDF chartbooks
│   ├── maps/                 # Exported map images (PNG)
│   ├── charts/               # Chart images and backups
│   ├── tables/               # Summary comparison tables
│   └── AD34_outputs/         # Legacy output directory for chartbook assets
├── cache/                    # Tile cache, R session data
└── docs/                     # Documentation
```

## Required R Packages

The project heavily relies on these libraries:
- **Spatial**: `sf`, `tigris`, `ggspatial`
- **Census Data**: `tidycensus` (requires Census API key)
- **Data Manipulation**: `tidyverse`, `dplyr`, `stringr`, `readr`
- **Visualization**: `ggplot2`, `viridis`, `patchwork`, `gridExtra`
- **Mapping**: `prettymapr`, `scales`, `grid`, `png`

## Census API Configuration

**IMPORTANT**: The Census API key is hardcoded in multiple scripts:
```r
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")
```

When modifying scripts, preserve this API key setup or Census data pulls will fail.

## Key Scripts

### Data Preparation (`scripts/01_data_prep/`)

1. **01_download_nyc_election_data.R** - Downloads election results from vote.nyc in multiple formats
2. **02_download_ad34_enr_results.R** - Downloads unofficial results from NYC ENR system
3. **03_download_ad34_election_results.R** - **PRIMARY SCRIPT** - Downloads Election District level results and filters for AD34

**Note**: AD34 is in Queens (Jackson Heights, Corona, East Elmhurst, Astoria), not Manhattan. The ED Level CSV files from NYC BOE have a unique structure where column headers are mixed with data - the script handles this properly.

### Census Analysis (`scripts/02_census_analysis/`)

1. **01_acs_block_groups.R** - Hispanic population analysis at block group level
2. **02_chartbook_mobility_housing.R** - Main chartbook for transit, housing, and ancestry
3. **03_demographic_chartbook.R** - Comprehensive demographic profile using DP tables
4. **04_district_comparison.R** - Side-by-side comparison of AD34 vs AD36
5. **05_foreign_born_analysis.R** - Foreign-born population deep dive

### Election Analysis (`scripts/03_election_analysis/`)

1. **01_election_winners_map.R** - Primary election winner mapping by precinct
2. **02_election_map.R** - General election mapping utilities
3. **03-07_election_*.R** - Various election visualization scripts (legacy versions)

### Visualization (`scripts/04_visualization/`)

1. **01_compile_chartbook_pdf.R** - Compiles PNG maps into PDF chartbook

### Utilities (`scripts/utils/`)

1. **variable_search.R** - Search ACS variable codes
2. **variable_explorer.R** - Explore ACS table structures

## Working Directory Convention

All scripts set working directory to project root:
```r
setwd("~/Mayoral Results AD34")
```

Paths are then specified relative to this root directory.

## Key Architecture Patterns

### Geometry Filtering to AD34

Most scripts use a consistent pattern to filter tract/block group data to Assembly District 34:

```r
# Load AD34 boundary
sldl <- state_legislative_districts("NY", house = "lower", year = 2025)
ad34 <- sldl %>% filter(NAMELSAD == "Assembly District 34")

# Transform and create mask
ad34_mask <- st_union(st_transform(ad34, 3857)) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Filter function using centroids
filter_to_ad34 <- function(sf_obj) {
  centroids <- suppressWarnings(st_centroid(sf_obj))
  ad34_local <- st_transform(ad34_mask, st_crs(sf_obj))
  sf_obj$in_ad34 <- st_within(centroids, ad34_local, sparse = FALSE)[,1]
  sf_obj %>% filter(in_ad34)
}
```

**Alternative approach** (used in `03_demographic_chartbook.R`):
- Filters by area overlap threshold (≥25% of tract area in district)
- Uses `st_intersection()` to calculate overlap fractions

### ACS Data Pulling Pattern

Census data is consistently pulled using this helper pattern:

```r
pull_sf <- function(vars, label) {
  cat("\n🔍 Pulling:", label, "\n")
  acs <- get_acs(
    geography = "tract",  # or "block group"
    state = "NY",
    county = "Queens",
    variables = vars,
    year = 2023,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  )
  st_transform(st_make_valid(acs), 3857)
}
```

### Map Generation Pattern

Maps are created with OpenStreetMap tiles and consistent styling:

```r
make_map <- function(geodata, varname, title, discrete = FALSE) {
  ggplot() +
    annotation_map_tile(type = "osm", zoom = 14) +
    geom_sf(data = st_transform(geodata, 4326),
            aes(fill = .data[[varname]]),
            color = "white", linewidth = 0.08) +
    geom_sf(data = st_transform(ad34_mask, 4326),
            fill = NA, color = "red", linewidth = 0.6) +
    scale_fill_viridis_c(option = "plasma", name = NULL) +
    labs(title = title, caption = "ACS 2019–2023 5-year • NYC AD34") +
    theme_minimal() + ...
}
```

### Output Path Patterns

Scripts save outputs to organized directories:
- **PDFs**: `outputs/reports/`
- **Maps (PNG)**: `outputs/maps/` or `outputs/AD34_outputs/`
- **Tables (CSV)**: `outputs/tables/` or `data/intermediate/`

## Common ACS Variable Groups

The project uses several ACS table groups repeatedly:

### Demographics
- **B01003** - Total Population
- **B01001** - Sex by Age
- **B01002** - Median Age
- **DP05** - Demographic Profile

### Housing
- **B25064** - Median Gross Rent
- **B25003** - Tenure (Owner/Renter)
- **B25002** - Occupancy Status
- **DP04** - Housing Characteristics Profile

### Commuting (Transit)
- **B08301** - Means of Transportation to Work (detailed modal breakdown)
- **B08006** - Sex by Means of Transportation (summary)
- **DP03** - Economic Characteristics (includes commute summary)

### Nativity & Language
- **B05002** - Place of Birth
- **B05006** - Place of Birth by Region/Subregion (ancestry)
- **C16001** - Language Spoken at Home
- **C16002** - Limited English Proficiency
- **DP02** - Social Characteristics (includes nativity)

### Economics
- **B19001** - Household Income brackets
- **DP03_0062** - Median Household Income (DP table)
- **DP03_0128P** - Poverty Rate (DP table)

### Race/Ethnicity
- **B02001** - Race
- **B03002** - Hispanic/Latino Origin

## Election Data Processing

### Data Sources
- **Precinct results**: `data/raw/election/Precinct results.csv`
- **ED boundaries**: `data/raw/election/NYS_Elections_Districts_and_Polling_Locations_*/`

### Precinct ID Formatting
Election results use specific precinct ID formatting:
```r
# Format precinct IDs as "34XXX" (district 34 + 3-digit ED number)
Precinct.results$X <- sprintf("34%03d", as.numeric(sub("ED ", "", Precinct.results$X)))
```

### Vote Aggregation
Vote totals are calculated across multiple party lines:
```r
# Aggregate votes across ballot lines (e.g., Democratic + Working Families)
Precinct.results$Zohran.Total <- as.numeric(Precinct.results$Zohran.Kwame.Mamdani) +
                                  as.numeric(Precinct.results$Zohran.Kwame.Mamdani.1)
```

## Coordinate Reference Systems

The project uses multiple CRS depending on context:
- **3857** (Web Mercator) - Primary CRS for analysis and area calculations
- **4326** (WGS84) - Used for map tile alignment and final visualization
- Always use `st_transform()` to switch between CRS when needed
- OSM tiles require data in EPSG:4326 for proper alignment

## PDF Output Conventions

PDFs are generated using R's native grid system:
```r
# Standard PDF generation
pdf("outputs/reports/output_file.pdf", width = 8.5, height = 11)
grid.arrange(plot1, plot2, nrow = 2)  # or print patchwork objects
dev.off()
```

For landscape orientation:
```r
pdf("outputs/reports/output_file.pdf", width = 11, height = 8.5)
```

## Spatial Data Sources

- **TIGER/Line**: Via `tigris` package for boundaries (tracts, block groups, legislative districts)
- **NYS Election Districts**: Local shapefiles in `data/raw/election/`
- **SLDL Crosswalks**: Census Bureau tract-to-district mappings in `data/raw/crosswalks/`
- **Geodatabases**: Full US legislative boundary GDB in `data/raw/census/`

## Common Development Patterns

### Creating Directories
Scripts ensure output directories exist:
```r
dir.create("outputs/AD34_outputs", showWarnings = FALSE, recursive = TRUE)
```

### Variable Discovery
Use `load_variables()` to explore ACS tables:
```r
vars <- load_variables(2023, "acs5", cache = TRUE)
vars %>% filter(str_detect(name, "^B08301"))  # Find transit variables
```

### Weighted Means for District Summaries
When aggregating tract-level data to district level:
```r
weighted_mean <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}
```

## Known Data Issues

1. Some scripts have duplicate library imports (e.g., `library(sf)` called multiple times) - this is harmless but can be cleaned up
2. Census variables sometimes use "E" suffix (estimates) vs "PE" suffix (percent estimates) - scripts handle both
3. Block group data requires filtering for `total_pop > 0` to avoid division by zero in percentages
4. Some partial tract handling differs between scripts (centroid-based vs area-based filtering)
5. Election district shapefile has long numeric directory name - keep path consistent

## Tips for New Development

1. **Always start from project root**: Set `setwd("~/Mayoral Results AD34")`
2. **Use relative paths**: Reference files relative to project root
3. **Check CRS**: Transform geometries to 3857 for analysis, 4326 for map display
4. **Filter to AD34**: Use established filter functions for consistency
5. **Save to organized directories**: Follow the outputs/ structure
6. **Test variable codes**: Use variable explorer scripts before pulling large datasets
7. **Handle NA values**: Census data often has missing values for small geographies
