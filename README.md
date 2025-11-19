# NYC Assembly District 34 Analysis

A comprehensive demographic and election analysis project for Queens Assembly District 34 using American Community Survey data and election results.

## Overview

This R-based analysis project generates detailed demographic chartbooks, election maps, and comparative analyses for NYC Assembly District 34 (Queens). The project combines Census Bureau data with election results to create publication-ready visualizations and reports.

## Key Outputs

### Demographic Chartbooks
- **Mobility & Housing Edition**: Transit patterns, housing costs, homeownership rates, and ancestry composition
- **Comprehensive Demographics**: Population, age, race/ethnicity, income, education, and nativity
- **District Comparisons**: Side-by-side analysis of AD34 vs neighboring districts

### Election Visualizations
- **Winner Maps**: Precinct-level election results with color-coded winners
- **Turnout Analysis**: Voter participation patterns across the district
- **Historical Comparisons**: Multi-election trend analysis

## Project Structure

```
/Mayoral Results AD34/
├── data/
│   ├── raw/                  # Source data (never modify)
│   │   ├── election/        # Precinct results, ED shapefiles
│   │   ├── census/          # SLDL boundaries, geodatabases
│   │   └── crosswalks/      # Geographic relationship files
│   └── intermediate/         # Processed analysis outputs
├── scripts/
│   ├── 02_census_analysis/  # Demographic analysis scripts
│   ├── 03_election_analysis/# Election result processing
│   ├── 04_visualization/    # PDF/map compilation
│   └── utils/               # Helper utilities
├── outputs/
│   ├── reports/             # Final PDF chartbooks
│   ├── maps/                # Map images (PNG)
│   ├── tables/              # Summary statistics (CSV)
│   └── AD34_outputs/        # Intermediate visualizations
├── cache/                   # Tile cache, R session data
└── docs/                    # Documentation
```

## Prerequisites

### Required Software
- **R** (version 4.0+)
- **RStudio** (recommended)

### Required R Packages

Install all dependencies:

```r
install.packages(c(
  # Spatial packages
  "sf", "tigris", "ggspatial", "prettymapr",

  # Census data
  "tidycensus",

  # Data manipulation
  "tidyverse", "dplyr", "stringr", "readr",

  # Visualization
  "ggplot2", "viridis", "patchwork", "gridExtra", "scales",

  # Utilities
  "grid", "png"
))
```

### Census API Key

**Required**: Obtain a free Census API key from https://api.census.gov/data/key_signup.html

The API key is currently hardcoded in scripts as:
```r
census_api_key("ac1b013173d488823dfd5f97b3d16851936e5ef6")
```

To use your own key, replace this line in all scripts, or set it globally:
```r
census_api_key("YOUR_KEY_HERE", install = TRUE)
```

## Quick Start

### 1. Set Working Directory
All scripts expect to run from the project root:
```r
setwd("~/Mayoral Results AD34")
```

### 2. Run Main Analysis Scripts

#### Generate Demographic Chartbook
```r
source("scripts/02_census_analysis/03_demographic_chartbook.R")
# Output: outputs/reports/Queens_AD34_Census_Chartbook.pdf
```

#### Generate Mobility & Housing Chartbook
```r
source("scripts/02_census_analysis/02_chartbook_mobility_housing.R")
# Output: outputs/AD34_outputs/AD34_Chartbook_2023_MOBILITY_HOUSING.pdf
```

#### Map Election Winners
```r
source("scripts/03_election_analysis/01_election_winners_map.R")
# Output: Interactive map displayed in viewer
```

#### Compare Districts
```r
source("scripts/02_census_analysis/04_district_comparison.R")
# Output: outputs/tables/Queens_AD34_AD36_Pivoted_Comparison_2023.csv
```

## Key Features

### Spatial Analysis
- **Boundary Detection**: Automatic filtering of Census tracts/block groups to AD34
- **CRS Management**: Handles coordinate system transformations (Web Mercator ↔ WGS84)
- **Partial Tract Handling**: Multiple methods for tracts that cross district boundaries

### Data Integration
- **Census ACS 5-year estimates** (2019-2023)
- **TIGER/Line shapefiles** via `tigris` package
- **NYC election district boundaries**
- **Precinct-level election results**

### Visualization
- **OpenStreetMap basemaps** for geographic context
- **Viridis color palettes** for accessibility
- **Multi-page PDF reports** with grid layouts
- **Consistent styling** across all outputs

## Common Tasks

### Explore Available Variables
```r
source("scripts/utils/variable_explorer.R")
```

### Update Analysis Year
To change from 2023 to another year:
1. Update `year = 2023` in `get_acs()` calls
2. Update `year = 2025` in `state_legislative_districts()` to match boundary year
3. Check variable availability with `load_variables(YEAR, "acs5")`

### Add New ACS Variables
1. Find variable codes at https://data.census.gov or use `load_variables()`
2. Add to the relevant `variables = c(...)` vector
3. Create derived metrics in the `mutate()` section
4. Generate map with `make_map()` function

### Customize Maps
Key parameters to adjust:
- `zoom = 14` - OpenStreetMap zoom level (higher = more detail)
- `option = "plasma"` - Viridis color palette (plasma, viridis, magma, inferno, turbo)
- `linewidth = 0.08` - Border thickness
- `alpha = 0.8` - Transparency (0-1)

## Data Sources

### Census Bureau
- **American Community Survey (ACS)**: Demographics, economics, housing, commuting
- **TIGER/Line Shapefiles**: Boundary files for tracts, block groups, districts
- **Crosswalk Files**: Geographic relationship tables

### NYC/NYS Election Data
- **Precinct Results**: `data/raw/election/Precinct results.csv`
- **Election Districts**: Shapefiles with polling locations
- Source: NYC Board of Elections / NYS Board of Elections

## Technical Notes

### Coordinate Reference Systems
- **EPSG:3857** (Web Mercator): Used for area calculations and spatial operations
- **EPSG:4326** (WGS84): Used for map display with OSM tiles
- Always transform geometries before spatial joins or area calculations

### Census Geography Hierarchy
```
State (NY)
  └─ County (Queens)
      └─ Census Tract
          └─ Block Group
```

Assembly Districts (SLDL) cross tract and block group boundaries, requiring spatial filtering.

### Performance Tips
1. **Enable caching**: `options(tigris_use_cache = TRUE)`
2. **Limit geography**: Pull only Queens County, not entire state
3. **Use wide format**: `output = "wide"` is faster than pivoting later
4. **Filter early**: Apply AD34 filter before heavy computations

## Troubleshooting

### "Census API key has not been installed"
```r
census_api_key("YOUR_KEY", install = TRUE)
```

### "sf objects don't align"
Check that both objects use the same CRS:
```r
st_crs(object1) == st_crs(object2)
object2 <- st_transform(object2, st_crs(object1))
```

### "No tiles are displayed on map"
Ensure geometry is in EPSG:4326 when using `annotation_map_tile()`:
```r
geodata_ll <- st_transform(geodata, 4326)
```

### "Cannot find file"
Verify you're in the project root:
```r
getwd()  # Should show: ~/Mayoral Results AD34
```

## File Naming Conventions

### Scripts
- Numbered prefixes indicate execution order or category
- Snake_case for readability
- Example: `02_chartbook_mobility_housing.R`

### Outputs
- **PDFs**: Descriptive names with district ID and topic
  - `Queens_AD34_Census_Chartbook.pdf`
- **CSVs**: Include district, topic, and year
  - `Queens_AD34_AD36_Pivoted_Comparison_2023.csv`
- **PNGs**: Variable or map type
  - `map_poverty.png`, `bar_bus_all.png`

## Contributing

### Adding New Analyses
1. Create script in appropriate `scripts/` subdirectory
2. Follow established naming convention (numbered prefix + descriptive name)
3. Use standard helper functions (`filter_to_ad34()`, `pull_sf()`, `make_map()`)
4. Save outputs to organized `outputs/` subdirectories
5. Document key findings in comments

### Code Style
- Use tidyverse conventions
- Comment complex spatial operations
- Include progress messages with `cat()` or emoji indicators
- Keep scripts modular (separate data pull, analysis, visualization)

## Author

William Kay

## License

This project is for research and analysis purposes.

## Acknowledgments

- **U.S. Census Bureau** - American Community Survey data
- **NYC/NYS Board of Elections** - Election results and district boundaries
- **R Community** - sf, tidycensus, tigris, and tidyverse packages
- **OpenStreetMap Contributors** - Basemap tiles

## Related Resources

- [tidycensus documentation](https://walker-data.com/tidycensus/)
- [sf package documentation](https://r-spatial.github.io/sf/)
- [Census Bureau API documentation](https://www.census.gov/data/developers/data-sets.html)
- [ACS variable search](https://data.census.gov)

---

**Last Updated**: November 2024
**Data Vintage**: ACS 2019-2023 (5-year estimates)
