# NYC Election Data Download Guide

This guide explains how to use the automated NYC Board of Elections data downloader.

## Quick Start

```r
source("scripts/01_data_prep/01_download_nyc_election_data.R")
```

This will automatically download the latest election results from https://vote.nyc/page/election-results-summary

## What Gets Downloaded

The script downloads from the NYC Board of Elections results page:

### 1. Official Results (CSV Format)
- **Recap Files**: Summary results by contest
- **ED Level Files**: Detailed results by Election District (ED)
- Organized by election date and contest type

### 2. Unofficial Results (if available)
- Live results from enr.boenyc.gov during active elections
- Automatically scraped tables saved as CSV

### 3. Filtered Data
- Automatically extracts data for Assembly District 34 (configurable)
- Creates separate files with `_AD34` suffix

## Configuration Options

Edit the `CONFIG` list at the top of the script (lines 30-38):

```r
CONFIG <- list(
  download_csv_recap = TRUE,      # Summary results
  download_csv_ed = TRUE,          # Detailed ED-level results
  download_pdf_recap = FALSE,      # PDF summaries (optional)
  download_pdf_ed = FALSE,         # PDF details (optional)
  download_unofficial = TRUE,      # Current unofficial results
  max_elections = 3,               # How many recent elections (NULL = all)
  assembly_district = "34"         # Your AD number
)
```

### Configuration Tips

**For quick updates:**
```r
CONFIG$max_elections = 1  # Just the latest election
CONFIG$download_csv_recap = TRUE
CONFIG$download_csv_ed = FALSE  # Skip detailed data
```

**For comprehensive analysis:**
```r
CONFIG$max_elections = NULL  # Download all available
CONFIG$download_csv_ed = TRUE  # Include ED-level detail
```

**For other districts:**
```r
CONFIG$assembly_district = "36"  # Change to your district
```

## Output Locations

### Downloaded Files
```
data/raw/election/downloads/
├── [Contest_Name]_Recap.csv
├── [Contest_Name]_EDLevel.csv
└── ...
```

### Unofficial Results
```
data/raw/election/unofficial/
├── unofficial_main_results_20250115.csv
└── unofficial_table_2_20250115.csv
```

### Filtered Data
```
data/raw/election/
├── [Contest_Name]_AD34.csv
└── ...
```

## Data Formats

### CSV Recap Files
Typical columns:
- Contest/Office name
- Candidate names
- Vote totals by candidate
- Percentages
- Party affiliations

### CSV ED Level Files
Additional columns:
- Election District (ED) number
- Assembly District
- County
- Vote counts per ED

### Unofficial Results Tables
Structure varies by election, typically includes:
- Live vote counts
- Reporting percentage
- Candidate/party breakdowns
- Last update timestamp

## Common Use Cases

### 1. Download Latest Primary Results
```r
# Edit CONFIG to:
CONFIG$max_elections = 1
CONFIG$download_csv_recap = TRUE

# Run script
source("scripts/01_data_prep/01_download_nyc_election_data.R")

# Results will be in:
# data/raw/election/downloads/
```

### 2. Get Detailed ED Data for Your District
```r
# Edit CONFIG to:
CONFIG$download_csv_ed = TRUE
CONFIG$assembly_district = "34"

# Run script - filtered files will be created automatically
# Look for: data/raw/election/*_AD34.csv
```

### 3. Monitor Live Election Night Results
```r
# Edit CONFIG to:
CONFIG$download_unofficial = TRUE
CONFIG$max_elections = 1

# Run script periodically during election night
# Each run saves timestamped unofficial results
```

### 4. Historical Analysis
```r
# Edit CONFIG to:
CONFIG$max_elections = NULL  # Download all
CONFIG$download_csv_recap = TRUE
CONFIG$download_csv_ed = TRUE

# Run script once to build historical database
# Warning: This downloads a LOT of data!
```

## Integration with Analysis Scripts

After downloading, use the data with existing analysis scripts:

```r
# Load the filtered AD34 data
results <- read_csv("data/raw/election/[Contest]_AD34.csv")

# Use with election mapping scripts
source("scripts/03_election_analysis/01_election_winners_map.R")
```

## Troubleshooting

### "No downloadable files found"
- Check if the vote.nyc website structure has changed
- Verify internet connection
- Try visiting https://vote.nyc/page/election-results-summary manually

### "Could not parse unofficial results"
- The ENR system may use JavaScript or require authentication
- Visit the ENR page manually to download
- Check `unofficial_link$href` for the correct URL

### "HTTP 404 errors"
- Some older elections may have moved or been archived
- Try reducing `max_elections` to focus on recent data
- Check the vote.nyc website for archive section

### File encoding issues
- NYC BOE files sometimes have special characters
- The script uses UTF-8 encoding by default
- If you see garbled text, try: `read_csv(file, locale = locale(encoding = "latin1"))`

## Data Updates

### When to Re-run

- **After elections**: Usually within 24-48 hours for official results
- **During election night**: Every 30-60 minutes for unofficial updates
- **Monthly**: To catch any corrections or updated files

### Automation (Optional)

Set up a cron job or scheduled task:
```bash
# Run every day at 2 AM
0 2 * * * Rscript ~/Mayoral\ Results\ AD34/scripts/01_data_prep/01_download_nyc_election_data.R
```

## API Rate Limiting

The script includes delays between downloads (`Sys.sleep(0.5)`) to be respectful to the BOE servers.

**Do not:**
- Remove the sleep delays
- Run the script in rapid succession
- Download all historical data repeatedly

**Do:**
- Use `max_elections` to limit downloads
- Cache downloaded files
- Only re-download when you need updated data

## File Naming Conventions

Files are automatically named based on:
1. Contest description from the website
2. File type (Recap or EDLevel)
3. Original filename from BOE

Special characters are replaced with underscores for filesystem compatibility.

## Data Quality Notes

### Official vs Unofficial
- **Official results**: Certified, final vote counts (available days/weeks after election)
- **Unofficial results**: Real-time counts during election night (may change)

### ED Level Data
- More detailed but larger files
- Useful for precinct-level analysis
- May include absentee and affidavit votes separately

### Recap Data
- Summarized totals
- Faster to download and process
- Good for district-wide analysis

## Related Documentation

- See `CLAUDE.md` for overall project architecture
- See `DATA_SOURCES.md` for all data dependencies
- See `README.md` for general project information

## Support

If you encounter issues:
1. Check the NYC BOE website manually: https://vote.nyc
2. Review the script output for specific error messages
3. Verify your Assembly District number in CONFIG
4. Check that required R packages are installed (`rvest`, `httr`, `tidyverse`)
