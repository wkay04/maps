# NYC Election Data Websites - Navigation Map

This document maps out the structure and data available on NYC's election websites.

---

## 1. vote.nyc - Official Board of Elections Results
**URL:** https://vote.nyc/page/election-results-summary

### Site Structure

```
vote.nyc/page/election-results-summary (Main Landing Page)
│
├── Election Results by Year (Tables organized chronologically)
│   ├── 2025 Elections
│   │   ├── Primary Election - June 24, 2025
│   │   └── General Election - November 4, 2025 (pending certification)
│   │
│   ├── 2024 Elections
│   │   ├── General Election - November 5, 2024
│   │   └── Primary Election - June 25, 2024
│   │
│   └── Earlier Years (2023, 2021, 2020, etc.)
│
└── For Each Election → For Each Contest → Multiple File Formats
    ├── CSV Files
    │   ├── **[Contest]_Recap.csv** - Summary results
    │   └── **[Contest]_EDLevel.csv** - Election District detail ⭐
    │
    ├── PDF Files
    │   ├── [Contest]_Recap.pdf
    │   └── [Contest]_EDLevel.pdf
    │
    ├── Excel Files (.xls, .xlsx)
    └── HTML Results Pages
```

### Available Contest Types

#### Citywide Races (All 5 Boroughs)
- Mayor
- Public Advocate
- Comptroller

#### Borough-Specific Races
- Borough President (by borough)
- District Attorney (by county/borough)

#### District Races
- Member of the City Council (by council district)
- State Senate (by senate district)
- State Assembly (by assembly district)
- U.S. House (by congressional district)

#### Judicial Races
- Justice of the Supreme Court
- Judge of the Civil Court (County & District)
- Surrogate Court Judge
- Delegate to Judicial Convention (by AD)

#### Party Positions
- District Leader (by AD)
- County Committee (by ED within AD)
- Alternate Delegate to Judicial Convention

#### Ballot Measures
- Proposals/Questions (numbered)

### EDLevel CSV File Structure ⭐ PRIMARY DATA SOURCE

**File naming pattern:**
```
/sites/default/files/pdf/election_results/[YEAR]/[DATE][Election Type]/[Code][Contest Name] EDLevel.csv
```

**Example:**
```
01401600013Queens Democratic Judge of the Civil Court - County Queens EDLevel.csv
```

**Data Structure:**
- **Columns 1-11:** Header labels (repeated in every row as data)
  - "AD", "ED", "County", "EDAD Status", "Event", "Party/Independent Body",
    "Office/Position Title", "District Key", "VoteFor", "Unit Name", "Tally"
- **Columns 12-22:** Actual data values
  - Column 12: Assembly District number (e.g., "34")
  - Column 13: Election District number (e.g., "001")
  - Column 14: County name (e.g., "Queens")
  - Column 15: EDAD Status (e.g., "IN-PLAY")
  - Column 16: Event (e.g., "Primary Election 2025 - 06/24/2025")
  - Column 17: Party (e.g., "Democratic")
  - Column 18: Office (e.g., "Judge of the Civil Court - County")
  - Column 19: District Key
  - Column 20: Vote For (number of seats)
  - Column 21: Unit Name (candidate name OR voting method)
  - Column 22: Tally (vote count)

**Voting Method Breakdowns in EDLevel Files:**
Each ED has multiple rows showing:
- Public Counter
- Manually Counted Emergency
- Absentee / Military
- Affidavit
- [Candidate 1 Name]
- [Candidate 2 Name]
- ...
- Scattered (write-ins)

### Geographic Coverage by Borough

| Borough | County Name | Assembly Districts | Example ADs |
|---------|-------------|-------------------|-------------|
| Manhattan | New York | 37, 61, 65-76 | AD 65-76 (mostly) |
| Bronx | Bronx | 77-87 | AD 77-87 |
| Brooklyn | Kings | 41-60 | AD 41-60 |
| Queens | Queens | 23-40 | AD 23-40 ⭐ (AD34 is here) |
| Staten Island | Richmond | 61-63 | AD 61-63 |

### How to Filter for AD34

**AD34 Location:** Queens (Jackson Heights, Corona, East Elmhurst, Astoria)

**Relevant Files:**
1. ✅ Queens county-wide races (District Attorney, Judge of Civil Court)
2. ✅ Citywide races (Mayor, Public Advocate, Comptroller) - **BUT** these may only be in Manhattan or aggregate files
3. ✅ Queens Council Districts that overlap AD34
4. ✅ Delegate to Judicial Convention - would need AD34-specific file
5. ✅ District Leader for AD34
6. ✅ County Committee for AD34

**File Selection Strategy:**
```r
# Download all files with "Queens" in the path
str_detect(href, "Queens")

# Also download Manhattan citywide races (they may include all ADs)
str_detect(href, "New York") & str_detect(text, "Citywide|Mayor|Advocate|Comptroller")

# Download any file explicitly mentioning AD34
str_detect(href, "34") & str_detect(href, "Assembly")
```

### Unofficial Results Link

At the top of the page during/after elections:
- **Link text:** "View Unofficial Results" or "Unofficial Election Night Results"
- **Destination:** https://enr.boenyc.gov
- **Status:** Only active during election night and shortly after

---

## 2. enr.boenyc.gov - Election Night Reporting System
**URL:** https://enr.boenyc.gov

### Site Structure

```
enr.boenyc.gov (Landing Page)
│
├── Active only during/after elections
├── Shows real-time unofficial results
└── Updates throughout election night
│
├── Contest Selection (15 contests for Nov 2025 General)
│   │
│   ├── For Each Contest:
│   │   ├── Summary Results (citywide totals)
│   │   └── "AD Details*" Link
│   │
│   └── "AD Details" Pages (misleading name!)
│       ├── [ContestCode]ADI0.html (e.g., CD27286ADI0.html for Mayor)
│       └── Shows BOROUGH-level breakdowns, NOT Assembly District
│
└── Contest Navigation by Borough
    ├── New York (Manhattan)
    ├── Bronx
    ├── Kings (Brooklyn)
    ├── Queens ⭐
    └── Richmond (Staten Island)
```

### Available Contests (November 2025 General Election)

| # | Contest | File Code | Has AD Details |
|---|---------|-----------|----------------|
| 1 | Mayor | CD27286ADI0.html | Yes (borough-level) |
| 2 | Public Advocate | CD27287ADI0.html | Yes (borough-level) |
| 3 | City Comptroller | CD27293ADI0.html | Yes (borough-level) |
| 4 | Justice of the Supreme Court | OF9AD0PY1.html | Yes (borough-level) |
| 5 | District Attorney | OF14AD0PY35.html | Yes (borough-level) |
| 6 | Borough President | OF15AD0PY1.html | Yes (borough-level) |
| 7 | Judge of the Civil Court - County | OF16AD0PY1.html | Yes (borough-level) |
| 8 | Member of the City Council | OF18AD0PY3.html | Yes (by district) |
| 9 | Judge of the Civil Court - District | OF17AD0PY1.html | Yes (borough-level) |
| 10-15 | Proposal Numbers 1-6 | CD27305-27310ADI0.html | Yes (borough-level) |

### ENR Page Data Structure

Each "AD Details" HTML page contains **3 tables**:

#### Table 1: Detailed Results by Borough
- **Rows:** One per borough (New York, Bronx, Kings, Queens, Richmond)
- **Columns:**
  - Reported % (scanner completion)
  - Vote counts for each candidate
  - Vote counts by party/ballot line
  - Write-in votes

#### Table 2: Summary by Borough
- Similar to Table 1 but with fewer columns
- May aggregate some party lines

#### Table 3: Legend/Navigation
- Contest list
- Borough selection links
- Footer information

### ⚠️ Critical Limitation

**The "AD Details" name is MISLEADING:**
- These pages show **BOROUGH-level** aggregates
- They do **NOT** show Assembly District breakdowns
- To get AD34 data, you need to:
  1. Look at Queens borough total (includes all Queens ADs: 23-40)
  2. Cannot isolate AD34 from ENR system alone

### ENR vs vote.nyc Comparison

| Feature | ENR (enr.boenyc.gov) | vote.nyc |
|---------|----------------------|----------|
| **Timing** | Election night, real-time | After certification (weeks later) |
| **Status** | Unofficial | Official, certified |
| **Granularity** | Borough-level | Election District-level ⭐ |
| **Format** | HTML tables | CSV, PDF, Excel, HTML |
| **AD34 Data** | ❌ No (only Queens total) | ✅ Yes (ED-level detail) |
| **Use Case** | Live results on election night | Historical analysis, research |
| **Availability** | Limited time window | Permanent archive |

### When to Use Which Source

**Use ENR for:**
- ✅ Live results on election night
- ✅ Quick borough-level snapshots
- ✅ Understanding overall race dynamics
- ❌ NOT for AD34-specific analysis

**Use vote.nyc for:**
- ✅ AD34-specific data (filter EDLevel CSVs)
- ✅ Election District granularity
- ✅ Historical elections
- ✅ Official certified results
- ✅ Detailed candidate performance by ED

---

## 3. Data Download Strategy for AD34

### Optimal Approach

```
1. PRIMARY SOURCE: vote.nyc EDLevel CSV files
   ├── Download all Queens EDLevel CSVs
   ├── Download citywide race EDLevel CSVs (all boroughs)
   └── Filter each file for AD == "34"

2. SUPPLEMENTARY: ENR for election night monitoring
   ├── Use Queens borough totals as proxy
   └── Note: Cannot isolate AD34 from borough total

3. HISTORICAL DATA: vote.nyc archives
   ├── 2021 General (Mayor, Comptroller, etc.)
   ├── 2024 General (Presidential, Congress)
   └── Various primaries
```

### Script Recommendations

**For AD34 Analysis:**
```r
# Use: scripts/01_data_prep/03_download_ad34_election_results.R
# - Downloads EDLevel CSVs from vote.nyc
# - Filters for AD34
# - Saves clean datasets
```

**For Election Night Monitoring:**
```r
# Use: scripts/01_data_prep/04_parse_enr_unofficial_results.R
# - Scrapes ENR tables
# - Saves borough-level results
# - Use Queens totals as AD34 proxy
```

---

## 4. Common Questions

### Q: Why can't I find AD34 data in ENR?
**A:** ENR only provides borough-level data, not Assembly District breakdowns.

### Q: Where can I get Mayor/Comptroller results for AD34?
**A:** Wait for vote.nyc to publish certified EDLevel CSV files after the election is certified (usually 2-4 weeks after election day).

### Q: Can I get election night results for AD34?
**A:** Not directly. Use Queens borough totals from ENR as an approximation.

### Q: What's the difference between Recap and EDLevel?
**A:**
- **Recap** = Summary totals (citywide or district-wide)
- **EDLevel** = Detailed breakdowns by Election District (smallest voting unit)

### Q: How do I know which Council District covers AD34?
**A:** AD34 (Queens) overlaps multiple council districts. Check the crosswalk or look for council races in Queens with "34" nearby.

---

## 5. AD34 Geography Reference

**Assembly District 34:**
- **Borough:** Queens
- **Neighborhoods:** Jackson Heights, Corona, East Elmhurst, parts of Astoria
- **Represented by:** Jessica González-Rojas (as of 2025)

**File Identifiers:**
- County: "Queens"
- AD: "34"
- ED: Various (001, 002, 003, ... up to 100+)

**Search Patterns:**
```r
# In filenames
"Queens.*34"
"34.*Assembly"

# In data (column 12 of EDLevel CSVs)
AD == "34"

# In county column
County == "Queens"
```

---

## Last Updated
2025-11-16

## Maintained By
Generated by Claude Code for "Mayoral Results AD34" project
