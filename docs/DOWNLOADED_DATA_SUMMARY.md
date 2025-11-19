# Downloaded Election Data Summary

This document summarizes what election data has been successfully downloaded for AD34.

---

## Data Location Overview

```
data/raw/election/
├── ad34/              ⭐ AD34-filtered Election District data (PRIMARY SOURCE)
├── ed_level/          All downloaded ED Level CSVs from vote.nyc
├── enr_results/       ENR unofficial results (borough-level, Nov 2025)
└── unofficial/        ENR table downloads (navigation pages)
```

---

## ✅ Successfully Downloaded

### 1. AD34 Election District Data (June 2025 Primary)
**Location:** `data/raw/election/ad34/`
**Source:** vote.nyc EDLevel CSV files, filtered for AD34

| Contest | Rows | Data Quality |
|---------|------|--------------|
| Judge of the Civil Court - County Queens | 390 | ✅ Complete ED-level data |
| Judge of the Civil Court - District 1st Municipal | 145 | ✅ Complete ED-level data |
| Judge of the Civil Court - District 2nd Municipal | 240 | ✅ Complete ED-level data |

**Data includes:**
- Assembly District: 34
- Election Districts: Various (001, 002, 003, etc.)
- Vote counts by:
  - Voting method (Public Counter, Absentee, etc.)
  - Candidate name
- Election: Primary Election 2025 - 06/24/2025
- Party: Democratic

**Limitation:** ⚠️ Only judicial races, no Mayor/Comptroller/Public Advocate

---

### 2. ENR Unofficial Results (November 2025 General)
**Location:** `data/raw/election/enr_results/`
**Source:** https://enr.boenyc.gov

#### Contests Downloaded (33 tables total):

##### Citywide Races (3 tables each):
- ✅ **Mayor** - Borough-level breakdowns
- ✅ **Public Advocate** - Borough-level breakdowns
- ✅ **Comptroller** - Borough-level breakdowns
- ✅ **Borough President** - Borough-level breakdowns

##### Council & Proposals:
- ✅ **City Council** - All 51 districts
- ✅ **Ballot Proposals 1-6** - Borough-level breakdowns

**Data includes:**
- Borough totals (New York, Bronx, Kings, **Queens**, Richmond)
- Reported % (scanner completion)
- Vote counts by candidate
- Vote counts by party line
- Write-in votes

**Election:** General Election - November 4, 2025
**Status:** UNOFFICIAL (election night results)

**Limitation:** ⚠️ Borough-level only, cannot isolate AD34 from Queens total

#### Example: Mayor Race (Nov 2025) - Queens Borough Total
From ENR data, you can see:
- Total votes in Queens borough (which includes AD34)
- Candidates: Zohran Kwame Mamdani, Curtis A. Sliwa, Eric L. Adams, etc.
- But **cannot** separate out AD34 specifically

---

### 3. Other Queens ED Level Data (June 2025 Primary)
**Location:** `data/raw/election/ed_level/`

Downloaded but **NO AD34 data** found in:
- District Leader races (ADs 65, 68, 70, 72, 74, 76)
- Delegate to Judicial Convention (ADs 28, 66, 68, 70, 71, 72, 74)
- County Committee races (various other ADs)

---

## ❌ Data NOT Available

### Missing AD34 Data:

1. **Mayor - AD34 Level** ❌
   - Have: Queens borough total (from ENR)
   - Need: Wait for vote.nyc to publish certified EDLevel CSVs

2. **Public Advocate - AD34 Level** ❌
   - Have: Queens borough total (from ENR)
   - Need: Wait for vote.nyc certification

3. **Comptroller - AD34 Level** ❌
   - Have: Queens borough total (from ENR)
   - Need: Wait for vote.nyc certification

4. **June 2025 Primary - Citywide Races for AD34** ❌
   - June primary only had judicial races in Queens/AD34
   - No Mayor/Comptroller primaries in AD34

### Why Missing?

The November 4, 2025 General Election results are:
- ✅ Available on ENR (unofficial, borough-level)
- ❌ NOT yet available on vote.nyc (pending certification)
- ⏰ Typically certified 2-4 weeks after election day
- 📅 Expected: Late November to early December 2025

---

## 📊 Current Analysis Capabilities

### What You CAN Do Now:

✅ **June 2025 Primary Analysis:**
- Analyze 3 judicial contests at ED-level for AD34
- Map voting patterns by Election District
- Compare turnout across EDs within AD34

✅ **November 2025 General - Borough Proxy:**
- Use Queens borough totals as AD34 proxy
- Understand overall race dynamics
- Track borough-level results

### What You CANNOT Do Yet:

❌ AD34-specific analysis for Mayor/Comptroller/Public Advocate
❌ Map Nov 2025 election results by ED within AD34
❌ Compare AD34 performance vs other Queens ADs for Nov 2025

---

## 🔄 Next Steps

### Option 1: Wait for Certification (Recommended)
- Monitor vote.nyc for EDLevel CSV publication
- Re-run: `scripts/01_data_prep/03_download_ad34_election_results.R`
- Expected: Late November - Early December 2025

### Option 2: Download Historical Elections
- Look for 2021 Mayoral race (already certified)
- Look for 2024 Presidential/Congressional races
- These should have EDLevel data available now

### Option 3: Use Borough Totals
- Analyze Queens borough results from ENR
- Note: AD34 is ~1 of 18 Assembly Districts in Queens
- Approximation only, not precise

---

## 🛠️ Scripts to Re-Run

### When EDLevel CSVs are Published:
```bash
# Re-download and filter for AD34
Rscript scripts/01_data_prep/03_download_ad34_election_results.R
```

### For Historical Elections:
```r
# Modify CONFIG in the script:
MAX_ELECTIONS <- 10  # Download more elections
filter_queens_only <- TRUE  # Keep Queens focus
```

### For Election Night Updates:
```bash
# Get latest unofficial results
Rscript scripts/01_data_prep/04_parse_enr_unofficial_results.R
```

---

## 📋 Data Quality Notes

### ED Level CSVs (vote.nyc)
- ✅ Official, certified results
- ✅ Election District granularity
- ✅ Clean, structured data
- ⚠️ Weird CSV format (headers in data rows)
- ✅ Scripts handle this properly

### ENR Results (enr.boenyc.gov)
- ⚠️ Unofficial, subject to change
- ⚠️ Borough-level only (not AD-level)
- ✅ Real-time election night
- ⚠️ Large, complex HTML tables
- ⚠️ "AD Details" is misleading name

---

## 🗺️ For More Information

See: `docs/NYC_ELECTION_WEBSITES_MAP.md` for complete website structure documentation

---

## Last Updated
2025-11-16 14:51

## Summary
- ✅ 3 AD34 datasets (June 2025 Primary - judges only)
- ✅ 33 ENR tables (Nov 2025 General - borough level)
- ❌ No AD34-level data for Mayor/Comptroller/Public Advocate yet
- ⏰ Waiting for vote.nyc certification (expected late Nov 2025)
