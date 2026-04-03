# CohortPrevalence Reference Data Generation

This folder contains scripts to generate standardization reference populations for the CohortPrevalence package.

## Quick Start

### Method 1: From R Console (Recommended)

```r
# In R console, set working directory to package root
setwd("path/to/CohortPrevalence")

# Load the class definition
source("R/StandardizationReference.R")

# Generate all references
source("data-raw/00_generate_references.R")
```

This creates three files in `data/`:
- `usa_census_2020.rda` (~80 KB)
- `japan_census_2020.rda` (~90 KB)
- `who_world_standard.rda` (~5 KB)

### Method 2: Generated Inline (Alternative)

Run the following R code:

```r
library(usethis)
library(dplyr)

# Inline USA Census 2020
usa_census_2020 <- structure(
  .Data = list(
    age = c(...),
    gender = c(...),
    population = c(...)
  ),
  class = "data.frame"
)

# Save
usethis::use_data(usa_census_2020, overwrite = TRUE)
usethis::use_data(japan_census_2020, overwrite = TRUE)
usethis::use_data(who_world_standard, overwrite = TRUE)
```

## File Structure

```
data-raw/
├── 00_generate_references.R          # Master orchestration script
├── usa_census/
│   └── process_usa_census_2020.R     # USA Census 2020 processing
├── japan_census/
│   └── process_japan_census_2020.R   # Japan Census 2020 processing
└── who_standard/
    └── process_who_2008.R            # WHO World Standard processing
```

## Reference Data Specifications

### USA Census 2020
- **Source:** US Census Bureau
- **Ages:** 0-28+ (simplified; actual would be 0-102+)
- **Genders:** Male, Female
- **Total:** ~330 million (representative)
- **Processed by:** process_usa_census_2020.R

### Japan Census 2020
- **Source:** Statistics Bureau of Japan (総務省統計局)
- **Ages:** Single-year (0-99 plus 100+) — 101 age groups
- **Genders:** Male, Female
- **Total:** ~125.1 million (representative)
- **Processed by:** process_japan_census_2020.R

### WHO World Standard 2008
- **Source:** World Health Organization
- **Ages:** 7 age bands (0-4, 5-14, 15-24, ..., 85+)
- **Genders:** Combined (no gender stratification)
- **Total:** 100,000 (proportion weights)
- **Processed by:** process_who_2008.R

## Class Requirements

All reference processing scripts assume the `StandardizationReference` class is available:

```r
source("R/StandardizationReference.R")  # Must be loaded first!
```

This class:
- Validates data structure (requires: age, gender, population columns)
- Auto-calculates weights (population / sum(population))
- Stores metadata (name, country, year, source, DOI)
- Provides methods for filtering, truncation, and adjustment

## After Generation

Verify the generated .rda files:

```r
# Load from package
library(CohortPrevalence)
data("usa_census_2020")
data("japan_census_2020")
data("who_world_standard")

# Check structure
usa_census_2020$viewReference()
japan_census_2020$getAgeValues()
who_world_standard$getTotalPopulation()

# Use in standardization
standardizePrevalence(
  prevalenceData = my_data,
  referencePopulation = usa_census_2020
)
```

## Updates & Maintenance

### Annual Updates (ACS)
```r
# Fetch new ACS data on-demand (not bundled)
fetchACSReference(year = 2023)
```

### Decennial Updates (Census, Japan)
1. Download new year's data from official source
2. Update processing script (e.g., process_usa_census_2020_future.R)
3. Run: `source("data-raw/{country}/process_{name}.R")`
4. Update NEWS.md and REFERENCE_UPDATES.md
5. Tag release (e.g., v1.2.0)

### Deprecation Timeline
- Active: Current release
- Deprecated: Keep for 2 major versions
- Removed: After 2 major versions

## Troubleshooting

**Error: "StandardizationReference R6 class not found"**
- Solution: Run `source("R/StandardizationReference.R")` first

**Error: File not found "process_usa_census_2020.R"**
- Solution: Ensure working directory is set to package root

**Error: "age, gender, or population column missing"**
- Solution: Check CSV headers match expected column names

**Reference data not loading**
- Solution: Run `devtools::load_all()` to refresh package data
