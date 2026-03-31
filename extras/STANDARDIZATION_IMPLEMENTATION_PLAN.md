# Standardization Feature Implementation Plan

**Last Updated:** March 31, 2026  
**Status:** Planning Phase  
**Target Release:** v1.1.0

---

## Executive Summary

Adding direct method standardization to CohortPrevalence package to enable age-sex standardized prevalence rate calculations. Core functionality will support US and Japan databases initially (Phase 1), with extensible architecture for additional countries.

---

## Phase 1: Core Implementation (Priority)

### Implementation Status

**Completed (Code Ready):**
- ✓ StandardizationReference R6 class (190 lines, 9 methods)
  - Methods: initialize, viewReference, getData, getTotalPopulation, getAgeValues, getGenderValues, getMetadata, getFilteredReference, getAgetruncatedReference, getAdjustedReference
  - Validates data structure, auto-calculates weights, detects NA values
  
- ✓ standardizePrevalence() function (240 lines)
  - Algorithm: demographic bounds filtering → age truncation → reference adjustment → weight joining → crude rate calculation → standardized rate calculation → Wilson CI
  - Supports ageMin, ageMax, ageRightTruncation parameters
  - Handles alternative column names (numerator/outcome_count, denominator/population_count)
  - Returns: standardized_rate, ci_lower, ci_upper, crude_rate, reference metadata, diagnostics

- ✓ standardizationHelpers.R (200 lines, 6 functions)
  - createStandardizationReference() - wraps R6 constructor
  - importStandardizationReference() - CSV import with encoding support
  - getStandardizationReference() - retrieve built-in reference (usa_census_2020, japan_census_2020, who_world_standard)
  - listStandardizationReferences() - display available references
  - fetchACSReference() - on-demand ACS fetching via tidyCensus (scaffold)

- ✓ Data processing scripts created
  - USA Census 2020: Representative population data, 29 age groups
  - Japan Census 2020: Single-year ages (0-99 plus 100+), 101 age groups per gender
  - WHO World Standard 2008: 7 age bands, combined gender

**Next Immediate Steps:**
1. Generate .rda files for reference populations (usa_census_2020, japan_census_2020, who_world_standard)
   - Execute: `devtools::load_all()` then `source("data-raw/00_generate_references.R")`
   - Result: data/*.rda files created
   
2. Verify package integrity
   - Check: `devtools::check()` for any warnings/errors
   - Test: `data("usa_census_2020"); usa_census_2020$viewReference()`

3. Integration testing
   - Create sample prevalence data
   - Test standardizePrevalence() with each reference
   - Test demographic bounds & age truncation features
- [x] `StandardizationReference` R6 class - ✓ COMPLETE
- [x] `standardizePrevalence()` function (direct method) - ✓ COMPLETE
- [x] Helper functions (createStandardizationReference, getStandardizationReference, etc.) - ✓ COMPLETE
- [ ] USA Census 2020 reference population (data generation in progress)
- [ ] Japan Census 2020 reference population (data generation in progress)
- [ ] WHO World Standard reference population (data generation in progress)
- [ ] Process scripts in data-raw/ (created, ready to execute)
- [ ] Documentation & vignette (deferred to Phase 2 per user request)
- [ ] Unit tests (deferred to Phase 2 per user request)

### Files Created (Phase 1)
- **New R Classes & Functions:**
  - `R/StandardizationReference.R` - R6 class with 9 methods (initialize, viewReference, getData, getTotalPopulation, etc.)
  - `R/standardize.R` - standardizePrevalence() function with demographic bounds & age truncation support
  - `R/standardizationHelpers.R` - Helper functions for reference management (createStandardizationReference, importStandardizationReference, getStandardizationReference, listStandardizationReferences, fetchACSReference)

- **Data Processing Scripts:**
  - `data-raw/usa_census/process_usa_census_2020.R` - USA Census 2020 processing
  - `data-raw/japan_census/process_japan_census_2020.R` - Japan Census 2020 processing (single-year ages)
  - `data-raw/who_standard/process_who_2008.R` - WHO World Standard processing
  - `data-raw/00_generate_references.R` - Master orchestration script

- **Utility Scripts:**
  - `generate_simple_data.R` - Standalone reference data generation
  - `generate_references_exec.R` - Executable wrapper

### Timeline
- ✓ Week 1: StandardizationReference class & helper functions (DONE)
- ✓ Code review of classes (no errors found)
- → Week 2: Reference data generation & package integration (IN PROGRESS)
- → Week 3: Integration testing (pending)
- → Week 4: Documentation & vignette (deferred)

---

## Japan Census 2020 Strategy - Full Implementation Guide

### Quick Reference

| Item | Details |
|------|---------|
| **Census Year** | 2020 (Decennial) |
| **Data Source** | Statistics Bureau of Japan (総務省統計局) |
| **Next Update** | October 2030 |
| **Download Time** | ~5 minutes |
| **Processing Time** | ~2 minutes |
| **Final Size** | ~150 KB (.rda) |
| **Total Population** | ~125.1 million |

### Step 1: Data Acquisition

#### Method A: Web Download (Recommended for Phase 1)

1. Visit: https://www.stat.go.jp/data/kokusei/2020/
2. Navigate to "Population by Sex and Age"
3. Look for table: **"全国×都道府県×性別,年齢別人口"** (National × Prefecture × Sex × Age)
4. Download: CSV format (Windows-1252 encoding)
   - Alternative name: "Population by Sex and Age" (English)
   - URL pattern: `/...../21-E.csv` or similar

#### File Characteristics
- **No. of Rows:** ~310 rows (2 sexes × 101 age groups + header)
- **No. of Columns:** ~50 columns (1 ID + 47 prefectures + metadata)
- **Encoding:** Shift-JIS or UTF-8 (check file properties)
- **Delimiter:** Comma

#### Expected File Structure
```
Row 1: Header with age labels (年齢 = Age, 男 = Male, 女 = Female, etc.)
Row 2: "全国" (National total) ← USE THIS ROW
Row 3-50: Prefectures (各都道府県) - SKIP for Phase 1
```

#### Save Location
```
data-raw/japan_census/
└── japan_2020_population.csv
```

### Step 2: Data Structure Inspection

Before processing, validate format:

```r
# Quick inspection
japan_raw <- readr::read_csv("data-raw/japan_census/japan_2020_population.csv")

# Check structure
str(japan_raw)
head(japan_raw)

# Expected columns (may be in Japanese):
# - 年齢 or "Age"
# - 男 or "Male" 
# - 女 or "Female"
# - 計 or "Total"
```

### Step 3: Processing Script

**File:** `data-raw/japan_census/process_japan_2020.R`

```r
# Load raw data
japan_2020_raw <- readr::read_csv(
  "data-raw/japan_census/japan_2020_population.csv",
  skip = 1,  # Skip header metadata if needed
  locale = readr::locale(encoding = "Shift-JIS")  # Adjust if needed
) |>
  # Filter to national totals only
  dplyr::filter(row_number() <= 1)  # First data row = national

# Clean column names - handle both Japanese and English headers
japan_2020_clean <- japan_2020_raw |>
  dplyr::select(
    age = c("年齢", "Age"),  # Try Japanese first, fall back to English
    male = c("男", "Male"),
    female = c("女", "Female")
  ) |>
  # Clean age values
  dplyr::mutate(
    age = stringr::str_trim(age),
    age = stringr::str_replace_all(age, "[a-zA-Z]+", ""),  # Remove letters
    age = stringr::str_replace(age, "～", "-"),  # Convert ~ to -
    age = stringr::str_replace(age, "以上", "+")  # 以上 = "and above"
  ) |>
  # Remove any rows with NA in critical columns
  dplyr::filter(!is.na(age), !is.na(male), !is.na(female)) |>
  # Reshape from wide to long
  tidyr::pivot_longer(
    cols = c(male, female),
    names_to = "gender",
    values_to = "population"
  ) |>
  # Normalize gender labels
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "male" | gender == "男" ~ "Male",
      gender == "female" | gender == "女" ~ "Female",
      TRUE ~ stringr::str_to_title(gender)
    )
  ) |>
  # Convert population to numeric (remove commas)
  dplyr::mutate(
    population = as.numeric(stringr::str_remove_all(population, ","))
  ) |>
  # Sort
  dplyr::arrange(gender, age)

# Validation checkpoint
cat("Data Check:\n")
cat("Total Population: ", sum(japan_2020_clean$population), "\n")
cat("  (Expected: ~125,124,000)\n")
cat("Age groups: ", n_distinct(japan_2020_clean$age), "\n")
cat("  (Expected: 101 groups: 0, 1, 2, ..., 100+)\n")
cat("Males: ", sum(japan_2020_clean$population[japan_2020_clean$gender=="Male"]), "\n")
cat("Females: ", sum(japan_2020_clean$population[japan_2020_clean$gender=="Female"]), "\n")

# Create StandardizationReference object
japan_census_2020 <- StandardizationReference$new(
  name = "Japan Census 2020",
  country = "Japan",
  year = 2020L,
  source = "Statistics Bureau of Japan (総務省統計局) - Population Census",
  doi = "https://www.stat.go.jp/data/kokusei/2020/",
  data = japan_2020_clean
)

# Save to package data
usethis::use_data(japan_census_2020, overwrite = TRUE)

# Verify
data("japan_census_2020")
japan_census_2020$viewReference()
```

### Step 4: Validation Checklist

Run these checks after processing:

```r
# Load processed reference
data("japan_census_2020")

# Check 1: Total population
total_pop <- japan_census_2020$getTotalPopulation()
expected_pop <- 125_124_000  # Approximate 2020 Japan population

cat("Total Population Check:\n")
cat("  Expected: ~125,124,000\n")
cat("  Actual:   ", format(total_pop, big.mark=","), "\n")
cat("  ✓ PASS\n" if abs(total_pop - expected_pop) < 100_000 else "  ✗ FAIL\n")

# Check 2: Age groups present
ages <- japan_census_2020$getAgeValues()
cat("\nAge Groups Check:\n")
cat("  Expected: 101 unique ages (0-100+)\n")
cat("  Actual:   ", length(ages), "\n")
cat("  First:    ", min(ages), "\n")
cat("  Last:     ", max(ages), "\n")
cat("  ✓ PASS\n" if length(ages) >= 100 else "  ✗ FAIL\n")

# Check 3: Male/Female balance
ref_data <- japan_census_2020$getData()
male_pop <- sum(ref_data[ref_data$gender == "Male", ]$population)
female_pop <- sum(ref_data[ref_data$gender == "Female", ]$population)
male_pct <- male_pop / total_pop * 100

cat("\nGender Distribution Check:\n")
cat("  Males:   ", format(male_pop, big.mark=","), 
    " (", round(male_pct, 1), "%)\n")
cat("  Females: ", format(female_pop, big.mark=","), 
    " (", round(100-male_pct, 1), "%)\n")
cat("  ✓ PASS\n" if abs(male_pct - 49) < 2 else "  ✗ FAIL\n")

# Check 4: No missing ages
missing_ages <- setdiff(0:100, as.numeric(ages[ages != "100+"]))
if (length(missing_ages) == 0) {
  cat("\nAge Coverage: ✓ PASS (no gaps)\n")
} else {
  cat("\nAge Coverage: ✗ FAIL - Missing ages:", paste(missing_ages, collapse=", "), "\n")
}

# Check 5: Weights sum to 1
weight_sum <- sum(ref_data$weight)
cat("\nWeight Normalization Check:\n")
cat("  Sum of weights: ", weight_sum, "\n")
cat("  ✓ PASS\n" if abs(weight_sum - 1.0) < 0.0001 else "  ✗ FAIL\n")
```

### Step 5: Known Data Issues & Solutions

| Issue | Solution |
|-------|----------|
| **Age Encoding** | Try `locale = readr::locale(encoding = "Shift-JIS")` |
| **Population Commas** | Remove: `stringr::str_remove_all(population, ",")` |
| **Age Format Inconsistency** | Use stringr replacements (~ to -, 以上 to +) |
| **Wrong Data Rows** | Filter to "全国" (national) rows only, SKIP prefectures |
| **Weights Don't Sum to 1** | Recalculate: `weight = population / sum(population)` |
| **Missing Ages** | Check age field for non-numeric/special characters |
| **Gender Column Names** | Use flexible matching: `c("男", "Male")` |

### Step 6: Update Documentation

After successful processing, update:

**File:** `data-raw/REFERENCE_UPDATES.md`

```markdown
## 2026-03-31: Japan Census 2020 (v1.0)
- Source: Statistics Bureau of Japan (総務省統計局)
- Year: 2020 Census (Decennial)
- Download Date: 2026-03-31
- URL: https://www.stat.go.jp/data/kokusei/2020/
- Processing Script: data-raw/japan_census/process_japan_2020.R
- Total Population: 125,124,000
- Age Groups: 101 (0, 1, ..., 100+)
- Gender: Male, Female
- Status: Validated ✓
- Next Update: 2030 (next decennial census)
```

**File:** `R/standardizationData.R` (add documentation)

```r
#' @name japan_census_2020
#' @title Japan Census 2020 Reference Population
#' @description
#' Age-sex stratified population distribution from the 2020 Japanese Decennial Census.
#' Suitable for standardizing prevalence estimates for Japanese databases.
#' 
#' @format
#' A StandardizationReference object containing:
#' - 202 rows (2 sexes × 101 age groups)
#' - age: Single-year (0-99) and 100+
#' - gender: Male, Female
#' - population: Census count
#' - weight: Standardized weight (proportion)
#' 
#' @source Statistics Bureau of Japan
#' \url{https://www.stat.go.jp/data/kokusei/2020/}
#' @keywords data
"japan_census_2020"
```

### Step 7: Integration into 00_generate_references.R

Add to master generation script:

```r
# Master script: data-raw/00_generate_references.R

# ... existing USA Census 2020 code ...

# JAPAN CENSUS 2020
source("data-raw/japan_census/process_japan_2020.R")
# This creates/saves japan_census_2020 to data/

# ... existing WHO World Standard code ...

# List all references created
cat("\n✓ Reference populations created:\n")
cat("  - usa_census_2020\n")
cat("  - japan_census_2020\n")
cat("  - who_world_standard\n")
```

Run entire master script:
```r
source("data-raw/00_generate_references.R")
```

### Step 8: Testing

Add to `tests/testthat/test-standardization.R`:

```r
test_that("Japan Census 2020 loads correctly", {
  data("japan_census_2020")
  
  expect_s3_class(japan_census_2020, "StandardizationReference")
  expect_equal(japan_census_2020$getCountry(), "Japan")
  expect_equal(japan_census_2020$getYear(), 2020L)
  
  # Check total population
  total_pop <- japan_census_2020$getTotalPopulation()
  expect_gt(total_pop, 125_000_000)
  expect_lt(total_pop, 126_000_000)
  
  # Check age coverage
  ages <- japan_census_2020$getAgeValues()
  expect_gte(length(ages), 100)
  
  # Check weights
  ref_data <- japan_census_2020$getData()
  weight_sum <- sum(ref_data$weight)
  expect_equal(weight_sum, 1.0, tolerance = 0.0001)
})
```

### Quick Command Reference

```bash
# Generate all references (including Japan)
Rscript -e "source('data-raw/00_generate_references.R')"

# Test references
Rscript -e "devtools::test(filter='standardization')"

# Check package size
ls -lh data/*.rda

# Verify Japan ref loads
Rscript -e "data('japan_census_2020'); japan_census_2020\$viewReference()"
```

### File Structure

```
data-raw/
  ├── japan_census/
  │   ├── process_japan_2020.R          # Processing script
  │   ├── japan_2020_population.csv     # Downloaded data
  │   └── README.md                     # Source notes
  ├── REFERENCE_UPDATES.md              # Acquisition log
  └── 00_generate_references.R          # Master generation script

R/
  └── standardizationData.R             # Data documentation

tests/testthat/
  └── test-standardization.R            # Unit tests
```

### Status & Timeline

- **Status:** Ready for Implementation
- **Estimated Time:** 1-2 hours (including validation)
- **Risk Level:** Low (official government data, straightforward processing)
- **Final Checklist:**
  - [ ] Downloaded japan_2020_population.csv to data-raw/japan_census/
  - [ ] Validated file structure (age, gender, population columns)
  - [ ] Created process_japan_2020.R processing script
  - [ ] Ran processing script successfully
  - [ ] Validated all 5 checks (population, ages, gender, coverage, weights)
  - [ ] Updated REFERENCE_UPDATES.md
  - [ ] Added documentation to R/standardizationData.R
  - [ ] Integrated into data-raw/00_generate_references.R
  - [ ] Added unit tests to tests/testthat/
  - [ ] Committed to git
  - [ ] Verified data/japan_census_2020.rda created (~150 KB)

## Quick Start: Generate Reference Data

To generate reference population data files, run these commands in R:

```r
# Set working directory
setwd("c:/Users/lavallem/R/open-source/CohortPrevalence")

# Load the StandardizationReference class
source("R/StandardizationReference.R")

# Generate all references (creates .rda files in data/)
source("data-raw/00_generate_references.R")
```

After running, verify:

```r
# Load generated data
data("usa_census_2020")
data("japan_census_2020")
data("who_world_standard")

# Check
usa_census_2020$viewReference()
japan_census_2020$viewReference()
who_world_standard$viewReference()

# Or use package functions
library(CohortPrevalence)
listStandardizationReferences()
```

---



### Problem
Standardization must match the analysis's demographic constraints:
- Analysis restricted to age 18+? Reference weights must account for adults only
- Database masks age as 70+ (Optum)? Reference population must be collapsed similarly

### Solution: Two Parameters

**1. Demographic Bounds (`ageMin`, `ageMax`)**
- Filters reference population to match analysis eligibility
- Re-normalizes weights within bounds
- Example: analysis age 18-150 → weights recalc for 18-150 only

**2. Age Truncation (`ageRightTruncation`)**
- Collapses ages >= threshold into single group
- Handles database-specific masking (Optum 70+, etc)
- Example: ageRightTruncation=70 → combine ages 70, 71, ..., 100+ into "70+"
- Prevents bias when comparing masked data to fine-grained reference data

### Usage Example: Optum DOD
```r
standardizePrevalence(
  res_list$prevalence,
  referencePopulation = japan_census_2020,
  ageMin = 0,                     # Analysis lower bound
  ageMax = 120,                   # Analysis upper bound
  ageRightTruncation = 70         # Optum masking: collapse 70+
)
```

---

- USA ACS annual updates (2023, 2024, 2025, ... via `fetchACSReference(year)`)
- EU Standard 2013 (via Eurostat)
- Canada Census 2021
- Germany Census 2011

---

## Class Architecture

### StandardizationReference R6 Class

**Public Methods:**
- `initialize(name, country, year, source, doi, data)`
- `getName()`, `getCountry()`, `getYear()`, `getSource()`, `getDoi()`
- `getData()` - Returns tibble with age, gender, population, weight
- `getTotalPopulation()`
- `viewReference()` - CLI display
- `getAgeValues()` - Returns unique ages
- `aggregateToAgeBands(breaks)` - Optional: collapse to age bands

**Private Fields:**
- `.name`, `.country`, `.year`, `.source`, `.doi`
- `.data` (tibble with computed weights)
- `.totalPopulation`

### standardizePrevalence() Function

**Input:**
- Dataframe with: age, gender, numerator, denominator columns
- StandardizationReference object

**Output:**
- Tibble with:
  - crude_rate
  - standardized_rate
  - ci_lower, ci_upper (Wilson score CI)
  - reference_population (name)
  - reference_year

**Algorithm:**
1. Join prevalence data to reference population weights
2. Calculate crude rate per stratum
3. Weight each stratum by reference population proportion
4. Sum weighted rates = standardized rate
5. Calculate CI using Poisson variance method

---

## Data Storage

### In Package

```
data/
  ├── usa_census_2020.rda
  ├── usa_acs_2022.rda           # (Optional, if bundled)
  ├── japan_census_2020.rda
  └── who_world_standard.rda
```

### Version Control
```
data-raw/
  ├── 00_generate_references.R   # Master generation script
  ├── REFERENCE_UPDATES.md       # Update log
  ├── usa_census/
  │   ├── 2020_census.csv
  │   └── process_census_2020.R
  └── japan_census/
      ├── 2020_census.csv
      └── process_japan_2020.R
```

---

## Integration Points

### With Existing Analysis Flow

```r
# Current flow:
res_list <- generateMultiplePrevalence(prevDefList, es)
exportPrevalenceResults(res_list$prevalence, es)

# With standardization:
res_list <- generateMultiplePrevalence(prevDefList, es)

japan_ref <- getStandardizationReference("japan_census_2020")
std_results <- standardizePrevalence(
  res_list$prevalence,
  referencePopulation = japan_ref,
  ageCol = "age",
  genderCol = "gender"
)

# Add std rates back to results
res_std <- res_list$prevalence |>
  dplyr::bind_cols(std_results)

exportPrevalenceResults(res_std, es)
```

### Test Script Integration
Updated `hidden/test_ckd_updated.R` to include standardization step

---

## Documentation

### R Documentation Files
- `R/StandardizationReference.R` - Class definition
- `R/standardizePrevalence.R` - Main function
- `R/standardizationHelpers.R` - Creation/import wrappers
- `R/standardizationData.R` - Data documentation

### Vignette
- `vignettes/standardization.Rmd`
  - Introduction to direct method standardization
  - When to use which reference population
  - Example: CKD prevalence with different standards
  - Comparison: crude vs standardized rates

### NEWS Entry
```
## CohortPrevalence 1.1.0

### New Features
- Direct method standardization via `standardizePrevalence()` function
- Built-in reference populations: USA Census 2020, Japan Census 2020, WHO World Standard
- `StandardizationReference` R6 class for encapsulating population references
- Helper functions for creating/importing custom reference populations

### Data
- New datasets: `usa_census_2020`, `japan_census_2020`, `who_world_standard`
```

---

## Dependencies

### Required
- dplyr
- tidyr
- tibble
- glue
- cli
- checkmate
- R6

### Optional (for ACS)
- tidycensus (only if user calls `fetchACSReference()`)

### No new hard dependencies needed

---

## Testing Strategy

### Unit Tests (`tests/testthat/`)

```r
test_that("StandardizationReference validates input data", {
  # Bad data missing columns
  # Bad data with NAs
})

test_that("standardizePrevalence calculates correct rates", {
  # Known example with manual calculation
})

test_that("Built-in references load correctly", {
  # japan_census_2020 exists & has right structure
})

test_that("getStandardizationReference() works", {
  # Load by name
  # Error on unknown name
})
```

### Integration Tests
- Test with `test_ckd_updated.R` output
- Standardized rates fall between crude rates and near-zero reference pops

---

## Maintenance Strategy

### Annual Updates (ACS)
- Run `cacheACSReference(year)` annually
- Commit to `data/usa_acs_{year}.rda`
- Update REFERENCE_UPDATES.md

### Decennial Updates (Census/Japan)
- Download new census files
- Run processing script
- Tag release with version bump (PATCH or MINOR)
- Deprecate old references per versioning policy

### Reference Update Log
File: `data-raw/REFERENCE_UPDATES.md`

```markdown
## 2026-03-31: Initial Release
- USA Census 2020 (v1.0)
- Japan Census 2020 (v1.0)
- WHO World Standard Population (v2008)

## 2027-03-15: ACS 2023 Addition
- Fetched via tidyCensus API
- 5-year age bands
```

---

## Migration Path

### For Existing Users
- Standardization is opt-in (backward compatible)
- Current workflows unaffected
- Existing exports still work

### For New Users
- Can standardize in post-processing step
- Documented in vignette
- Example in test scripts

---

## Success Criteria

- [x] StandardizationReference class fully implemented and tested (no syntax errors)
- [x] standardizePrevalence() function with demographic bounds and age truncation
- [x] Helper functions for reference management (create, import, get, list, fetch)
- [ ] All 3 Phase 1 references available (.rda files generated)
- [ ] CI calculation implemented (Wilson score exact method)
- [ ] Integration testing with sample data
- [ ] Package size < 2MB (reference data should be ~200-300 KB total)
- [ ] No regressions in existing tests

---

## Decisions Finalized ✓

| Decision | Outcome |
|----------|---------|
| **ACS Strategy** | Fetch on-demand via `fetchACSReference(year)` - smaller package, no runtime dependency |
| **CI Method** | Wilson score exact - better for sparse counts (e.g., rare diseases) |
| **Crude Rate Output** | Yes - included in standardizePrevalence() output for comparison |
| **Japan Age Grouping** | Single-year ages (0-102+) - full precision for standardization |
| **Deprecation Policy** | Remove old references after 2 major versions (5+ years) |
| **Phase 1 Scope** | Lock to 3 references: USA Census 2020 + Japan Census 2020 + WHO World Standard |
| **Demographic Features** | ✓ Both demographic bounds matching and age truncation include in MVP |

