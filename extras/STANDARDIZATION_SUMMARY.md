# CohortPrevalence Standardization Feature - Implementation Summary

**Created:** March 31, 2026  
**Status:** Planning & Code Sketches Complete  
**Ready for:** Developer Review

---

## Overview

Comprehensive direct method standardization framework for CohortPrevalence package. Enables users to calculate age-sex standardized prevalence rates using multiple reference populations (US, Japan, WHO, EU, etc.).

---

## What's Implemented (Sketched)

### 1. Core R6 Class: `StandardizationReference`

**Purpose:** Encapsulate reference population data with weights and metadata

**Key Features:**
- Validates data structure (requires: age, gender, population columns)
- Auto-calculates weights (population / total population)
- Stores metadata: name, country, year, source, DOI
- Methods for viewing, aggregating, and accessing reference data
- Extensible for custom references

**Code Location:** `R/StandardizationReference.R`

---

### 2. Main Function: `standardizePrevalence()`

**Purpose:** Apply direct method standardization to crude prevalence data with demographic constraint matching and age truncation

**Input:**
- Age-sex stratified prevalence dataframe (numerator, denominator per strata)
- StandardizationReference object (weights)
- `ageMin`, `ageMax`: Demographic bounds from analysis (NEW)
- `ageRightTruncation`: Optional age masking threshold, e.g., 70 for Optum (NEW)

**Output:**
- Standardized rate
- 95% Confidence Interval (Wilson score exact method)
- Crude rate (for comparison)
- Reference population metadata (name, year)

**Algorithm:**
1. Filter reference population to demographic bounds (ageMin-ageMax)
2. If ageRightTruncation specified: collapse ages >= threshold into single group
3. Re-normalize weights within filtered/collapsed age groups
4. Join prevalence data to adjusted reference population weights
5. Calculate crude rate per age-sex stratum
6. Multiply each stratum's crude rate by reference weight
7. Sum weighted rates = standardized rate
8. Calculate 95% CI using Wilson score exact method

**Code Location:** `R/standardize.R`

**NEW Features:**
- **Demographic Bounds Matching:** Automatically restricts reference population to match analysis eligibility (e.g., age 18+ analysis)
- **Age Truncation Support:** Handles database-specific patterns like Optum's 70+ age masking

#### Feature 1: Demographic Bounds (`ageMin`, `ageMax`)
- Filters reference population to match analysis eligibility
- Re-normalizes weights within bounds
- Example: Analysis restricted to age 18+ → reference weights recalculated for adults only

#### Feature 2: Age Truncation (`ageRightTruncation`)
- Collapses ages >= threshold into single group
- Handles database-specific masking (Optum DOD: age capped at 70+)
- Example: `ageRightTruncation = 70` → combines ages 70, 71, ..., 100+ into "70+"
- Prevents bias when standardizing against masked data

**Real-World Example: Optum DOD**
```r
standardizePrevalence(
  res_list$prevalence,
  referencePopulation = japan_census_2020,
  ageMin = 0,                # No lower bound constraint
  ageMax = 120,              # Analysis maximum
  ageRightTruncation = 70    # Optum masks age at 70+
)
```

---

### 3. Reference Population Data (Phase 1)

#### USA Census 2020
- **Filename:** `data/usa_census_2020.rda`
- **Ages:** Single-year (0-102)
- **Structure:** Male/Female stratified
- **Note:** Optum-friendly (70+ available)
- **Source:** US Census Bureau

#### Japan Census 2020
- **Filename:** `data/japan_census_2020.rda`
- **Ages:** Single-year (0-102+) - full precision for standardization
- **Structure:** Male/Female stratified
- **Source:** Statistics Bureau of Japan (総務省統計局)
- **Processing:** Manual download → process_japan_2020.R → single-year age output
- **Note:** No age masking complexity (unlike Optum)

#### WHO World Standard
- **Filename:** `data/who_world_standard.rda`
- **Ages:** 7 age bands (classic 2008 standard)
- **Structure:** Combined (international standard)
- **Source:** World Health Organization

---

### 4. Helper Functions for Reference Management

#### `createStandardizationReference()`
- Create custom reference population from user data
- Input validation (columns, no NAs, data types)
- Returns StandardizationReference object

#### `importStandardizationReference()`
- Import reference from CSV file
- Wraps createStandardizationReference()

#### `getStandardizationReference(name)`
- Retrieve built-in reference by name
- Error handling for unknown references
- Example: `getStandardizationReference("japan_census_2020")`

#### `listStandardizationReferences()`
- View all available references
- Shows: name, country, year, status

#### `fetchACSReference(year)` [ACS-specific, on-demand]
- Fetch USA ACS data via tidyCensus API for specified year
- Downloads on first call, caches locally
- No auto-bundling (reduces package size)
- Example: `fetchACSReference(2020)` for ACS 2020 estimates
- Documents years available in REFERENCE_UPDATES.md

**Code Location:** `R/standardizationHelpers.R`

---

### 5. Data Organization & Versioning

#### In-Package Structure
```
data/
  ├── usa_census_2020.rda        (compiled)
  ├── japan_census_2020.rda      (compiled)
  └── who_world_standard.rda     (compiled)
```

#### Version Control
```
data-raw/
  ├── 00_generate_references.R        (master generation script)
  ├── REFERENCE_UPDATES.md            (update log)
  ├── usa_census/
  │   ├── process_census_2020.R
  │   └── 2020_census.csv
  └── japan_census/
      ├── process_japan_2020.R
      └── 2020_census.csv
```

#### Update Strategy
- **Annual (ACS):** Run `cacheACSReference()`, commit result
- **Decennial (Census/Japan):** Download new data, run processing script, tag release
- **Deprecation:** Keep old references, mark as deprecated, remove after 2 major versions

---

### 6. Integration with Existing Workflow

#### Current Flow
```r
res_list <- generateMultiplePrevalence(prevDefList, es)
exportPrevalenceResults(res_list$prevalence, es)
```

#### With Standardization
```r
res_list <- generateMultiplePrevalence(prevDefList, es)

# Load reference
japan_ref <- getStandardizationReference("japan_census_2020")

# Standardize
std_results <- standardizePrevalence(
  res_list$prevalence,
  referencePopulation = japan_ref,
  ageCol = "age",
  genderCol = "gender"
)

# Combine and export
res_with_std <- res_list$prevalence |>
  dplyr::bind_cols(
    std_results |> dplyr::select(standardized_rate, ci_lower, ci_upper)
  )

exportPrevalenceResults(res_with_std, es)
```

**Note:** Fully backward compatible - existing code unaffected

---

### 7. Documentation & Education

#### Vignette: `vignettes/standardization.Rmd`
-Introduction to direct method standardization
- When to use which reference population
- When NOT to standardize (small denominators, etc.)
- Example: CKD prevalence standardized vs crude
- Comparison table: different standards impact on point estimate
- Interpretation guidance

#### Function Documentation
- Roxygen2 docstrings for all functions
- Examples included in ?standardizePrevalence
- Cross-references to StandardizationReference class

#### Data Documentation
- `R/standardizationData.R` contains data descriptions
- Each .rda file documented with source, year, DOI

#### NEWS Entry
```
## CohortPrevalence 1.1.0

### New Features
- Direct method standardization via standardizePrevalence()
- Built-in reference populations: US Census 2020, Japan Census 2020, WHO World Standard
- StandardizationReference R6 class for custom references
- Helper functions: createStandardizationReference(), importStandardizationReference(), getStandardizationReference()
```

---

### 8. Testing Strategy

#### Unit Tests (`tests/testthat/test-standardization.R`)
- StandardizationReference validates input (missing columns, NAs)
- standardizePrevalence() calculates correct rates
  - Known test case with manual verification
  - Edge cases: sparse strata, missing age groups
- Built-in references load & have correct structure
- getStandardizationReference() returns correct object
- Error handling for invalid inputs

#### Integration Tests
- End-to-end: generate prevalence → standardize → export
- Known epidemiological examples validate results
- Crude rates fall within expected range relative to standardized

#### Regression Tests
- Existing tests still pass (no breakage to generateMultiplePrevalence)
- Backward compatibility verified

---

### 9. Japan Census 2020 Strategy

#### Data Acquisition
- **Source:** Statistics Bureau of Japan (総務省統計局)
- **URL:** https://www.stat.go.jp/data/kokusei/2020/
- **Format:** CSV download from official site
- **Approach:** Manual download Phase 1 (no API dependency)

#### Processing Workflow
```r
# data-raw/japan_census/process_japan_2020.R

japan_raw <- readr::read_csv("japan_2020_population.csv") |>
  dplyr::rename(age = 年齢, male = 男, female = 女) |>
  tidyr::pivot_longer(c(male, female), names_to = "gender") |>
  # ... standardization ...
  
japan_census_2020 <- StandardizationReference$new(...)
usethis::use_data(japan_census_2020, overwrite = TRUE)
```

#### Data Characteristics
- 5-year age bands (0-4, 5-9, ..., 100+)
- Male/Female categories
- High quality, official government source
- No privacy masking needed (unlike US)

#### Validation
- Total ≈ 125M (Japan population 2020)
- All age groups present
- No negative values
- Ascending age order

---

### 10. Architecture Benefits

#### Extensibility
- Users can add custom references via `createStandardizationReference()`
- New countries added by placing .rda in data/
- No code changes needed for new reference populations

#### Reproducibility
- Reference population version tied to package version
- Exact standard documented in results
- Can compare "standardized by WHO" vs "standardized by Japan Census"

#### Maintenance
- Single source of truth: data-raw/{country}/process_{year}.R
- Automated generation via master script
- Clear update log in REFERENCE_UPDATES.md

#### Backward Compatibility
- Opt-in feature - doesn't change existing workflows
- Old packages continue to work
- No new hard dependencies

---

## Phase Structure

### Phase 1 (Priority - This Sprint)
- ✓ StandardizationReference class (sketched)
- ✓ standardizePrevalence() function (sketched)
- ✓ USA Census 2020 (data ready)
- ✓ **Japan Census 2020** (NEW - strategy complete)
- ✓ WHO World Standard (sketched)
- [ ] Code review & refinement
- [ ] Tests & validation
- [ ] Documentation complete

### Phase 2 (Next Release)
- EU Standard 2013
- USA ACS 2022-2030 (annual updates)
- Canada Census 2021
- Germany 2011

### Phase 3 (Future)
- Regional WHO standards
- UK/Additional countries per demand
- Auto-update infrastructure if needed

---

## Decisions Finalized ✓

| Decision | Rationale |
|----------|-----------|
| **R6 Class Design** | Encapsulation, extensibility, validation |
| **Direct Method** | Standard in epidemiology, matches user workflow |
| **Phase 1 = USA + Japan + WHO** | US for local work, Japan for real-world need, WHO for international |
| **Wilson Score Exact CI** | Better for sparse counts (e.g., rare diseases); exact probability |  
| **ACS On-Demand** | No API runtime dependency, smaller package size; users fetch as needed |
| **Single-Year Japan Ages** | Full precision for standardization (not collapsed to 5-year bands) |
| **Deprecation Policy** | Remove old references after 2 major versions (5+ years) |
| **Opt-in Feature** | Backward compatible; doesn't break existing workflows |

---

## Advanced Features (NEW)

### Demographic Bounds Matching & Age Truncation

Beyond basic standardization, two advanced features handle real-world database patterns:

1. **Demographic Bounds (`ageMin`, `ageMax`)**: Restrict reference population to match analysis eligibility
   - Example: 18+ age-restricted analysis → reference weights recalculated for adults only  
   - Prevents bias from including ineligible age strata
   - Re-normalizes weights within bounds

2. **Age Truncation (`ageRightTruncation`)**: Collapse ages above threshold into single "masking group"
   - Solves Optum DOD masking (age capped at 70+)   
   - Example: `ageRightTruncation = 70` → combines 70, 71, ..., 100+ → "70+" group
   - Maintains statistical rigor when data structure differs from reference population

**Real-World Scenario:** Optum DOD study of frailty prevalence
```r
# Data restricted to ages 0-70+ (all ages 70+ collapsed)
# Reference population must match this structure

optum_std <- standardizePrevalence(
  prevalence_by_age,
  referencePopulation = japan_census_2020,
  ageMin = 0,
  ageMax = 120,
  ageRightTruncation = 70  # Collapse reference to match data masking
)
```

---

## Files & Locations

### Code Files (To Be Created)
```
R/
  ├── StandardizationReference.R          (~150 lines)
  ├── standardize.R                       (~100 lines)
  └── standardizationHelpers.R            (~200 lines)
```

### Data Files (To Be Created)
```
data/
  ├── usa_census_2020.rda
  ├── japan_census_2020.rda
  └── who_world_standard.rda
```

### Support Files (To Be Created)
```
data-raw/
  ├── 00_generate_references.R            (master script)
  ├── REFERENCE_UPDATES.md
  ├── usa_census/
  │   ├── process_census_2020.R
  │   └── 2020_census.csv
  └── japan_census/
      ├── process_japan_2020.R
      └── 2020_census.csv

vignettes/
  └── standardization.Rmd                 (~400 lines)

tests/testthat/
  └── test-standardization.R              (~200 lines)
```

### Documentation
```
docs/
  ├── STANDARDIZATION_IMPLEMENTATION_PLAN.md    (this doc structure)
  └── STANDARDIZATION_SUMMARY.md                (this file)
```

---

## Next Steps

1. **Code Review Meeting:** Review sketch code, architecture decisions, open questions
2. **Data Preparation:** Acquire & validate Japan Census 2020 data
3. **Implementation Sprint:**
   - Week 1: Create R6 class + unit tests
   - Week 2: Standardization function + validation
   - Week 3: Reference data prep + integration tests
   - Week 4: Vignette + docs + final review
4. **Testing & Validation:** Cross-check against published epidemiological examples
5. **Release:** Tag v1.1.0

---

## Success Criteria Checklist

- [ ] StandardizationReference fully tested
- [ ] standardizePrevalence() produces clinically sensible results
- [ ] All Phase 1 references available (USA, Japan, WHO)
- [ ] 95% CI calculations validated
- [ ] Vignette demonstrates clear use case
- [ ] Backward compatibility confirmed
- [ ] Package size < 100KB increase
- [ ] Zero regressions in existing tests
- [ ] Code review approved
- [ ] Documentation complete

---

## Resources & References

**Standardization Methods:**
- Epidemiology textbooks (Rothman, etc.) - direct method
- WHO guidance on standardization
- Published examples of standardized prevalence estimates

**Data Sources:**
- US Census Bureau: https://data.census.gov/
- Japan Statistics Bureau: https://www.stat.go.jp/data/kokusei/2020/
- WHO Standard: https://apps.who.int/iris/ (search "standard population")
- Eurostat: https://ec.europa.eu/eurostat/

---

**Document Version:** 1.0  
**Last Updated:** March 31, 2026  
**Prepared for:** Review by Project Lead

