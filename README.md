# CohortPrevalence

The goal of `CohortPrevalence` is to provide a standard prevalence calculation for data mapped to the OMOP CDM. 
This package relies on `DatabaseConnector` and `SqlRender` to perform its prevalence calculation. 

## Rassen Methodology

The methods for calculating prevalence are sourced from [Rassen et al 2018](https://pmc.ncbi.nlm.nih.gov/articles/PMC6301730/).
Users have the option of selecting:

- Period of Interest: the specific time for which prevalence is anchored for its enumeration. This is often one year but can also be defined as a span of multiple years.

- Lookback time: A defined span of time prior to the period of interest during which the database is queried for existing evidence of disease. In longitudinal observational databases, we are unable to dip into the data at a single point in time to determine whether a chronic condition is present. Instead, we define a period where we surveil for existing disease. If the chronic disease occurs during the lookback time, then it is considered to have prevalent disease.

- Denominator Option: 
    - Option 1: "day 1" population - the number of persons in the population who were observed on the first day of the period of interest
    - Option 2: "complete-period" population - the number of persons in the population who contribute all observable person-days in the period of interest. This is the strictest denominator
    - Option 3: "any-time" population -  the number of persons who who contributes at least 1 day in the period of interest. This is the most naive denominator and what IHD uses.
    - Option 4: "sufficient-time" population - the number of persons who contributes *sufficient* time in the period of interest based on at least *n* observable person-days in the period of interest

- Numerator Option:
    - Option 1: The number of patients who have been observed to have the condition of interest on the first day of the period of interest or within the lookback time
    - Option 2: The number of patients who have been observed to have the condition of interest at any time in the period of interest or within the lookback time.
    
- Defining Eligible Observation Periods in the population
    1) Minimum Observation Period Length: This is the required time that persons in the database must have been observed to be eligible. This can be any number in days; typical options would be 0 days or 365 days. 
    2) First or any observation period: Determine whether to use first observation period or any observation period to evaluate the prevalence of a disease during the period of interest. In claims data, it is possible for patients to leave the database and return. 

- Demographic Stratification: age, gender and race


## Key Features

- **Flexible Prevalence Definitions**: Multiple denominator and numerator options following Rassen et al. 2018 methodology
- **Age Standardization**: Standardize prevalence estimates using built-in or custom reference populations
- **Demographic Stratification**: Results stratified by age, gender, and race
- **Multi-Dimensional Experiment Design**: Define complex multi-analysis specifications with automatic Cartesian product expansion
- **Results Management**: Unified container with dual crude/standardized storage, SQL audit trail, and export/import
- **Interactive Results Explorer**: Shiny-based dashboard for visualizing trends across analyses
- **Multiple Output Types**: Simultaneous generation of prevalence, incidence, and experimental drug usage metrics
- **OMOP CDM Compliant**: Works with data mapped to the OMOP Common Data Model

---

## Usage

### Single Prevalence Analysis
```r
# Create analysis specification
analysis <- createCohortPrevalenceAnalysis(
  analysisId = 1L,
  prevalentCohort = createPopulationCohort(name = "CKD", isoPeriod = "2020-2024"),
  periodOfInterest = createYearlyRange(2020:2024),
  prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 0L),
  demographicConstraints = createDemographicConstraints(ageMin = 18, ageMax = 150),
  outputTypes = "prevalence"
)

# Execute analysis
results <- generatePrevalence(
  prevalenceAnalysisList = analysis,
  executionSettings = settings,
  captureSql = TRUE
)

# Explore results interactively
results$explore()  # Launches Shiny dashboard with tabs: Prevalence | Incidence | Drug Usage
# Note: explore() is not yet ready for use

# View SQL executed
results$show_query(analysisId = 1)

# Standardize prevalence
reference <- getStandardizationReference("WHO World Standard 2008")
results$standardizePrevalence(reference)

# Export results with manifest
results$export(outputFolder = "~/analysis_results")

```

### Multi-Dimensional Experiment Design

```r
# Define analysis dimensions
exp <- CohortPrevalenceExperiment$new(
  name = "CKD Multi-Cohort Study",
  description = "3 cohort definitions × 3 prevalence types × 2 demographics"
)

# Add dimensions (fluent API)
exp$addCohorts(tibble::tibble(
  cohortId = c(1, 2, 3),
  cohortName = c("CKD A", "CKD B", "CKD C")
))
exp$addPrevalenceTypes(list(
    createPrevalenceType("point_prevalence", lookBackDays = 0L),
    createPrevalenceType("period_prevalence_pd2", lookBackDays = 365L),
    createPrevalenceType("period_prevalence_pd3", lookBackDays = 0L)
  ))
exp$addDemographicConstraints(list(
    createDemographicConstraints(ageMin = 18, ageMax = 150),
    createDemographicConstraints(ageMin = 65, ageMax = 150)
  ))
exp$addPeriodsOfInterest(list(
    createYearlyRange(2015:2024)
  ))
exp$validate()

# Preview specification (3 × 3 × 2 × 10 = 180 analyses)
exp$viewDesign()        # Console table
exp$viewDesign("html")  # Reactable (searchable/filterable)

# Materialize analysis objects
analyses <- exp$define()

# Execute all analyses
results <- generatePrevalence(
  prevalenceAnalysisList = analyses,
  executionSettings = settings
)

# Dashboard shows all 180 analyses with filtering
# Note: explore() is not yet ready for use
results$explore()
```

### Multiple Output Types
```r
# ⚠️ Note: Drug output type is experimental - use with caution

# Define drug concept sets using Capr
drug_concepts <- list(
  Capr::readConceptSet("path/to/aspirin.json"),
  Capr::readConceptSet("path/to/statin.json")
)

# Include drug analysis
analysis <- createCohortPrevalenceAnalysis(
  analysisId = 1L,
  # ... other parameters ...
  outputTypes = c("prevalence", "incidence", "drugs"),
  drugConceptSets = drug_concepts  # Required if "drugs" in outputTypes
)

results <- generatePrevalence(analysis, executionSettings)

# Results now contain three data frames
results$prevalence    # Prevalence rates
results$incidence     # Incidence rates
results$drugUsage     # Drug usage patterns (experimental)

# Explore tab-based dashboard
# Note: explore() is not yet ready for use
results$explore()     # Tabs: Prevalence | Incidence | Drug Usage
```

## Understanding Results Output

The `PrevalenceResults` object consolidates all analysis output:

```r
# Raw data
results$prevalence      # Crude prevalence estimates
results$incidence       # Incidence rates (if requested)
results$drugUsage       # Drug usage metrics (if requested, experimental)
results$metaInfo        # Analysis metadata & configuration

# Standardized data
results$stdPrev         # Standardized prevalence (after standardizePrevalence())

# Query tracking
results$show_query(analysisId = 1)  # View SQL executed

# Provenance & export
results$export(outputFolder = "output")  # Creates manifest.json with SHA256 checksums
```

## Experimental Features

### Drug Usage Output Type
⚠️ **Experimental** - The `"drugs"` output type is currently experimental and should be used with caution. Results may be subject to future changes. Requires valid Capr ConceptSetItems via the `drugConceptSets` parameter.

### Results Explorer (In Development)
⚠️ **Not Yet Ready** - The interactive `results$explore()` Shiny dashboard is under development. This feature will provide tabbed visualization of prevalence, incidence, and drug usage results with interactive filtering and exploration capabilities. Check back in a future release.

## Composition & Best Practices

### Recommended Workflow
1. **Design**: Use `CohortPrevalenceExperiment` to define multi-dimensional specifications
2. **Validate**: Call `exp$viewDesign()` to inspect Cartesian product before execution
3. **Execute**: `generatePrevalence(exp$define(), ...)` runs all analyses with unified logging
4. **Explore**: `results$explore()` launches interactive dashboard for pattern discovery ⚠️ *Not yet ready*
5. **Standardize**: Apply `results$standardizePrevalence(reference)` to comparable estimates
6. **Export**: `results$export()` bundles all outputs with provenance manifest

### Performance Tips
- For large experiments (>100 analyses), run in stages with monitoring
- Use `captureSql = FALSE` to reduce memory footprint (disables query audit trail)
- Advanced: `explore()` UI filter available once feature is ready

## Installation

```r
remotes::install_github("ohdsi/CohortPrevalence")
```

### Requirements
- DatabaseConnector
- SqlRender

## Reference Populations for Standardization

The package includes pre-built reference populations for age standardization:

- **WHO World Standard 2008**: Official WHO population standard with 21 age groups (0-4, 5-9, ..., 100+)
  - Source: https://seer.cancer.gov/stdpopulations/world.who.html
  
- **USA Census 2020**: Decennial Census by single-year age and gender
  - Source: US Census Bureau (Census API via tidycensus)
  - See [data-raw/usa_census/README.md](data-raw/usa_census/README.md) for Census API setup
  
- **Japan Census 2020**: National Census by single-year age and gender
  - Source: Statistics Bureau of Japan (e-Stat)
  - See [data-raw/japan_census/README.md](data-raw/japan_census/README.md) for data processing details

## License

Apache License 2.0
