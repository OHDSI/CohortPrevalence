CohortPrevalence v1.0.0
=======================

## Refactor of Sql 

* Cohort Types:
    * Allow for two cohorts (Era: first occurrence end of continuous observation; Occurrence: all occurrences end on start date)
    * Validation of circe inputs 
    * apply different sql logic based on cohort type
    * requires circeJsonPath for validation
* Refine Prevalence type options
    * point_prevalence --> pn1 and pd1 with a lookback period
    * period_prevalence_pd2 --> pn2 and pd2 with all time in poi
    * period_prevalence_pd3 --> pn2 and pd3 at least 1 day in poi
    * period_prevalence_pd4 --> pn2 and pd4 at least n days in poi

## Major New Features

* **PrevalenceResults Class** 
  * Unified results container with dual crude/standardized storage
  * Export/import with manifest.json tracking
  * SQL query audit trail (SHA256 checksums)
  * `.addExecutedQuery()` for provenance tracking

* **Standardization Integration**
  * Add stored reference data for census, who and japanese population for standardization
  * functions to build StandardizationReference for ACS on the fly
  * `standardizePrevalence()` method (modifies in-place, returns self)
  * Separate `stdPrev` slot preserves crude prevalence
  * Calls internal `standardize_prevalence()` function (snake_case)

* **CohortPrevalenceExperiment Class**
  * Multi-dimensional specification builder (fluent builder pattern)
  * Cartesian product expansion: cohorts × types × constraints × periods of interest
  * Expansion via `tidyr::expand_grid()` with automatic analysisId generation
  * `define()` materializes analysis objects from specifications
  * `viewDesign()` displays specification (console text or reactable HTML)
  * CSV-exportable specification for reproducibility and version control
  * Full validation gates at each step (fail-fast)

* **Drug Output Type Support**
  * `drugConceptSets` slot added to CohortPrevalenceAnalysis
  * Validation: required if "drugs" in outputTypes, otherwise ignored
  * Each item validated as Capr ConceptSetItem with @id and @Name slots
  * **Marked experimental** with warning in documentation: "Use with caution"

* **Unified API Simplification**
  * `generatePrevalence()` accepts only `prevalenceAnalysisList` (single input)
  * Composition pattern: `exp$define()` → `generatePrevalence()`
  * Removed dual-input routing logic (cleaner separation of concerns)

* **New Dependencies**
  * **Imports**: rlang, lubridate, digest, jsonlite, checkmate
  * **Suggests**: shiny, plotly, reactable


## Deprecated Features

* All: `generateSinglePrevalence()`, `generateMultiplePrevalence()`, `runPrevalence()` (removed in prior session)


CohortPrevalence v0.1.0
=======================
* bug fix pn2 uses events in the current poi. ensure pn1 only uses prior events
* change sql render for pn1

CohortPrevalence v0.0.4
=======================
* bug fix span results - see #11
* bug fix pd2 

CohortPrevalence v0.0.3
=======================
* add demographic constraint to population: limit by age range or specific gender
* fix pd4 bug
* fix prevalence results collect bug
* add rassen incidence calculator
* add meta info to results output

CohortPrevalence v0.0.2
=======================
* add ability to specify a multi year span as an analysis: ie 2020-2024
* add ability to specify demographic strata: none, age, gender, race
* Now able to execute multiple prevalence analyses in sequence...run different specs of prevAnalysisObj
* updates to docs, vignettes and minor bug fixes


CohortPrevalence v0.0.1
=======================

* Initial Release
* Single Yearly Prevalence functionality following Rassen et al 2018
