CohortPrevalence v1.1.0
=======================

## Feature Enhancements

* Improvements to standardization
    * Allow for standardization by other weighting schemes acs vs 2020 US census.
    * Automatically resolves differences in age groups between standardization references

## Breaking Changes

* **Occurrence Mode Removed**: Package now uses era pattern exclusively for all analyses. The `calculationMode` and `circeJsonPath` parameters have been removed from `createTargetCohort()` and `CohortPrevalenceExperiment$addCohorts()`. All SQL files with "_occurrence" pattern have been deleted.
  * Migration: Remove `calculationMode` and `circeJsonPath` arguments from any calls to `createTargetCohort()`
  * All analyses now use interval overlap (era) pattern by default
* **CIRCE JSON Validation Removed**: Cohort validation against CIRCE patterns is no longer performed. The `TargetCohort$validateCirceJson()` method has been removed.

## Method Changes

* add calcuation types of formal vs rough to determine is using either any occurrence of the disease or the formal start in the lookback period
    * `createPrevalenceType()` when mode = "rough" then counts based on the cohort_end_date
    * `createPrevalenceType()` when mode = "formal" then counts based on the cohort_start_date

## API Changes

* `CohortInfo` R6 class has been removed. It has been replaced by two purpose-specific classes:
    * `TargetCohort` — for the prevalence numerator and incidence target cohort.
    * `PopulationCohort` — for the denominator population cohort.
* `createPrevalenceCohort()` has been renamed to `createTargetCohort()`.
* `createPopulationCohort()` now constructs a `PopulationCohort` object instead of a `CohortInfo` object. The function signature is unchanged.
* `createTargetCohort()` signature simplified: `createTargetCohort(cohortId, cohortName)` (removed `calculationMode` and `circeJsonPath` parameters)
* `CohortPrevalenceExperiment$addCohorts()` tibble columns simplified: now requires only `cohortId` and `cohortName` (removed optional `calculationMode` and `circeJsonPath` columns)

CohortPrevalence v1.0.1
=======================

## Main Bug fix
- add R6 as import for package
- correct bugs in prevalence

CohortPrevalence v1.0.0
=======================

## Refactor of Sql 

* Cohort Types:
    * Era pattern: first occurrence end of continuous observation
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
