#' Create a `PrevalenceType` object
#'
#' Constructs a `PrevalenceType` object specifying the prevalence type and lookback period.
#'
#' @param prevalenceType Character string specifying prevalence type. Must be one of:
#' \itemize{
#'   \item `"point_prevalence"`: Status on a specific day (pn1 + pd1)
#'   \item `"period_prevalence_pd2"`: All time observed during period (pn2 + pd2)
#'   \item `"period_prevalence_pd3"`: Continuous observation (pn2 + pd3)
#'   \item `"period_prevalence_pd4"`: Sufficient days observed (pn2 + pd4)
#' }
#' @param lookBackDays Integer number of days for lookback window. Must be a positive integer (>= 1) or Inf for complete historical lookback. Note: 0 is automatically coerced to Inf.
#' @return A `PrevalenceType` R6 object.
#' @export
#'
createPrevalenceType <- function(prevalenceType, lookBackDays) {
  pt <- PrevalenceType$new(
    prevalenceType = prevalenceType,
    lookBackDays = lookBackDays
  )
  return(pt)
}

#' Create a `CohortPrevalenceAnalysis` object
#'
#' Constructs an `CohortPrevalenceAnalysis` object with the specified settings.
#'
#' @param analysisId Unique integer analysisId to identify the analysis (required).
#' @param prevalentCohort A `TargetCohort` object specifying the cohort of interest (required).
#' @param periodOfInterest A `PeriodOfInterest` object (required).
#' @param prevalenceType A `PrevalenceType` object (required).
#' @param minimumObservationLength: Integer specifying minimum observation length (optional).
#' @param useOnlyFirstObservationPeriod Logical: `TRUE` to restrict analysis to the first observation period (optional).
#' @param multiplier Integer specifying prevalence multiplier (optional).
#' @param strata Character string. Must be one, or some of: `"age"`, `"gender"`, `"race"` (optional).
#' @param demographicConstraints a `DemoConstraint` object specifying the constraints of the population.
#' @param populationCohort A `PopulationCohort` object specifying the population of interest on which to compute prevalence.
#' @param outputTypes Character vector specifying which output types to generate. Defaults to `"prevalence"`. Can include `"incidence"` and/or `"drugs"` for simultaneous generation using shared base tables.
#'   **Warning**: The `"drugs"` output type is experimental and should be used with caution. Results may be subject to future changes.
#' @param drugConceptSets Optional list of Capr ConceptSetItems. Required if `"drugs"` is in `outputTypes`, otherwise ignored.
#'
#' @return A `CohortPrevalenceAnalysis` R6 object.
#' @export
#'
createCohortPrevalenceAnalysis <- function(analysisId,
                                           analysisTag = NULL,
                                           prevalentCohort,
                                           periodOfInterest,
                                           prevalenceType,
                                           minimumObservationLength = 0L,
                                           useOnlyFirstObservationPeriod = FALSE,
                                           multiplier = 100000L,
                                           strata = NULL,
                                           demographicConstraints = createDemographicConstraints(),
                                           populationCohort = NULL,
                                           outputTypes = "prevalence",
                                           drugConceptSets = NULL){
  if (is.null(analysisTag)) {
    analysisTag <- glue::glue("Analysis {analysisId} | {prevalentCohort$name()}")
  } else {
    analysisTag <- glue::glue("{analysisTag} | {prevalentCohort$name()}")
  }

  analysisDef <- CohortPrevalenceAnalysis$new(
    analysisId = analysisId,
    analysisTag = analysisTag,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType,
    minimumObservationLength = minimumObservationLength,
    useOnlyFirstObservationPeriod = useOnlyFirstObservationPeriod,
    multiplier = multiplier,
    strata = strata,
    demographicConstraints = demographicConstraints,
    populationCohort = populationCohort,
    outputTypes = outputTypes,
    drugConceptSets = drugConceptSets
  )
  return(analysisDef)
}

#' Create a `IncidenceAnalysis` object for Rassen Incidence
#'
#' Constructs an `IncidenceAnalysis` object with the specified settings.
#'
#' @param analysisId Unique integer analysisId to identify the analysis (required).
#' @param targetCohort A `TargetCohort` object specifying the cohort of interest (required).
#' @param periodOfInterest A `PeriodOfInterest` object (required).
#' @param minimumObservationLength: Integer specifying minimum observation length (optional).
#' @param useOnlyFirstObservationPeriod Logical: `TRUE` to restrict analysis to the first observation period (optional).
#' @param multiplier Integer specifying prevalence multiplier (optional).
#' @param strata Character string. Must be one, or some of: `"age"`, `"gender"`, `"race"` (optional).
#' @param demographicConstraints a `DemoConstraint` object specifying the constraints of the population.
#' @param populationCohort A `CohortPopulation` object specifying the population of interest on which to compute prevalence.
#'
#' @return A `IncidenceAnalysis` R6 object.
#' @export
#'
createRassenIncidenceAnalysis <- function(analysisId,
                                          targetCohort,
                                          periodOfInterest,
                                          minimumObservationLength = 0L,
                                          useOnlyFirstObservationPeriod = FALSE,
                                          multiplier = 100000L,
                                          strata = NULL,
                                          demographicConstraints = createDemographicConstraints(),
                                          populationCohort = NULL){

  analysisDef <- IncidenceAnalysis$new(
    analysisId = analysisId,
    targetCohort = targetCohort,
    periodOfInterest = periodOfInterest,
    minimumObservationLength = minimumObservationLength,
    useOnlyFirstObservationPeriod = useOnlyFirstObservationPeriod,
    multiplier = multiplier,
    strata = strata,
    demographicConstraints = demographicConstraints,
    populationCohort = populationCohort
  )
  return(analysisDef)
}



#' Create a target cohort `TargetCohort` object
#'
#' Constructs a `TargetCohort` object for use as the prevalence numerator or incidence target cohort.
#' All analyses use the era pattern (interval overlap).
#'
#' @param cohortId Integer: the cohort ID within the database results schema of interest.
#' @param cohortName Character string specifying a name for the cohort.
#'
#' @return A `TargetCohort` R6 object.
#' @export
#'
createTargetCohort <- function(cohortId, cohortName) {
  cohortId <- as.integer(cohortId)
  targetCohort <- TargetCohort$new(id = cohortId, name = cohortName)
  return(targetCohort)
}


#' Create a population cohort `PopulationCohort` object
#'
#' Constructs a `PopulationCohort` object for the denominator population cohort.
#' Population cohorts do not require a CIRCE JSON file or calculation mode.
#'
#' @param cohortId Integer: the cohort ID within the database results schema of interest.
#' @param cohortName Character string specifying a name for the cohort.
#'
#' @return A `PopulationCohort` R6 object.
#' @export
#'
createPopulationCohort <- function(cohortId, cohortName) {
  cohortId <- as.integer(cohortId)
  populationCohort <- PopulationCohort$new(id = cohortId, name = cohortName)
  return(populationCohort)
}



#' Create a `PeriodOfInterest` object
#'
#' Constructs an `PeriodOfInterest` object for yearly prevalence analyses.
#'
#' @param range A numeric vector of years of interest.
#' @return A `PeriodOfInterest` R6 object.
#' @export
#'
createYearlyRange <- function(range) {
  poi <- PeriodOfInterest$new(
    poiType = "yearly",
    poiRange = range
  )
  return(poi)
}


#' Create a `PeriodOfInterest` object
#'
#' Constructs an `PeriodOfInterest` object for span prevalence analyses.
#'
#' @param startYears A numeric vector of start years of interest.
#' @param endYears A numeric vector of end years of interest.
#' @return A `PeriodOfInterest` R6 object.
#' @export
#'
createSpan <- function(startDates, endDates) {
  spanLabel <- paste(startDates, "-", endDates)

  if(is.numeric(startDates)){
    startDates <- paste0(startDates,"-01-01") |>
      as.Date()
  }

  if(is.numeric(endDates)){
    endDates <- paste0(endDates,"-12-31") |>
      as.Date()
  }

  range <- data.frame(calendar_start_date = startDates,
                      calendar_end_date = endDates,
                      span_label = spanLabel)
  poi <- PeriodOfInterest$new(
    poiType = "span",
    poiRange = range
  )
  return(poi)
}


#' Create a `DemoConstraint` object
#'
#' Constructs an `DemoConstraint` object for prevalence analyses.
#'
#' @param ageMin The minimum age allowed for the population. Default is 0
#' @param ageMax the maximum age allowed for the population. Default is 150
#' @param genderIds the genderIds allowed. Default is 8507 - M, and 8532 - F
#' @return A `DemoConstraint` R6 object.
#' @export
createDemographicConstraints <- function(ageMin = 0, ageMax = 150, genderIds = c(8507, 8532)) {
  dc <- DemoConstraint$new(
    ageMin = ageMin,
    ageMax = ageMax,
    genderIds = genderIds
  )
  return(dc)
}

#' Create a `DenominatorType` object
#'
#' Constructs an `DenominatorType` object for denominator choice.
#'
#' @param denomType Character string specifying denominator type. Must be one of:
#' \itemize{
#'   \item `"pd1"`: Patients who have been observed on the first day of the period of interest
#'   \item `"pd2"`: Patients who contribute all observable person-days in the period of interest.
#'   \item `"pd3"`: Patients who contribute at least 1 day in the period of interest.
#'   \item `"pd4"`: Patients who contribute sufficient time in the period of interest based on at least n observable person-days in the period of interest.
#' }
#' @param sufficientDays Integer: For denominator choice `"pd4"`, the number of minimum observable days patients must be observed.
#' @return A `DenominatorType` R6 object.
#' @export
#'
createDenominatorType <- function(denomType, sufficientDays = NULL) {
  dt <- DenominatorType$new(
    denomType = denomType,
    sufficientDays = sufficientDays
  )
  return(dt)
}
