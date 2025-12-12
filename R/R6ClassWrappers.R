#' Create a `CohortPrevalenceAnalysis` object
#'
#' Constructs an `CohortPrevalenceAnalysis` object with the specified settings.
#'
#' @param analysisId Unique integer analysisId to identify the analysis (required).
#' @param prevalentCohort A `PrevalenceCohort` object specifying the cohort of interest (required).
#' @param periodOfInterest A `PeriodOfInterest` object (required).
#' @param lookBackDays Integer used for specifying length of lookback (required).
#' @param numeratorType Character string specifying numerator type. Must be one of:
#' \itemize{
#'   \item `"pn1"`: Patients who have been observed to have the condition of interest on the first day of the period of interest or within the lookback time
#'   \item `"pn2"`: patients who have been observed to have the condition of interest at any time in the period of interest or within the lookback time
#' }
#' @param denominatorType A `DenominatorType` object (required).
#' @param minimumObservationLength: Integer specifying minimum observation length (optional).
#' @param useOnlyFirstObservationPeriod Logical: `TRUE` to restrict analysis to the first observation period (optional).
#' @param lookbackOptions A `LookBackOption` object (required).
#' @param multiplier Integer specifying prevalence multiplier (optional).
#' @param strata Character string. Must be one, or some of: `"age"`, `"gender"`, `"race"` (optional).
#' @param populationCohort A `CohortPopulation` object specifying the population of interest on which to compute prevalence.
#'
#' @return A `CohortPrevalenceAnalysis` R6 object.
#' @export
#'
createCohortPrevalenceAnalysis <- function(analysisId,
                                           prevalentCohort,
                                           periodOfInterest,
                                           lookBackOptions,
                                           numeratorType,
                                           denominatorType,
                                           minimumObservationLength = 0L,
                                           useOnlyFirstObservationPeriod = FALSE,
                                           multiplier = 100000L,
                                           strata = NULL,
                                           populationCohort = NULL){
  analysisDef <- CohortPrevalenceAnalysis$new(analysisId = analysisId,
                               prevalentCohort = prevalentCohort,
                               periodOfInterest = periodOfInterest,
                               lookBackOptions = lookBackOptions,
                               numeratorType = numeratorType,
                               denominatorType = denominatorType,
                               minimumObservationLength = minimumObservationLength,
                               useOnlyFirstObservationPeriod = useOnlyFirstObservationPeriod,
                               multiplier = multiplier,
                               strata = strata,
                               populationCohort = populationCohort)
  return(analysisDef)
}

#' Create a prevalence cohort `CohortInfo` object
#'
#' Constructs an `CohortInfo` object for target cohort of interest
#'
#' @param cohortId Integer: the cohort ID within the database results schema of interest.
#' @param cohortName Character string specifying a name for the cohort.
#' @return A `CohortInfo` R6 object.
#' @export
#'
createPrevalenceCohort <- function(cohortId, cohortName) {
  cohortId <- as.integer(cohortId)
  prevalenceCohort <- CohortInfo$new(id = cohortId, name = cohortName)
  return(prevalenceCohort)
}

#' Create a population cohort `CohortInfo` object
#'
#' Constructs an `CohortInfo` object for population of interest.
#'
#' @param cohortId Integer: the cohort ID within the database results schema of interest.
#' @param cohortName Character string specifying a name for the cohort.
#' @return A `CohortInfo` R6 object.
#' @export
#'
createPopulationCohort <- function(cohortId, cohortName) {
  populationCohort <- CohortInfo$new(id = cohortId, name = cohortName)
  return(populationCohort)
}

#' Create a `LookBackOptions` object
#'
#' Constructs an `LookBackOptions` object with the specified settings.
#'
#' @param lookBackDays An integer number of days for the lookback period.
#' @param useObservedTimeOnly Logical: `TRUE` restricts the lookback period to only using observed periods.
#' @return A `LookBackOptions` R6 object.
#' @export
#'
createLookBackOptions <- function(lookBackDays = 99999L, useObservedTimeOnly = FALSE) {
  lbo <- LookBackOptions$new(
    lookBackDays = lookBackDays,
    useObservedTimeOnly = useObservedTimeOnly
  )
  return(lbo)
}

#' Create a `PeriodOfInterest` object
#'
#' Constructs an `PeriodOfInterest` object for yearly prevalence analyses.
#'
#' @param range A numeric vector of years of interest.
#' @return A `PeriodOfInterest` R6 object.
#' @export
#'
createYearlyPrevalence <- function(range) {
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
createSpanPrevalence <- function(startDates, endDates) {
  spanLabel <- paste(startDates, "-", endDates)

  if(is.numeric(startDates)){
    startDates <- paste0(startDates,"-01-01") |>
      as.Date()
  }

  if(is.numeric(endDates)){
    endDates <- paste0(endDates,"-01-01") |>
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
