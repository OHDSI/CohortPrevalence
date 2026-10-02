validatePrevalenceOutputOptions <- function(outputTypes, strata) {
  checkmate::assert_character(outputTypes, min.len = 1, any.missing = FALSE)
  checkmate::assert_subset(
    outputTypes,
    choices = c("prevalence", "incidence", "drugs", "demographics")
  )

  if ("demographics" %in% outputTypes) {

    if (!("prevalence" %in% outputTypes)) {
      stop("The 'demographics' output type requires 'prevalence'.", call. = FALSE)
    }

    if (is.null(strata) || length(strata) == 0) {
      stop(
        "The 'demographics' output type requires at least one demographic stratum.",
        call. = FALSE
      )
    }

  }

  invisible(TRUE)
}

validateCommonPrevalenceOutputTypes <- function(prevalenceAnalysisList) {
  checkmate::assert_list(prevalenceAnalysisList, min.len = 1)

  for (analysis in prevalenceAnalysisList) {
    checkmate::assert_class(analysis, "CohortPrevalenceAnalysis")
  }

  referenceOutputTypes <- prevalenceAnalysisList[[1]]$outputTypes
  outputTypesMatch <- vapply(
    prevalenceAnalysisList,
    function(analysis) setequal(analysis$outputTypes, referenceOutputTypes),
    logical(1)
  )

  if (!all(outputTypesMatch)) {

    stop(
      "All analyses passed to generatePrevalence() must request the same outputTypes.",
      call. = FALSE
    )

  }

  referenceOutputTypes
}

bindDemographicsResults <- function(demographicsResultsList) {
  checkmate::assert_list(demographicsResultsList)
  availableResults <- Filter(Negate(is.null), demographicsResultsList)

  if (length(availableResults) == 0) {

    return(NULL)

  }

  for (result in availableResults) {
    checkmate::assert_data_frame(result)
  }

  dplyr::bind_rows(availableResults)
}


validateAgeGroups <- function(ageGroups) {
  if (is.null(ageGroups)) {
    return(invisible(TRUE))
  }

  checkmate::assert_list(ageGroups, min.len = 1)
  groupNames <- names(ageGroups)

  if (is.null(groupNames) || anyNA(groupNames) || any(!nzchar(groupNames))) {
    stop("'ageGroups' must be a named list of age ranges.", call. = FALSE)
  }

  if (anyDuplicated(groupNames)) {
    stop("Names in 'ageGroups' must be unique.", call. = FALSE)
  }

  if ("Other/Unmapped" %in% groupNames) {
    stop("'Other/Unmapped' is reserved for ages outside configured ranges.", call. = FALSE)
  }

  lowerBounds <- numeric(length(ageGroups))
  upperBounds <- numeric(length(ageGroups))

  for (i in seq_along(ageGroups)) {
    ageRange <- ageGroups[[i]]
    checkmate::assert_numeric(ageRange, len = 2, any.missing = FALSE, lower = 0)

    lower <- ageRange[[1]]
    upper <- ageRange[[2]]

    if (!is.finite(lower) || lower != floor(lower)) {
      stop("Each age-group lower bound must be a finite non-negative integer.", call. = FALSE)
    }

    validUpper <- (is.finite(upper) && upper == floor(upper)) || identical(upper, Inf)

    if (!validUpper) {
      stop("Each age-group upper bound must be an integer or Inf.", call. = FALSE)
    }

    if (lower > upper) {
      stop("Each age-group lower bound must be less than or equal to its upper bound.", call. = FALSE)
    }

    lowerBounds[[i]] <- lower
    upperBounds[[i]] <- upper
  }

  if (is.unsorted(lowerBounds, strictly = TRUE) || any(diff(lowerBounds) == 0)) {
    stop("Age groups must be ordered by increasing lower bound.", call. = FALSE)
  }

  if (length(ageGroups) > 1 && any(lowerBounds[-1] <= upperBounds[-length(upperBounds)])) {
    stop("Age-group ranges must not overlap.", call. = FALSE)
  }

  invisible(TRUE)
}


getDenomText <- function(denomType) {
  checkmate::assert_choice(x = denomType, choices = c("pd1", "pd2", "pd3", "pd4"))
  if (denomType == "pd1") {
    txt <- "- Day 1 population (PD1): the number of persons in the population who were observed on the first day of the period of interest"
  }
  if (denomType == "pd2") {
    txt <- "- Complete-period population (PD2): the number of persons in the population who contribute all observable person-days in the period of interest."
  }
  if (denomType == "pd3") {
    txt <- "- Any-time population (PD3): the number of persons who who contributes at least 1 day in the period of interest."
  }
  if (denomType == "pd4") {
    txt <- "- Sufficient-time population (PD4): the number of persons who contributes sufficient time in the period of interest based on at least n observable person-days in the period of interest"
  }
  return(txt)
}


getNumText <- function(numType) {
  checkmate::assert_choice(x = numType, choices = c("pn1", "pn2"))
  if (numType == "pn1") {
    txt <- "- Cases before POI (PN1): the number of patients who have been observed to have the condition of interest prior to the period of interest, within the lookback time"
  }
  if (numType == "pn2") {
    txt <- "- Cases before and during POI (PN2): the number of patients who have been observed to have the condition of interest either within the lookback time or during the period of interest."
  }
  return(txt)
}


# function to combine multiple Capr concept sets into one, with the same name
combineCaprCs <- function(csList,
                          csName) {
  csDf <- data.frame(conceptId = integer(),
                     conceptName = character(),
                     domainId = character(),
                     vocabularyId = character(),
                     standardConcept = character(),
                     includeDescendants = logical(),
                     isExcluded = logical(),
                     includeMapped = logical())
  for (i in csList) {
    cs <- Capr:::as.data.frame(i)
    csDf <- rbind(csDf, cs)
  }

  concepts <- c()
  desc <- c()
  excl <- c()
  excl_desc <- c()
  for (j in 1:nrow(csDf)) {
    if (csDf$includeDescendants[j] && !csDf$isExcluded[j]) {
      desc <- c(desc, csDf$conceptId[j])
    }
    if (csDf$isExcluded[j] && !csDf$includeDescendants[j]) {
      excl <- c(excl, csDf$conceptId[j])
    }
    if (csDf$isExcluded[j] && csDf$includeDescendants[j]) {
      excl_desc <- c(excl_desc, csDf$conceptId[j])
    }
    if (!csDf$isExcluded[j] && !csDf$includeDescendants[j]) {
      concepts <- c(concepts, csDf$conceptId[j])
    }
  }

  caprCs <- Capr::cs(
    unique(concepts),
    Capr::descendants(unique(desc)),
    Capr::exclude(unique(excl)),
    Capr::exclude(Capr::descendants(unique(excl_desc))),
    name = csName
  )
  return(caprCs)
}

# prepare druc concept sets for util query
prepDrugConceptSets <- function(drugConceptSets) {

  # prep cs in subGroups
  csList <- list()
  uniSubCat <- unique(drugConceptSets$subCategory)[!is.na(unique(drugConceptSets$subCategory))]
  for (subCat in uniSubCat) {
    subcatCs <- drugConceptSets |>
      dplyr::filter(subCategory == subCat)

    #import concepts as Capr
    covCs <- vector('list', length = nrow(subcatCs))
    for (i in seq_along(covCs)) {
      covCs[[i]] <- Capr::readConceptSet(
        path = subcatCs$path[i],
        name = subcatCs$label[i]
      )
    }

    comboCs <- combineCaprCs(covCs, csName = paste0("*", subCat))
    covCs <- append(covCs, comboCs)
    csList <- append(csList, covCs)
  }

  # prep those not in subgroups
  otherCs <- drugConceptSets |>
    dplyr::filter(is.na(subCategory))
  #import concepts as Capr
  covCs2 <- vector('list', length = nrow(otherCs))
  for (i in seq_along(covCs2)) {
    covCs2[[i]] <- Capr::readConceptSet(
      path = otherCs$path[i],
      name = otherCs$label[i]
    )
  }

  csList <- append(csList, covCs2)
  return(csList)

}
# Build prevalence aggregation SQL
buildPrevalenceAggSQL <- function(strata) {
  glue::glue(
    "-- Do prevalence
     IF OBJECT_ID('#prevalence', 'U') IS NOT NULL
       DROP TABLE #prevalence;

     CREATE TABLE #prevalence AS
     SELECT
       span_label{strata}
       ,SUM(case_event) AS numerator
       ,COUNT(DISTINCT subject_id) AS denominator
       ,(SUM(case_event) / COUNT(DISTINCT subject_id)) * @multiplier AS prevalence_rate
     FROM #allEvents
     GROUP BY span_label{strata};"
  )
}

# Build demographics summary SQL for only the requested strata.
buildDemographicsAggSQL <- function(strata) {
  checkmate::assert_subset(
    strata,
    choices = c("age", "gender", "race", "ethnicity"),
    empty.ok = FALSE
  )

  demographicCounts <- vapply(
    strata,
    function(stratum) {
      glue::glue(
        "SELECT span_label, '{stratum}' AS demographic,\n",
        "  CAST({stratum} AS VARCHAR(255)) AS demographic_value,\n",
        "  COUNT(DISTINCT subject_id) AS case_count\n",
        "FROM case_rows\n",
        "GROUP BY span_label, {stratum}"
      )
    },
    character(1)
  ) |>
    paste(collapse = "\n\n  UNION ALL\n\n")

  demographicsTemplate <- readr::read_file(
    fs::path_package(package = "CohortPrevalence", "sql/demographics.sql")
  )

  glue::glue(demographicsTemplate)
}

# Build incidence aggregation SQL
buildIncidenceAggSQL <- function(strata) {
  glue::glue(
    "-- Do incidence
     IF OBJECT_ID('#incidence', 'U') IS NOT NULL
       DROP TABLE #incidence;

     CREATE TABLE #incidence AS
     SELECT
       span_label{strata}
       ,SUM(inc_event) AS numerator
       ,SUM(time_at_risk) / 365.25 AS denominator
       ,(CAST(SUM(inc_event) AS FLOAT) / NULLIF(SUM(time_at_risk) / 365.25, 0)) * @multiplier AS incidence_rate
     FROM (
       SELECT *,
             CASE WHEN inc_event = 1 THEN DATEDIFF(day, calendar_start_date, cohort_start_date)
                 ELSE DATEDIFF(day, calendar_start_date, calendar_end_date) END AS time_at_risk
       FROM #denomInc
     )
     GROUP BY span_label{strata};"
  )
}


prepDrugConceptSetQuery <- function(drugConceptSets, executionSettings) {

  csDropSql <- "DROP TABLE IF EXISTS #Codeset;" |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  cs_query <- .bindCodesetQueries(drugConceptSets, codesetTable = "#Codeset") |>
      SqlRender::render(
        vocabulary_database_schema = executionSettings$cdmDatabaseSchema
      ) |>
      SqlRender::translate(
        targetDialect = executionSettings$getDbms(),
        tempEmulationSchema = executionSettings$tempEmulationSchema
      )

  drugSql <- readr::read_file(fs::path_package(package = "CohortPrevalence", "sql/drugCalendar.sql")) 
  finalSql <- c(csDropSql, cs_query, drugCountSql) |>
    glue::glue_collapse("\n\n")
  return(finalSql)
} 