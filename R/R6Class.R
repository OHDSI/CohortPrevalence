# Main class -------------------

## CohortPrevalenceAnalysis ----------------
CohortPrevalenceAnalysis <- R6::R6Class(
  classname = "CohortPrevalenceAnalysis",
  public = list(
    initialize = function(analysisId,
                          analysisTag,
                          prevalentCohort,
                          periodOfInterest,
                          prevalenceType,
                          minimumObservationLength = 0L,
                          useOnlyFirstObservationPeriod = FALSE,
                          multiplier = 100000,
                          strata = NULL,
                          demographicConstraints,
                          populationCohort = NULL,
                          outputTypes = "prevalence") {
      # set analysisId
      checkmate::assert_integerish(x = analysisId, len = 1)
      private[[".analysisId"]] <- analysisId

      # set analysisTag
      checkmate::assert_string(x = analysisTag)
      private[[".analysisTag"]] <- analysisTag

      # set prevalent cohort
      checkmate::assert_class(x = prevalentCohort, classes = "CohortInfo")
      private[[".prevalentCohort"]] <- prevalentCohort

      # set periodOfInterest
      checkmate::assert_class(x = periodOfInterest, classes = "PeriodOfInterest")
      private[[".periodOfInterest"]] <- periodOfInterest

      # set prevalenceType
      checkmate::assert_class(x = prevalenceType, classes = "PrevalenceType")
      private[[".prevalenceType"]] <- prevalenceType


      # set minimumObservationLength
      checkmate::assert_integerish(x = minimumObservationLength, len = 1)
      private[[".minimumObservationLength"]] <- minimumObservationLength

      # set useOnlyFirstObservationPeriod
      checkmate::assert_logical(x = useOnlyFirstObservationPeriod, len = 1)
      private[[".useOnlyFirstObservationPeriod"]] <- useOnlyFirstObservationPeriod

      # set multiplier
      checkmate::assert_integerish(x = multiplier, len = 1)
      private[[".multiplier"]] <- multiplier

      # set strata
      checkmate::assert_subset(x = strata, choices = c("age", "gender", "race"), empty.ok = TRUE)
      private[[".strata"]] <- strata

      # set demographic constraints
      checkmate::assert_class(x = demographicConstraints, classes = "DemoConstraint")
      private[[".demographicConstraints"]] <- demographicConstraints

      # set population
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo", null.ok = TRUE)
      private[[".populationCohort"]] <- populationCohort

      # set outputTypes (defaults to "prevalence", can include "incidence" and/or "drugs")
      checkmate::assert_character(x = outputTypes, min.len = 1)
      checkmate::assert_subset(x = outputTypes, choices = c("prevalence", "incidence", "drugs"))
      private[[".outputTypes"]] <- outputTypes

    },

    assembleSql = function(executionSettings) {

      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      # Step 0: if yearly prev than add the year ranges
      if (self$periodOfInterest$poiType == "yearly") {
        # insert range of years as a table
        years <- tibble::tibble(
          span_label = self$periodOfInterest$poiRange,
          calendar_start_date = self$periodOfInterest$poiRange,
          calendar_end_date = self$periodOfInterest$poiRange + 1
        ) |>
          dplyr::mutate(
            calendar_start_date = as.Date(paste0(calendar_start_date, "-01-01")),
            calendar_end_date = as.Date(paste0(calendar_end_date, "-01-01"))
          )
      } else {
        years <- self$periodOfInterest$poiRange
      }
        yearRangeSql <- .insertTableSql(
          executionSettings,
          tableName = "#year_interval",
          data = years
        )
        # get the obs pop year sql
        ageMin <- self$demographicConstraints$ageMin
        ageMax <- self$demographicConstraints$ageMax
        genderIds <- self$demographicConstraints$genderIds |> paste(collapse = ", ")
        obsPopYearSql <- readr::read_file(
          fs::path_package(package = "CohortPrevalence", "sql/obsPopYear.sql")
        ) |>
          glue::glue()


      # Step 1: Get the appropriate sql files

      # get the ObsPop -- this filters to eligible obs periods
      obsPopSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", "sql/obsPop.sql")
      )

      # deal with demo strata
      if (!is.null(self$strata)) {
        strata <- c(",", paste0(self$strata, collapse = ", ")) |> paste0(collapse = "")
      } else {
        strata <- ""
      }

      # get the denom file based on denominator type and cohort pattern
      denomType <- self$prevalenceType$getDenominatorType()
      cohortPattern <- self$prevalentCohort$getCircePattern()
      
      # Start with base SQL (always needed)
      sqlComponents <- c(yearRangeSql, obsPopSql, obsPopYearSql)
      
      # Add output-specific SQL based on outputTypes
      if ("prevalence" %in% private$.outputTypes) {
        denomSql <- readr::read_file(
          fs::path_package(package = "CohortPrevalence", glue::glue("sql/{denomType}_{cohortPattern}.sql"))
        ) |>
          glue::glue()
        
        prevSql <- buildPrevalenceAggSQL(strata)
        
        sqlComponents <- c(sqlComponents, denomSql, prevSql)
      }
      
      if ("incidence" %in% private$.outputTypes) {
        incDenomSql <- readr::read_file(
          fs::path_package(package = "CohortPrevalence", "sql/incDenom.sql")
        ) |>
          glue::glue()
        
        incSql <- buildIncidenceAggSQL(strata)
        
        sqlComponents <- c(sqlComponents, incDenomSql, incSql)
      }
      
      if ("drugs" %in% private$.outputTypes) {
        drugSql <- readr::read_file(
          fs::path_package(package = "CohortPrevalence", "sql/drugCalendar.sql")
        ) |>
          glue::glue()
        
        sqlComponents <- c(sqlComponents, drugSql)
      }

      allSql <- sqlComponents |>
        glue::glue_collapse("\n\n")

      return(allSql)

    },

    renderAssembledSql = function(sql, executionSettings){

      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      renderedSql <- SqlRender::render(
        sql,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        min_obs_time = self$minimumObservationLength,
        use_first_op = self$useOnlyFirstObservationPeriod,
        cohort_database_schema = executionSettings$workDatabaseSchema,
        cohort_table = executionSettings$cohortTable,
        prevalent_cohort_id = self$prevalentCohort$id(),
        lookback = self$prevalenceType$lookBackDays,
        multiplier = self$multiplier
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
      return(renderedSql)
    },

    viewAnalysisInfo = function() {
      txt <- c(
        glue::glue("Analysis Tag: {self$analysisTag}"),
        glue::glue("Prevalent Cohort ==> {self$prevalentCohort$viewCohortInfo()}"),
        self$periodOfInterest$viewPeriodOfInterest(),
        self$prevalenceType$viewPrevalenceType(),
        glue::glue("Output Types ==> {paste0(private$.outputTypes, collapse = ', ')}"),
        glue::glue("Observation Period Eligibility ==> Min Observation Period Length: {self$minimumObservationLength} | Using First Observation Period: {self$useOnlyFirstObservationPeriod}"),
        glue::glue("Strata ==> {paste0(self$strata, collapse = ', ')}"),
        glue::glue("Demographic Constraint ==> ageRange: {self$demographicConstraints$ageMin} - {self$demographicConstraints$ageMax}; genderIds: {paste0(self$demographicConstraints$genderIds, collapse =', ')}")
      ) |>
        glue::glue_collapse("\n\n")
      cli::cat_line(txt)
      invisible(txt)
    },

    tabulateAnalysisSettings = function() {

      # prep poi
      if (self$periodOfInterest$poiType == "yearly") {
        a <- min(self$periodOfInterest$poiRange)
        b <- max(self$periodOfInterest$poiRange)
        poi <- glue::glue("{self$periodOfInterest$poiType}: {a}-{b}")
      } else {
        poi <- glue::glue("{self$periodOfInterest$poiType}: {self$periodOfInterest$poiRange$span_label}")
      }

      # prep denom
      denomType <- self$prevalenceType$getDenominatorType()
      dn <- denomType

      # prep op
      ope <- glue::glue("obsLength: {self$minimumObservationLength} | firstObs: {self$useOnlyFirstObservationPeriod}")
      demoConAge <- glue::glue("{self$demographicConstraints$ageMin} - {self$demographicConstraints$ageMax}")
      demoConGender <- glue::glue("{paste0(self$demographicConstraints$genderIds, collapse =', ')}")
      
      # prep outputTypes
      outputs <- paste0(private$.outputTypes, collapse = ", ")

      tb <- tibble::tibble(
        analysisId = self$analysisId,
        analysisTag = self$analysisTag,
        cohortId = self$prevalentCohort$id(),
        cohortName = self$prevalentCohort$name(),
        poi = poi,
        prevalenceType = self$prevalenceType$getPrevalenceLabel(),
        lookBackDays = glue::glue("{self$prevalenceType$lookBackDays}d"),
        numerator = self$prevalenceType$getNumeratorType(),
        denominator = dn,
        obsPeriod = ope,
        outputTypes = outputs,
        demoConAge = demoConAge,
        demoConGender = demoConGender
      )

      return(tb)
    },

    collectResults = function(connection, executionSettings) {
      
      checkmate::assert_class(connection, classes = "DatabaseConnectorConnection")
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings")
      
      resultList <- list()
      metaInfo <- self$tabulateAnalysisSettings()
      
      # Collect prevalence results
      if ("prevalence" %in% private$.outputTypes) {
        prevResults <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = "SELECT * FROM #prevalence;",
          tempEmulationSchema = executionSettings$tempEmulationSchema,
          snakeCaseToCamelCase = TRUE
        ) |>
          dplyr::arrange(.data$spanLabel) |>
          dplyr::mutate(
            databaseId = executionSettings$cdmSourceName,
            statType = "Prevalence",
            .before = 1
          ) |>
          dplyr::inner_join(
            metaInfo, by = c("cohortDefinitionId" = "cohortId")
          )
        resultList$prevalence <- prevResults
      }
      
      # Collect incidence results  
      if ("incidence" %in% private$.outputTypes) {
        incResults <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = "SELECT * FROM #incidence;",
          tempEmulationSchema = executionSettings$tempEmulationSchema,
          snakeCaseToCamelCase = TRUE
        ) |>
          dplyr::arrange(.data$spanLabel) |>
          dplyr::mutate(
            databaseId = executionSettings$cdmSourceName,
            statType = "Incidence Rate",
            .before = 1
          ) |>
          dplyr::inner_join(
            metaInfo, by = c("cohortDefinitionId" = "cohortId")
          )
        resultList$incidence <- incResults
      }
      
      # Collect drug usage results
      if ("drugs" %in% private$.outputTypes) {
        drugResults <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = "SELECT * FROM #drug_cal_res;",
          tempEmulationSchema = executionSettings$tempEmulationSchema,
          snakeCaseToCamelCase = TRUE
        ) |>
          dplyr::arrange(.data$spanLabel) |>
          dplyr::mutate(
            databaseId = executionSettings$cdmSourceName,
            statType = "Drug Usage",
            .before = 1
          ) |>
          dplyr::inner_join(
            metaInfo, by = c("targetId" = "cohortId")
          )
        resultList$drugUsage <- drugResults
      }
      
      return(resultList)
    }

  ),
  private = list(
    # formalize vars
    .analysisId = NULL,
    .analysisTag = NULL,
    .prevalentCohort = NULL,
    .periodOfInterest = NULL,
    .prevalenceType = NULL,
    .minimumObservationLength = NULL,
    .useOnlyFirstObservationPeriod = NULL,
    .multiplier = NULL,
    .strata = NULL,
    .demographicConstraints = NULL,
    .populationCohort = NULL,
    .outputTypes = NULL
  ),
  active = list(
    # active fields for R6 class

    analysisId = function(value) {
      if (missing(value)) {
        return(private$.analysisId)
      }
      checkmate::assert_integerish(x = analysisId, len = 1)
      private$.analysisId <- value
    },

    analysisTag = function(value) {
      if (missing(value)) {
        return(private$.analysisTag)
      }
      checkmate::assert_string(x = analysisTag)
      private$.analysisTag <- value
    },

    prevalentCohort = function(value) {
      if (missing(value)) {
        return(private$.prevalentCohort)
      }
      checkmate::assert_class(x = prevalentCohort, classes = "CohortInfo")
      private$.prevalentCohort <- value
    },

    periodOfInterest = function(value) {
      if (missing(value)) {
        return(private$.periodOfInterest)
      }
      checkmate::assert_class(x = periodOfInterest, classes = "PeriodOfInterest")
      private$.periodOfInterest <- value
    },

    prevalenceType = function(value) {
      if (missing(value)) {
        return(private$.prevalenceType)
      }
      checkmate::assert_class(x = prevalenceType, classes = "PrevalenceType")
      private$.prevalenceType <- value
    },

    minimumObservationLength = function(value) {
      if (missing(value)) {
        return(private$.minimumObservationLength)
      }
      checkmate::assert_integerish(x = minimumObservationLength, len = 1)
      private$.minimumObservationLength <- value
    },

    useOnlyFirstObservationPeriod = function(value) {
      if (missing(value)) {
        return(private$.useOnlyFirstObservationPeriod)
      }
      checkmate::assert_logical(x = useOnlyFirstObservationPeriod , len = 1)
      private$.useOnlyFirstObservationPeriod <- value
    },

    strata = function(value) {
      if (missing(value)) {
        return(private$.strata)
      }
      checkmate::assert_subset(x = strata, choices = c("age", "gender", "race"), empty.ok = TRUE)
      private$.strata <- value

    },

    multiplier = function(value) {
      if (missing(value)) {
        return(private$.multiplier)
      }
      checkmate::assert_integerish(x = multiplier, len = 1)
      private$.multiplier <- value
    },

    populationCohort = function(value) {
      if (missing(value)) {
        return(private$.populationCohort)
      }
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo", null.ok = TRUE)
      private$.populationCohort <- value
    },


    demographicConstraints = function(value) {
      if (missing(value)) {
        return(private$.demographicConstraints)
      }
      checkmate::assert_class(x = demographicConstraints, classes = "DemoConstraint")
      private$.demographicConstraints <- value
    }

  )
)
## Incidence Analysis Class -----------------

IncidenceAnalysis <- R6::R6Class(
  classname = "IncidenceAnalysis",
  public = list(
    initialize = function(analysisId,
                          targetCohort,
                          periodOfInterest,
                          minimumObservationLength = 0L,
                          useOnlyFirstObservationPeriod = FALSE,
                          multiplier = 100000,
                          strata = NULL,
                          demographicConstraints,
                          populationCohort = NULL) {

      # set analysisId
      checkmate::assert_integerish(x = analysisId, len = 1)
      private[[".analysisId"]] <- analysisId

      # set prevalent cohort
      checkmate::assert_class(x = targetCohort, classes = "CohortInfo")
      private[[".targetCohort"]] <- targetCohort

      # set periodOfInterst
      checkmate::assert_class(x = periodOfInterest, classes = "PeriodOfInterest")
      private[[".periodOfInterest"]] <- periodOfInterest

      # set minimumObservationLength
      checkmate::assert_integerish(x = minimumObservationLength, len = 1)
      private[[".minimumObservationLength"]] <- minimumObservationLength

      # set useOnlyFirstObservationPeriod
      checkmate::assert_logical(x = useOnlyFirstObservationPeriod, len = 1)
      private[[".useOnlyFirstObservationPeriod"]] <- useOnlyFirstObservationPeriod

      # set multiplier
      checkmate::assert_integerish(x = multiplier, len = 1)
      private[[".multiplier"]] <- multiplier

      # set strata
      checkmate::assert_subset(x = strata, choices = c("age", "gender", "race"), empty.ok = TRUE)
      private[[".strata"]] <- strata

      # set demographic constraints
      checkmate::assert_class(x = demographicConstraints, classes = "DemoConstraint")
      private[[".demographicConstraints"]] <- demographicConstraints

      # set population
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo", null.ok = TRUE)
      private[[".populationCohort"]] <- populationCohort

    },

    assembleSql = function(executionSettings) {

      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      # Step 0: if yearly prev than add the year ranges
      if (self$periodOfInterest$poiType == "yearly") {
        # insert range of years as a table
        years <- tibble::tibble(
          span_label = self$periodOfInterest$poiRange,
          calendar_start_date = self$periodOfInterest$poiRange,
          calendar_end_date = self$periodOfInterest$poiRange + 1
        ) |>
          dplyr::mutate(
            calendar_start_date = as.Date(paste0(calendar_start_date, "-01-01")),
            calendar_end_date = as.Date(paste0(calendar_end_date, "-01-01"))
          )
      } else {
        years <- self$periodOfInterest$poiRange
      }
      yearRangeSql <- .insertTableSql(
        executionSettings,
        tableName = "#year_interval",
        data = years
      )
      # get the obs pop year sql
      ageMin <- self$demographicConstraints$ageMin
      ageMax <- self$demographicConstraints$ageMax
      genderIds <- self$demographicConstraints$genderIds |> paste(collapse = ", ")
      obsPopYearSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", "sql/obsPopYear.sql")
      ) |>
        glue::glue()


      # Step 1: Get the appropriate sql files

      # get the ObsPop -- this filters to eligible obs periods
      obsPopSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", "sql/obsPop.sql")
      )

      # deal with demo strata
      if (!is.null(self$strata)) {
        strata <- c(",", paste0(self$strata, collapse = ", ")) |> paste0(collapse = "")
      } else {
        strata <- ""
      }

      # get inc Denom
      incDenomSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/incDenom.sql"))
      ) |>
        glue::glue()

      # get the doPrev file
      incSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/doIncidence.sql"))
      ) |>
        glue::glue()

      allSql <- c(yearRangeSql, obsPopSql, obsPopYearSql, incDenomSql, incSql) |>
        glue::glue_collapse("\n\n")

      return(allSql)

    },

    renderAssembledSql = function(sql, executionSettings){

      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      renderedSql <- SqlRender::render(
        sql,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        min_obs_time = self$minimumObservationLength,
        use_first_op = self$useOnlyFirstObservationPeriod,
        cohort_database_schema = executionSettings$workDatabaseSchema,
        cohort_table = executionSettings$cohortTable,
        prevalent_cohort_id = self$targetCohort$id(),
        multiplier = self$multiplier
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
      return(renderedSql)
    },

    viewAnalysisInfo = function() {
      txt <- c(
        glue::glue("Targt Cohort ==> {self$targetCohort$viewCohortInfo()}"),
        self$periodOfInterest$viewPeriodOfInterest(),
        glue::glue("Observation Period Eligibility ==> Min Observation Period Length: {self$minimumObservationLength} | Using First Observation Period: {self$useOnlyFirstObservationPeriod}"),
        glue::glue("Strata ==> {paste0(self$strata, collapse = ', ')}"),
        glue::glue("Demographic Constraint ==> ageRange: {self$demographicConstraints$ageMin} - {self$demographicConstraints$ageMax}; genderIds: {paste0(self$demographicConstraints$genderIds, collapse =', ')}")
      ) |>
        glue::glue_collapse("\n\n")
      cli::cat_line(txt)
      invisible(txt)
    },

    tabulateAnalysisSettings = function() {

      # prep poi
      if (self$periodOfInterest$poiType == "yearly") {
        a <- min(self$periodOfInterest$poiRange)
        b <- max(self$periodOfInterest$poiRange)
        poi <- glue::glue("{self$periodOfInterest$poiType}: {a}-{b}")
      } else {
        poi <- glue::glue("{self$periodOfInterest$poiType}: {self$periodOfInterest$poiRange$span_label}")
      }


      # prep op
      ope <- glue::glue("obsLength: {self$minimumObservationLength} | firstObs: {self$useOnlyFirstObservationPeriod}")
      demoConAge <- glue::glue("{self$demographicConstraints$ageMin} - {self$demographicConstraints$ageMax}")
      demoConGender <- glue::glue("{paste0(self$demographicConstraints$genderIds, collapse =', ')}")


      tb <- tibble::tibble(
        analysisId = self$analysisId,
        cohortId = self$targetCohort$id(),
        cohortName = self$targetCohort$name(),
        poi = poi,
        obsPeriod = ope,
        demoConAge = demoConAge,
        demoConGender = demoConGender
      )

      return(tb)
    }

  ),
  private = list(
    # formalize vars
    .analysisId = NULL,
    .targetCohort = NULL,
    .periodOfInterest = NULL,
    .minimumObservationLength = NULL,
    .useOnlyFirstObservationPeriod = NULL,
    .multiplier = NULL,
    .strata = NULL,
    .demographicConstraints = NULL,
    .populationCohort = NULL
  ),
  active = list(

    # active fields for R6 class

    analysisId = function(value) {
      if (missing(value)) {
        return(private$.analysisId)
      }
      checkmate::assert_integerish(x = analysisId, len = 1)
      private$.analysisId <- value
    },

    targetCohort = function(value) {
      if (missing(value)) {
        return(private$.targetCohort)
      }
      checkmate::assert_class(x = targetCohort, classes = "CohortInfo")
      private$.targetCohort <- value
    },

    periodOfInterest = function(value) {
      if (missing(value)) {
        return(private$.periodOfInterest)
      }
      checkmate::assert_class(x = periodOfInterst, classes = "PeriodOfInterest")
      private$.periodOfInterest <- value
    },

    minimumObservationLength = function(value) {
      if (missing(value)) {
        return(private$.minimumObservationLength)
      }
      checkmate::assert_integerish(x = minimumObservationLength, len = 1)
      private$.minimumObservationLength <- value
    },

    useOnlyFirstObservationPeriod = function(value) {
      if (missing(value)) {
        return(private$.useOnlyFirstObservationPeriod)
      }
      checkmate::assert_logical(x = useOnlyFirstObservationPeriod , len = 1)
      private$.useOnlyFirstObservationPeriod <- value
    },

    strata = function(value) {
      if (missing(value)) {
        return(private$.strata)
      }
      checkmate::assert_subset(x = strata, choices = c("age", "gender", "race"), empty.ok = TRUE)
      private$.strata <- value

    },

    multiplier = function(value) {
      if (missing(value)) {
        return(private$.multiplier)
      }
      checkmate::assert_integerish(x = multiplier, len = 1)
      private$.multiplier <- value
    },

    populationCohort = function(value) {
      if (missing(value)) {
        return(private$.populationCohort)
      }
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo", null.ok = TRUE)
      private$.populationCohort <- value
    },


    demographicConstraints = function(value) {
      if (missing(value)) {
        return(private$.demographicConstraints)
      }
      checkmate::assert_class(x = demographicConstraints, classes = "DemoConstraint")
      private$.demographicConstraints <- value
    }

  )
)

# SubClasses ------

## PrevalenceType Class ----------------
PrevalenceType <- R6::R6Class(
  classname = "PrevalenceType",
  public = list(
    initialize = function(prevalenceType, lookBackDays) {
      
      # Validate prevalenceType is one of the valid options
      validTypes <- c(
        "point_prevalence",      # pn1 + pd1
        "period_prevalence_pd2", # pn2 + pd2
        "period_prevalence_pd3", # pn2 + pd3
        "period_prevalence_pd4"  # pn2 + pd4
      )
      checkmate::assert_choice(x = prevalenceType, choices = validTypes)
      private[[".prevalenceType"]] <- prevalenceType
      
      # Validate lookBackDays - any non-negative integer, or Inf for complete lookback
      checkmate::assert_integerish(x = lookBackDays, len = 1, lower = 0)
      private[[".lookBackDays"]] <- lookBackDays
      
    },
    
    # Getter: returns the numerator type (pn1 or pn2)
    getNumeratorType = function() {
      if (private$.prevalenceType == "point_prevalence") {
        return("pn1")
      } else {
        return("pn2")
      }
    },
    
    # Getter: returns the denominator type (pd1, pd2, pd3, or pd4)
    getDenominatorType = function() {
      denomMap <- list(
        "point_prevalence" = "pd1",
        "period_prevalence_pd2" = "pd2",
        "period_prevalence_pd3" = "pd3",
        "period_prevalence_pd4" = "pd4"
      )
      denomType <- denomMap[[private$.prevalenceType]]  
      return(denomType)
    },
    
    # Getter: returns human-readable prevalence type label
    getPrevalenceLabel = function() {
      labelMap <- list(
        "point_prevalence" = "Point Prevalence",
        "period_prevalence_pd2" = "Period Prevalence (all time observed)",
        "period_prevalence_pd3" = "Period Prevalence (continuous observation)",
        "period_prevalence_pd4" = "Period Prevalence (sufficient days)"
      )
      return(labelMap[[private$.prevalenceType]])
    },
    
    # Getter: returns human-readable lookback label
    getLookbackLabel = function() {
      lbd <- private$.lookBackDays
      if (lbd == 0) {
        return("No lookback (events during POI only)")
      } else if (lbd == 365) {
        return("1-year lookback")
      } else if (lbd == 1095) {
        return("3-year lookback")
      } else if (is.infinite(lbd)) {
        return("Complete historical lookback")
      } else {
        return(glue::glue("{lbd}-day lookback"))
      }
    },
    
    # View method: display full prevalence type configuration
    viewPrevalenceType = function() {
      prevalenceLabel <- self$getPrevalenceLabel()
      lookbackLabel <- self$getLookbackLabel()
      numerator <- self$getNumeratorType()
      denominator <- self$getDenominatorType()
      
      txt <- c(
        glue::glue("Prevalence Type ==> {prevalenceLabel}"),
        glue::glue("  Numerator: {numerator} | Denominator: {denominator}"),
        glue::glue("  Lookback: {lookbackLabel}")
      ) |> glue::glue_collapse("\n")
      
      return(txt)
    }
    
  ),
  private = list(
    .prevalenceType = NULL,
    .lookBackDays = NULL
  ),
  active = list(
    
    prevalenceType = function(value) {
      if (missing(value)) {
        return(private$.prevalenceType)
      }
      validTypes <- c(
        "point_prevalence",
        "period_prevalence_pd2",
        "period_prevalence_pd3",
        "period_prevalence_pd4"
      )
      checkmate::assert_choice(x = value, choices = validTypes)
      private$.prevalenceType <- value
    },
    
    lookBackDays = function(value) {
      if (missing(value)) {
        return(private$.lookBackDays)
      }
      checkmate::assert_integerish(x = value, len = 1, lower = 0)
      private$.lookBackDays <- value
    }
  )
)

## Cohort Info ----------------
CohortInfo <- R6::R6Class(
  classname = "CohortInfo",
  public = list(
    initialize = function(id, name, cohortType = "prevalent", circeJsonPath = NULL) {

      checkmate::assert_integerish(x = id, len = 1)
      private[[".id"]] <- id

      checkmate::assert_string(x = name, min.chars = 1)
      private[[".name"]] <- name

      # Validate cohort type
      checkmate::assert_choice(x = cohortType, choices = c("prevalent", "population"))
      private[[".cohortType"]] <- cohortType

      # CIRCE validation only required for prevalent cohorts
      if (cohortType == "prevalent") {
        if (is.null(circeJsonPath)) {
          stop(glue::glue("circeJsonPath is required for {cohortType} cohorts"))
        }
        
        tryCatch(
          {
            checkmate::assert_file_exists(circeJsonPath)
            validationResult <- self$validateCirceJson(circeJsonPath)
            
            cli::cli_alert_success(
              glue::glue("✓ CIRCE validation passed for '{name}' (ID: {id})")
            )
            cli::cli_alert_info(
              glue::glue("  Pattern: {validationResult$patternLabel}")
            )
            
            private[[".circePattern"]] <- validationResult$pattern
            private[[".circePatternLabel"]] <- validationResult$patternLabel
          },
          error = function(e) {
            cli::cli_alert_danger(
              glue::glue("✗ CIRCE validation failed for '{name}' (ID: {id})")
            )
            cli::cli_alert_info(glue::glue("  Error: {e$message}"))
            stop(e)
          },
          warning = function(w) {
            cli::cli_alert_warning(
              glue::glue("⚠ Warning during CIRCE validation for '{name}' (ID: {id}): {w$message}")
            )
          }
        )
      } else {
        # Population cohorts don't need CIRCE validation
        private[[".circePattern"]] <- NA_character_
        private[[".circePatternLabel"]] <- "N/A (Population denominator)"
      }
    },
    id = function() {
      cId <- private$.id
      return(cId)
    },
    name = function() {
      cName <- private$.name
      return(cName)
    },
    getCohortType = function() {
      return(private$.cohortType)
    },
    viewCohortInfo = function(){
      id <- self$id()
      name <- self$name()
      cohortType <- private$.cohortType
      patternLabel <- private$.circePatternLabel
      info <- glue::glue("Cohort Id: {id} | Name: {name} | Type: {cohortType} | Pattern: {patternLabel}")
      return(info)
    },
    
    getCircePattern = function() {
      return(private$.circePattern)
    },
    
    validateCirceJson = function(circeJsonPath) {
      # Read and parse CIRCE JSON file
      tryCatch(
        {
          circeJson <- jsonlite::read_json(circeJsonPath)
        },
        error = function(e) {
          stop(glue::glue("Failed to parse CIRCE JSON: {e$message}"))
        }
      )
      
      # Extract key components with safe access
      pcl <- tryCatch(circeJson$PrimaryCriteria$PrimaryCriteriaLimit$Type, error = function(e) NULL)
      el <- tryCatch(circeJson$ExpressionLimit$Type, error = function(e) NULL)
      es <- tryCatch(circeJson$EndStrategy, error = function(e) NULL)
      hasDateOffset <- !is.null(es) && !is.null(es$DateOffset)
      
      # Pattern 1: ERA (First/First/No EndStrategy)
      # Pattern 2: OCCURRENCE (All/All/DateOffset)
      isPattern1 <- (pcl == "First" && el == "First" && is.null(es))
      isPattern2 <- (pcl == "All" && el == "All" && hasDateOffset)
      isValid <- isPattern1 || isPattern2
      
      if (!isValid) {
        stop(
          "Invalid CIRCE cohort definition for ", self$name(), ".\n",
          "Pattern 1 (ERA): PrimaryCriteriaLimit='First', ExpressionLimit='First', no EndStrategy\n",
          "Pattern 2 (OCCURRENCE): PrimaryCriteriaLimit='All', ExpressionLimit='All', DateOffset EndStrategy\n",
          "Found: PrimaryCriteriaLimit='", pcl, "', ExpressionLimit='", el, "', ",
          "EndStrategy=", if (is.null(es)) "NULL" else "Present"
        )
      }
      ll <- list(
        isValid = isValid,
        pattern = if (isPattern1) "era" else "occurrence",
        patternLabel = if (isPattern1) "ERA (Interval Overlap)" else "OCCURRENCE (Point-in-Time)",
        details = list(primaryCriteriaLimit = pcl, expressionLimit = el, hasDateOffset = hasDateOffset)
      )
      
      invisible(ll)
    }
  ),
  private = list(
    .id = NULL,
    .name = NULL,
    .cohortType = NULL,
    .circePattern = NULL,
    .circePatternLabel = NULL
  )
)

## Period of Interest ----------------
PeriodOfInterest <- R6::R6Class(
  classname = "PeriodOfInterest",
  public = list(
    initialize = function(poiRange, poiType = "yearly") {

      if (poiType == "yearly"){
        checkmate::assert_integerish(x = poiRange, min.len = 1)
      } else if (poiType == "span"){
        checkmate::assert_data_frame(x = poiRange,
                                     any.missing = FALSE)
        checkmate::assert_date(poiRange$calendar_start_date, min.len = 1)
        checkmate::assert_date(poiRange$calendar_end_date, min.len = 1)
      }

      private[[".poiRange"]] <- poiRange

      checkmate::assert_choice(x = poiType, choices = c("yearly", "span"))
      private[[".poiType"]] <- poiType
    },

    viewPeriodOfInterest = function() {
      poiType <- self$poiType
      poiRange <- self$poiRange

      if (poiType == "yearly") {
        poiRange2 <- poiRange |> glue::glue_collapse(", ")
      }

      if (poiType == "span") {
        poiRange2 <- poiRange$span_label |> glue::glue_collapse(", ")
      }

      txt <- glue::glue("Period of Interest ==> type: {poiType} | Range: {poiRange2}")
      return(txt)
    }
  ),

  private = list(
    .poiType = NULL,
    .poiRange = NULL
  ),

  active = list(
    poiType = function(value) {
      if (missing(value)) {
        return(private$.poiType)
      }
      checkmate::assert_choice(x = value, choices = c("yearly", "span"))
      private$.poiType <- value
    },

    poiRange = function(value) {
      if (missing(value)) {
        return(private$.poiRange)
      }
      if (self$poiType == "yearly"){
        checkmate::assert_integerish(x = value, min.len = 1)
      } else if (self$poiType == "span"){
        checkmate::assert_data_frame(x = value,
                                     any.missing = FALSE)
        checkmate::assert_date(value$calendar_start_date, min.len = 1)
        checkmate::assert_date(value$calendar_end_date, min.len = 1)
      }
      private$.poiRange <- value
    }
  )

)

## Demographic Constraints -------------
DemoConstraint <- R6::R6Class(
  classname = "DemoConstraint",
  public = list(
    initialize = function(
      ageMin = 0,
      ageMax = 150,
      genderIds = c(8532, 8507)
    ) {
      checkmate::assert_integerish(x = ageMin, len = 1)
      private[[".ageMin"]] <- ageMin

      checkmate::assert_integerish(x = ageMax, len = 1)
      private[[".ageMax"]] <- ageMax

      checkmate::assert_integerish(x = genderIds)
      private[[".genderIds"]] <- genderIds
    }
  ),
  private = list(
    .ageMin = NULL,
    .ageMax = NULL,
    .genderIds = NULL
  ),
  active = list(

    ageMin = function(value) {
      if (missing(value)) {
        return(private$.ageMin)
      }
      checkmate::assert_integerish(x = value, len = 1)
      private$.ageMin <- value
    },

    ageMax = function(value) {
      if (missing(value)) {
        return(private$.ageMax)
      }
      checkmate::assert_integerish(x = value, len = 1)
      private$.ageMax <- value
    },

    genderIds = function(value) {
      if (missing(value)) {
        return(private$.genderIds)
      }
      checkmate::assert_integerish(x = value)
      private$.genderIds <- value
    }
  )
)
