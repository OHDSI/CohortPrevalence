# Main class -------------------

## CohortPrevalenceAnalysis ----------------
CohortPrevalenceAnalysis <- R6::R6Class(
  classname = "CohortPrevalenceAnalysis",
  public = list(
    initialize = function(analysisId,
                          prevalentCohort,
                          periodOfInterest,
                          lookBackOptions,
                          numeratorType,
                          denominatorType,
                          minimumObservationLength = 0L,
                          useOnlyFirstObservationPeriod = FALSE,
                          multiplier = 100000,
                          strata = NULL,
                          populationCohort = NULL) {
      # set analysisId
      checkmate::assert_integerish(x = analysisId, len = 1)
      private[[".analysisId"]] <- analysisId

      # set prevalent cohort
      checkmate::assert_class(x = prevalentCohort, classes = "CohortInfo")
      private[[".prevalentCohort"]] <- prevalentCohort

      # set periodOfInterst
      checkmate::assert_class(x = periodOfInterest, classes = "PeriodOfInterest")
      private[[".periodOfInterest"]] <- periodOfInterest

      # set lookBackOptions
      checkmate::assert_class(x = lookBackOptions, classes = "LookBackOptions")
      private[[".lookBackOptions"]] <- lookBackOptions

      # set numeratorType
      checkmate::assert_choice(x = numeratorType, choices = c("pn1", "pn2"))
      private[[".numeratorType"]] <- numeratorType

      # set denominator type
      checkmate::assert_class(x = denominatorType, classes = "DenominatorType")
      private[[".denominatorType"]] <- denominatorType


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
      checkmate::assert_choice(x = strata, choices = c("age", "gender", "race"), null.ok = TRUE)
      private[[".strata"]] <- strata

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
          calendar_year = self$periodOfInterest$poiRange
        )
        yearRangeSql <- .insertTableSql(
          executionSettings,
          tableName = "yearly_interval",
          data = years
        )
        # get the obs pop year sql
        obsPopYearSql <- readr::read_file(
          fs::path_package(package = "CohortPrevalence", "sql/obsPopYear.sql")
        )

      }

      # Step 1: Get the appropriate sql files

      # get the ObsPop -- this filters to eligible obs periods
      obsPopSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", "sql/obsPop.sql")
      )

      # get the denom file
      denomType <- self$denominatorType$getDenomType()
      denomSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/{denomType}.sql"))
      )

      # get the numerator file
      numType <- self$numeratorType
      numSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/{numType}.sql"))
      )

      # get the doPrev file
      prevSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/doPrevalence.sql"))
      )

      allSql <- c(yearRangeSql, obsPopSql, obsPopYearSql, denomSql, numSql, prevSql) |>
        glue::glue_collapse("\n\n")

      return(allSql)

    },

    renderAssembledSql = function(sql, executionSettings){

      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      renderedSql <- SqlRender::render(sql,
                                       cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
                                       min_obs_time = self$minimumObservationLength,
                                       use_first_op = self$useOnlyFirstObservationPeriod,
                                       cohort_database_schema = executionSettings$workDatabaseSchema,
                                       cohort_table = executionSettings$cohortTable,
                                       prevalent_cohort_id = self$prevalentCohort$cohortId,
                                       use_observed_time = self$lookBackOptions$useObservedTimeOnly)
      return(renderedSql)
    }
  ),
  private = list(
    # formalize vars
    .analysisId = NULL,
    .prevalentCohort = NULL,
    .periodOfInterest = NULL,
    .lookBackOptions = NULL,
    .numeratorType = NULL,
    .denominatorType = NULL,
    .minimumObservationLength = NULL,
    .useOnlyFirstObservationPeriod = NULL,
    .multiplier = NULL,
    .strata = NULL,
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
      checkmate::assert_class(x = periodOfInterst, classes = "PeriodOfInterest")
      private$.periodOfInterest <- value
    },

    lookBackOptions = function(value) {
      if (missing(value)) {
        return(private$.lookBackOptions)
      }
      checkmate::assert_class(x = lookBackOptions, classes = "LookBackOptions")
      private$.lookBackOptions <- value
    },


    numeratorType = function(value) {
      if (missing(value)) {
        return(private$.numeratorType)
      }
      checkmate::assert_choice(x = numeratorType, choices = c("pn1", "pn2"))
      private$.numeratorType <- value
    },

    denominatorType = function(value) {
      if (missing(value)) {
        return(private$.denominatorType)
      }
      checkmate::assert_class(x = denominatorType, classes = "DenominatorType")
      private$.denominatorType <- value
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
      checkmate::assert_choice(x = strata, choices = c("age", "gender", "race"), null.ok = TRUE)
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
    }

  )
)

# SubClasses ------

## Lookback options --------------
LookBackOptions <- R6::R6Class(
  classname = "LookBackOptions",
  public = list(
    initialize = function(lookBackDays, useObservedTimeOnly = FALSE) {
      # set lookBackDays
      checkmate::assert_integerish(x = lookBackDays, len = 1)
      private[[".lookBackDays"]] <- lookBackDays

      # set useObservedTimeOnly
      checkmate::assert_logical(x = useObservedTimeOnly, len = 1)
      private[[".useObservedTimeOnly"]] <- useObservedTimeOnly
    },

    viewLookBackOptions = function() {
      lbd <- self$lookBackDays
      uoto <- self$useObservedTimeOnly
      txt <- glue::glue("Lookback Options ==> Lookback Days: {lbd}d | Using Observed Time: {uoto}")
      cli::cat_line(txt)
      invisible(txt)
    }
  ),
  private = list(
    .lookBackDays = NULL,
    .useObservedTimeOnly = NULL
  ),

  active = list(

    lookBackDays = function(value) {
      if (missing(value)) {
        return(private$.lookBackDays)
      }
      checkmate::assert_integerish(x = lookBackDays, len = 1)
      private$.lookBackDays <- value
    },

    useObservedTimeOnly = function(value) {
      if (missing(value)) {
        return(private$.useObservedTimeOnly)
      }
      checkmate::assert_logical(x = useObservedTimeOnly, len = 1)
      private$.useObservedTimeOnly <- value
    }
  )
)

## Cohort Info ----------------
CohortInfo <- R6::R6Class(
  classname = "CohortInfo",
  public = list(
    #' @param id the cohort definition id
    #' @param name the name of the cohort definition
    initialize = function(id, name) {

      checkmate::assert_integerish(x = id, len = 1)
      private[[".id"]] <- id

      checkmate::assert_string(x = name, min.chars = 1)
      private[[".name"]] <- name
    },
    #' @description get the cohort id
    id = function() {
      cId <- private$.id
      return(cId)
    },
    #' @description get the cohort name
    name = function() {
      cName <- private$.name
      return(cName)
    },
    #' @description print the cohort details
    cohortDetails = function(){
      id <- self$id()
      name <- self$name()
      info <- glue::glue_col( "\t- Cohort Id: {green {id}}; Cohort Name: {green {name}}")
      return(info)
    }
  ),
  private = list(
    .id = NULL,
    .name = NULL
  )
)

## Period of Interest ----------------
PeriodOfInterest <- R6::R6Class(
  classname = "PeriodOfInterest",
  public = list(
    initialize = function(poiRange, poiType = "yearly") {

      checkmate::assert_integerish(x = poiRange, min.len = 1)
      private[[".poiRange"]] <- poiRange

      checkmate::assert_string(x = poiType, min.chars = 1)
      private[[".poiType"]] <- poiType
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
      checkmate::assert_string(x = poiType, min.chars = 1)
      private$.poiType <- value
    },

    poiRange = function(value) {
      if (missing(value)) {
        return(private$.poiRange)
      }
      checkmate::assert_integerish(x = poiRange, min.len = 1)
      private$.poiRange <- value
    }
  )

)

## Denominator Type -------------
DenominatorType <- R6::R6Class(
  classname = "DenominatorType",
  public = list(
    initialize = function(denomType, sufficientDays) {
      # set denominatorType
      checkmate::assert_choice(x = denomType, choices = c("pd1", "pd2", "pd3", "pd4"))
      private[[".denomType"]] <- denomType

      if (denomType == "pd4") {
        checkmate::assert_integerish(x = sufficientDays, len = 1)
        private[[".sufficientDays"]] <- sufficientDays
      }
    },
    # TODO add more defensive programming to prevent suffientDays to non pd4
    updateDenominatorType = function(denomType, sufficientDays) {
      checkmate::assert_choice(x = denomType, choices = c("pd1", "pd2", "pd3", "pd4"))
      private$.denomType <- denomType
      if (denomType != "pd4") {
        private$.sufficientDays <- NULL
        cli::cat_line(glue::glue("Remove sufficientDays option for {denomType}"))
      }
      if (denomType == "pd4") {
        private$.sufficientDays <- sufficientDays
        cli::cat_line(glue::glue("Update sufficientDays option to {sufficientDays} days for {denomType}"))
      }
    },

    getDenomType = function() {
      denomType <- private$.denomType
      return(denomType)
    }
  ),
  private = list(
    .denomType = NULL,
    .sufficientDays = NULL
  )
)
