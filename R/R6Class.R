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
                          demographicConstraints,
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

      # get the denom file
      denomType <- self$denominatorType$getDenomType()
      if (denomType == "pd4") {
        sufficientDays <- self$denominatorType$getSufficientDays()
      }
      denomSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/{denomType}.sql"))
      ) |>
        glue::glue()

      # get the numerator file
      numType <- self$numeratorType
      numSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/{numType}.sql"))
      )

      # get the doPrev file
      prevSql <- readr::read_file(
        fs::path_package(package = "CohortPrevalence", glue::glue("sql/doPrevalence.sql"))
      ) |>
        glue::glue()

      allSql <- c(yearRangeSql, obsPopSql, obsPopYearSql, denomSql, numSql, prevSql) |>
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
        use_observed_time = self$lookBackOptions$useObservedTimeOnly,
        lookback = self$lookBackOptions$lookBackDays,
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
        glue::glue("Prevalent Cohort ==> {self$prevalentCohort$viewCohortInfo()}"),
        self$periodOfInterest$viewPeriodOfInterest(),
        c(glue::glue("Numerator Type ==> {self$numeratorType}"),
          getNumText(numType = self$numeratorType)
        ) |> glue::glue_collapse("\n"),
        self$denominatorType$viewDenominatorType(),
        self$lookBackOptions$viewLookBackOptions(),
        glue::glue("Observation Period Eligibility ==> Min Observation Period Length: {self$minimumObservationLength} | Using First Observation Period: {self$useOnlyFirstObservationPeriod}"),
        glue::glue("Strata ==> {paste0(self$strata, collapse = ', ')}"),
        glue::glue("Demographic Constraint ==> ageRange: {self$demographicConstraints$ageMin} - {self$demographicConstraints$ageMax}; genderIds: {paste0(self$demographicConstraints$genderIds, collapse =', ')}")
      ) |>
        glue::glue_collapse("\n\n")
      cli::cat_line(txt)
      invisible(txt)
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
        use_observed_time = FALSE,
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
      return(txt)
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
    initialize = function(id, name) {

      checkmate::assert_integerish(x = id, len = 1)
      private[[".id"]] <- id

      checkmate::assert_string(x = name, min.chars = 1)
      private[[".name"]] <- name
    },
    id = function() {
      cId <- private$.id
      return(cId)
    },
    name = function() {
      cName <- private$.name
      return(cName)
    },
    viewCohortInfo = function(){
      id <- self$id()
      name <- self$name()
      info <- glue::glue("Cohort Id: {id} | Cohort Name: {name}")
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
      checkmate::assert_choice(x = poiType, choices = c("yearly", "span"))
      private$.poiType <- value
    },

    poiRange = function(value) {
      if (missing(value)) {
        return(private$.poiRange)
      }
      if (poiType == "yearly"){
        checkmate::assert_integerish(x = poiRange, min.len = 1)
      } else if (poiType == "span"){
        checkmate::assert_data_frame(x = poiRange,
                                     any.missing = FALSE)
        checkmate::assert_date(poiRange$calendar_start_date, min.len = 1)
        checkmate::assert_date(poiRange$calendar_end_date, min.len = 1)
      }
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
    },

    viewDenominatorType = function() {
      denomType <- self$getDenomType()
      sufficientDays <- private$.sufficientDays
      if (denomType == "pn4") {
        txt1 <- glue::glue("Denominator Type ==> {denomType} | Sufficient Days: {sufficientDays}")
      } else {
        txt1 <- glue::glue("Denominator Type ==> {denomType}")
      }
      txt2 <- getDenomText(denomType)
      txt <- c(txt1, txt2) |> glue::glue_collapse("\n")
      return(txt)
    },

    getSufficientDays = function() {
      private$.sufficientDays
    }
  ),
  private = list(
    .denomType = NULL,
    .sufficientDays = NULL
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
      checkmate::assert_integerish(x = ageMin, len = 1)
      private$.ageMin <- value
    },

    ageMax = function(value) {
      if (missing(value)) {
        return(private$.ageMax)
      }
      checkmate::assert_integerish(x = ageMax, len = 1)
      private$.ageMax <- value
    },

    genderIds = function(value) {
      if (missing(value)) {
        return(private$.genderIds)
      }
      checkmate::assert_integerish(x = genderIds, len = 1)
      private$.genderIds <- value
    }
  )
)
