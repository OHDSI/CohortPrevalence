# Main class -------------------
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
      checkmate::assert_integer(x = analysisId, len = 1)
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

      # set denominatorType
      checkmate::assert_choice(x = denominatorType, choices = c("pd1", "pd2", "pd3", "pd4"))
      private[[".denominatorType"]] <- denominatorType

      # set minimumObservationLength
      checkmate::assert_integer(x = minimumObservationLength, len = 1)
      private[[".minimumObservationLength"]] <- minimumObservationLength

      # set useOnlyFirstObservationPeriod
      checkmate::assert_logical(x = useOnlyFirstObservationPeriod, len = 1)
      private[[".useOnlyFirstObservationPeriod"]] <- useOnlyFirstObservationPeriod

      # set multiplier
      checkmate::assert_integer(x = multiplier, len = 1)
      private[[". multiplier"]] <- multiplier

      # set strata
      checkmate::assert_choice(x = strata, choices = c("age", "gender", "race"), null.ok = TRUE)
      private[[".strata"]] <- strata

      # set population
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo")
      private[[".populationCohort"]] <- populationCohort

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
      checkmate::assert_integer(x = analysisId, len = 1)
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
      checkmate::assert_choice(x = denominatorType, choices = c("pd1", "pd2", "pd3", "pd4"))
      private$.denominatorType <- value
    },

    minimumObservationLength = function(value) {
      if (missing(value)) {
        return(private$.minimumObservationLength)
      }
      checkmate::assert_integer(x = minimumObservationLength, len = 1)
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
      checkmate::assert_integer(x = multiplier, len = 1)
      private$.multiplier <- value
    },


    populationCohort = function(value) {
      if (missing(value)) {
        return(private$.populationCohort)
      }
      checkmate::assert_class(x = populationCohort, classes = "CohortInfo")
      private$.populationCohort <- value
    }

  )
)

# SubClasses ------

LookBackOptions <- R6::R6Class(
  classname = "LookBackOptions",
  public = list(
    initialize = function(lookBackDays, useObservedTimeOnly = FALSE) {
      # set lookBackDays
      checkmate::assert_integer(x = lookBackDays, len = 1)
      private[[".lookBackDays"]] <- lookBackDays

      # set useObservedTimeOnly
      checkmate::assert_logical(x = useObservedTimeOnly, len = 1)
      private[[".useObservedTimeOnly"]] <- useObservedTimeOnly
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
      checkmate::assert_integer(x = lookBackDays, len = 1)
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


CohortInfo <- R6::R6Class(
  classname = "CohortInfo",
  public = list(
    #' @param id the cohort definition id
    #' @param name the name of the cohort definition
    initialize = function(id, name) {

      checkmate::assert_integer(x = id, len = 1)
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


PeriodOfInterest <- R6::R6Class(
  classname = "PeriodOfInterest",
  public = list(
    initialize = function(poiType, poiRange) {

      checkmate::assert_integer(x = poiRange, min.len = 1)
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
      checkmate::assert_integer(x = poiRange, min.len = 1)
      private$.poiRange <- value
    }
  )

)
