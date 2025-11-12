# Main class -------------------
CohortPrevalenceAnalysis <- R6::R6Class(
  classname = "CohortPrevalenceAnalysis",
  public = list(
    initialize = function(analysisId,
                          prevalentCohort,
                          periodOfInterest,
                          lookBackDays,
                          numeratorType,
                          denominatorType,
                          minimumObservationLength = 0L,
                          useOnlyFirstObservationPeriod = FALSE,
                          useObservedTimeOnly = FALSE) {
      # set analysisId
      checkmate::assert_integer(x = analysisId, len = 1)
      private[[".analysisId"]] <- analysisId

      # set prevalent cohort
      checkmate::assert_class(x = prevalentCohort, classes = "PrevalenceCohort")
      private[[".prevalentCohort"]] <- prevalentCohort

      # set periodOfInterst
      checkmate::assert_class(x = periodOfInterest, classes = "PeriodOfInterest")
      private[[".periodOfInterest"]] <- periodOfInterest

      # set lookBackDays
      checkmate::assert_integer(x = lookBackDays, len = 1)
      private[[".lookBackDays"]] <- lookBackDays

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

      # set useObservedTimeOnly
      checkmate::assert_logical(x = useObservedTimeOnly, len = 1)
      private[[".useObservedTimeOnly"]] <- useObservedTimeOnly

    }
  ),
  private = list(
    # formalize vars
    .analysisId = NULL,
    .prevalentCohort = NULL,
    .periodOfInterest = NULL,
    .lookBackDays = NULL,
    .numeratorType = NULL,
    .denominatorType = NULL,
    .minimumObservationLength = NULL,
    .useOnlyFirstObservationPeriod = NULL,
    .useObservedTimeOnly = NULL
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
      checkmate::assert_class(x = prevalentCohort, classes = "PrevalenceCohort")
      private$.prevalentCohort <- value
    },

    periodOfInterest = function(value) {
      if (missing(value)) {
        return(private$.periodOfInterest)
      }
      checkmate::assert_class(x = periodOfInterst, classes = "PeriodOfInterest")
      private$.periodOfInterest <- value
    },

    lookBackDays = function(value) {
      if (missing(value)) {
        return(private$.lookBackDays)
      }
      checkmate::assert_integer(x = lookBackDays, len = 1)
      private$.lookBackDays <- value
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

    useObservedTimeOnly = function(value) {
      if (missing(value)) {
        return(private$.useObservedTimeOnly)
      }
      checkmate::assert_logical(x = useObservedTimeOnly, len = 1)
      private$.useObservedTimeOnly <- value
    }

  )
)

# SubClasses -----

