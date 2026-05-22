#' CohortPrevalenceExperiment R6 Class
#'
#' @description
#' Define and manage multi-dimensional prevalence analysis experiments.
#' Enables users to specify cohorts, prevalence types, demographic constraints,
#' and periods of interest, then automatically generates a Cartesian product of
#' analysis definitions with full provenance tracking.
#'
#' @details
#' The experiment class uses the builder pattern to define analysis dimensions:
#'
#' 1. Add cohorts (one or more)
#' 2. Add prevalence types (one or more)
#' 3. Add demographic constraints (one or more)
#' 4. Add periods of interest (one or more)
#' 5. Set common parameters (strata, output types)
#' 6. Call `define()` to materialize analysis objects
#' 7. Pass analysis list to `generatePrevalence()`
#'
#' ## Design Philosophy
#' - **Input**: Object-based (type-safe), validated on each addition
#' - **Output**: Flat tibble specification, CSV-exportable for reproducibility
#' - **Execution**: `exp$define()` → `generatePrevalence(analyses, ...)`
#'   - Explicit data flow: experiment defines dimensions, `define()` materializes analyses, `generatePrevalence()` executes
#'
#' ## Example
#' ```
#' exp <- CohortPrevalenceExperiment$new("CKD Multi-Cohort")
#'
#' exp$addCohorts(tibble::tibble(
#'   cohortId = c(1, 2, 3),
#'   cohortName = c("CKD A", "CKD B", "CKD C")
#' ))
#'
#' exp$addPrevalenceTypes(list(
#'   createPrevalenceType("point", lookBackDays = 0L),
#'   createPrevalenceType("period_prevalence_pd2", lookBackDays = 365L),
#'   createPrevalenceType("period_prevalence_pd3", lookBackDays = 0L)
#' ))
#'
#' exp$addDemographicConstraints(list(
#'   createDemographicConstraints(ageMin = 18, ageMax = 150, genderIds = c(8507L, 8532L, 0L)),
#'   createDemographicConstraints(ageMin = 65, ageMax = 150, genderIds = c(8507L, 8532L, 0L)),
#'   createDemographicConstraints(ageMin = 18, ageMax = 150, genderIds = c(8507L))
#' ))
#'
#' exp$addPeriodsOfInterest(list(
#'   createYearlyRange(2015:2024),
#'   createSpan(start = "2020-01-01", end = "2020-12-31"),
#'   createSpan(start = "2021-01-01", end = "2021-12-31")
#' ))
#'
#' exp$setCommonParameters(strata = c("age", "gender"), outputTypes = "prevalence")
#'
#' # View specification
#' exp$viewDesign()
#' exp$viewDesign("html")
#'
#' # Define analyses (3 × 3 × 3 × 3 = 81 analyses)
#' analyses <- exp$define()
#'
#' # Execute
#' results <- generatePrevalence(analyses, executionSettings = settings)
#' ```
#'
#' @export
CohortPrevalenceExperiment <- R6::R6Class(
  "CohortPrevalenceExperiment",
  public = list(
    #' @description Create new experiment
    #' @param name Character. Descriptive name for the experiment
    #' @param description Character. Optional description
    initialize = function(name, description = NULL) {
      private$.name <- name
      private$.description <- description
    },

    #' @description Add cohort definitions
    #' @param cohorts Tibble with columns:
    #'   - `cohortId` (numeric, required): cohort ID in the results schema
    #'   - `cohortName` (character, required): display name
    #' @return Invisibly returns self for method chaining
    addCohorts = function(cohorts) {
      checkmate::assert_tibble(cohorts)
      checkmate::assert_names(
        colnames(cohorts),
        must.include = c("cohortId", "cohortName")
      )
      checkmate::assert_numeric(cohorts$cohortId, any.missing = FALSE)
      checkmate::assert_character(cohorts$cohortName, any.missing = FALSE)
      checkmate::assert_true(length(unique(cohorts$cohortId)) == nrow(cohorts),
                            .var.name = "cohortIds must be unique")

      private$.cohorts <- cohorts
      cli::cli_alert_success("Added {nrow(cohorts)} cohort(s)")
      invisible(self)
    },

    #' @description Add prevalence type definitions
    #' @param types_list List of prevalenceType objects (created via createPrevalenceType)
    #' @return Invisibly returns self for method chaining
    addPrevalenceTypes = function(types_list) {
      checkmate::assert_list(types_list, min.len = 1)

      # Validate each item has required structure
      for (i in seq_along(types_list)) {
        obj <- types_list[[i]]
        if (!("prevalenceType" %in% names(obj)) || !("lookBackDays" %in% names(obj))) {
          stop(
            "Item ", i, " in types_list does not appear to be a valid prevalenceType object. ",
            "Use createPrevalenceType() to create objects."
          )
        }
      }

      private$.prevalenceTypes <- types_list
      cli::cli_alert_success("Added {length(types_list)} prevalence type(s)")
      invisible(self)
    },

    #' @description Add demographic constraint definitions
    #' @param constraints_list List of demographicConstraints objects (created via createDemographicConstraints)
    #' @return Invisibly returns self for method chaining
    addDemographicConstraints = function(constraints_list) {
      checkmate::assert_list(constraints_list, min.len = 1)

      # Validate each item has required structure
      for (i in seq_along(constraints_list)) {
        obj <- constraints_list[[i]]
        if (!all(c("ageMin", "ageMax", "genderIds") %in% names(obj))) {
          stop(
            "Item ", i, " in constraints_list does not appear to be a valid demographicConstraints object. ",
            "Use createDemographicConstraints() to create objects."
          )
        }
      }

      private$.demographicConstraints <- constraints_list
      cli::cli_alert_success("Added {length(constraints_list)} demographic constraint(s)")
      invisible(self)
    },

    #' @description Add periods of interest
    #' @param poi_list List of period objects (YearlyRange or Span objects)
    #' @return Invisibly returns self for method chaining
    addPeriodsOfInterest = function(poi_list) {
      checkmate::assert_list(poi_list, min.len = 1)

      # Validate each item is either YearlyRange or Span
      for (i in seq_along(poi_list)) {
        obj <- poi_list[[i]]
        is_poi <- inherits(obj, "PeriodOfInterest")
        #is_yearly <- obj$poiType == "yearly"
        #is_span <- obj$poiType == "span"

        if (!is_poi) {
          stop(
            "Item ", i, " in poi_list is not recognized as PeriodOfInterest Class. ",
            "Use createYearlyRange() or createSpan() to create objects."
          )
        }
      }

      private$.periodsOfInterest <- poi_list
      cli::cli_alert_success("Added {length(poi_list)} period(s) of interest")
      invisible(self)
    },

    #' @description Set common parameters for all analyses
    #' @param strata Character vector of strata variables (e.g., c("age", "gender"))
    #' @param outputTypes Character vector of output types (e.g., "prevalence")
    #' @return Invisibly returns self for method chaining
    setCommonParameters = function(strata = NULL, outputTypes = NULL) {
      if (!is.null(strata)) {
        checkmate::assert_character(strata, any.missing = FALSE)
      }
      if (!is.null(outputTypes)) {
        checkmate::assert_character(outputTypes, any.missing = FALSE)
      }

      private$.strata <- strata
      private$.outputTypes <- outputTypes
      invisible(self)
    },

    #' @description Validate all required parameters are set
    #' @return Invisibly returns TRUE if valid, otherwise stops with error
    validate = function() {
      if (is.null(private$.cohorts)) {
        stop("Cohorts not set. Call addCohorts() first.")
      }
      if (is.null(private$.prevalenceTypes)) {
        stop("Prevalence types not set. Call addPrevalenceTypes() first.")
      }
      if (is.null(private$.demographicConstraints)) {
        stop("Demographic constraints not set. Call addDemographicConstraints() first.")
      }
      if (is.null(private$.periodsOfInterest)) {
        stop("Periods of interest not set. Call addPeriodsOfInterest() first.")
      }

      invisible(TRUE)
    },

    #' @description Get specification as flat tibble
    #' @return Tibble with one row per analysis combination
    getSpecification = function() {
      self$validate()
      private$.createExpandedSpec()
    },

    #' @description View experiment design
    #' @param format Character. "console" (default) for text table, "html" for interactive reactable
    #' @return Invisibly returns specification tibble
    viewDesign = function(format = "console") {
      spec <- self$getSpecification()

      if (format == "console") {
        cli::cat_line()
        cli::cat_rule(private$.name)
        if (!is.null(private$.description)) {
          cli::cli_inform(private$.description)
        }
        cli::cli_alert_info("Total analyses: {nrow(spec)}")
        cli::cli_alert_info(
          "  {nrow(private$.cohorts)} cohort(s) × {length(private$.prevalenceTypes)} prevalence type(s) × {length(private$.demographicConstraints)} demographic constraint(s) × {length(private$.periodsOfInterest)} period(s) of interest"
        )
        cli::cat_line()

        # Show summary table
        summary_table <- spec |>
          dplyr::select(analysisId, cohortName, prevalenceType, ageMin, ageMax, poiLabel) |>
          head(25)

        print(summary_table)

        if (nrow(spec) > 25) {
          cli::cli_alert_info("... and {nrow(spec) - 25} more analyses. Call getSpecification() to see all.")
        }
        cli::cat_line()

      } else if (format == "html") {
        if (!requireNamespace("reactable", quietly = TRUE)) {
          stop(
            "reactable package required for HTML view. ",
            "Install with: install.packages('reactable')"
          )
        }

        reactable::reactable(
          spec,
          searchable = TRUE,
          filterable = TRUE,
          striped = TRUE,
          highlight = TRUE,
          columns = list(
            analysisId = reactable::colDef(width = 70),
            cohortId = reactable::colDef(width = 70),
            cohortName = reactable::colDef(width = 140),
            prevalenceType = reactable::colDef(width = 120),
            lookBackDays = reactable::colDef(width = 80),
            ageMin = reactable::colDef(width = 60),
            ageMax = reactable::colDef(width = 60),
            genderIds = reactable::colDef(width = 80),
            poiType = reactable::colDef(width = 80),
            poiLabel = reactable::colDef(width = 100)
          ),
          pagination = TRUE,
          defaultPageSize = 25,
          outlined = TRUE
        )
      } else {
        stop("format must be 'console' or 'html'")
      }

      invisible(spec)
    },

    #' @description Define analysis specifications (create analysis objects)
    #' @param autoIdStart Numeric. Starting ID for analyses (default: 1L)
    #' @return List of CohortPrevalenceAnalysis objects with specification attached as attribute
    define = function(autoIdStart = 1L) {
      checkmate::assert_int(autoIdStart, lower = 1L)

      spec <- self$getSpecification()

      # Adjust IDs if needed
      if (autoIdStart != 1L) {
        spec$analysisId <- seq(autoIdStart, autoIdStart + nrow(spec) - 1L)
      }

      # Create analysis objects from spec rows
      analyses <- vector("list", nrow(spec))

      for (i in 1:nrow(spec)) {
        row <- spec[i, ]

        # Reconstruct POI object from spec row
        poi_obj <- if (row$poiType == "yearly") {
          # Extract years from poiLabel (format: "2015-2024")
          year_parts <- as.numeric(strsplit(row$poiLabel, "-")[[1]])
          createYearlyRange(year_parts[1]:year_parts[2])
        } else {
          # Reconstruct Span from dates
          createSpan(start = row$poiStart, end = row$poiEnd)
        }

        analyses[[i]] <- createCohortPrevalenceAnalysis(
          analysisId = row$analysisId,
          prevalentCohort = createTargetCohort(
            cohortId = row$cohortId,
            cohortName = row$cohortName
          ),
          periodOfInterest = poi_obj,
          prevalenceType = createPrevalenceType(
            prevalenceType = row$prevalenceType,
            lookBackDays = row$lookBackDays
          ),
          strata = row$strata[[1]],
          demographicConstraints = createDemographicConstraints(
            ageMin = row$ageMin,
            ageMax = row$ageMax,
            genderIds = row$genderIds[[1]]
          ),
          outputTypes = row$outputTypes[[1]]
        )
      }

      # Attach metadata for reproducibility
      attr(analyses, "specification") <- spec
      attr(analyses, "experiment_name") <- private$.name
      attr(analyses, "experiment_description") <- private$.description

      cli::cli_alert_success("Defined {length(analyses)} analysis object(s)")
      invisible(analyses)
    }
  ),

  private = list(
    .name = NULL,
    .description = NULL,
    .cohorts = NULL,
    .prevalenceTypes = NULL,
    .demographicConstraints = NULL,
    .periodsOfInterest = NULL,
    .strata = NULL,
    .outputTypes = NULL,

    # Expand periods of interest into flat specification rows
    .expandPeriodsOfInterest = function() {
      poi_spec <- tibble::tibble(
        poiType = character(),
        poiLabel = character()
      )

      for (i in seq_along(private$.periodsOfInterest)) {
        poi <- private$.periodsOfInterest[[i]]
        is_yearly <- poi$poiType == "yearly"
        is_span <- poi$poiType == "span"
        # Check if YearlyRange
        if (is_yearly) {
           a <- min(poi$poiRange)
           b <- max(poi$poiRange)
           poiLabel <- glue::glue("{a}-{b}")
           poi_spec <- rbind(
            poi_spec, 
            tibble::tibble(
             poiType = "yearly",
             poiLabel = poiLabel
            )
           )
        } else {
          poiLabel <- glue::glue("{poi$poiRange$span_label}")
          poi_spec <- rbind(
            poi_spec, 
            tibble::tibble(
             poiType = "span",
             poiLabel = poiLabel
            )
           )
        }
      }

      return(poi_spec)
    },

    # Create expanded specification tibble
    .createExpandedSpec = function() {
      # Convert resilience types list to tibble
      prevalence_spec <- tibble::tibble(
        prevalenceType = sapply(private$.prevalenceTypes, function(x) x$prevalenceType),
        lookBackDays = sapply(private$.prevalenceTypes, function(x) x$lookBackDays)
      )

      # Convert demographic constraints list to tibble
      demographic_spec <- tibble::tibble(
        ageMin = sapply(private$.demographicConstraints, function(x) x$ageMin),
        ageMax = sapply(private$.demographicConstraints, function(x) x$ageMax),
        genderIds = lapply(private$.demographicConstraints, function(x) x$genderIds)
      )

      # Expand periods of interest
      poi_spec <- private$.expandPeriodsOfInterest()

      # Cartesian product
      spec <- tidyr::expand_grid(
        private$.cohorts,
        prevalence_spec,
        demographic_spec,
        poi_spec
      ) |>
        dplyr::mutate(
          analysisId = dplyr::row_number(),
          strata = list(private$.strata),
          outputTypes = list(private$.outputTypes)
        ) |>
        dplyr::select(
          analysisId,
          cohortId, cohortName,
          prevalenceType, lookBackDays,
          ageMin, ageMax, genderIds,
          poiType, poiLabel,
          strata, outputTypes
        )

      spec
    }
  )
)
