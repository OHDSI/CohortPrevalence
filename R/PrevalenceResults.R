#' PrevalenceResults R6 Class
#'
#' @description
#' Container for prevalence analysis results with standardization capabilities.
#' Manages export/import with full provenance tracking via manifest file.
#'
#' @importFrom rlang %||%
#'
#' @details
#' Results are stored as CSV files in a directory bundle with a manifest.json file
#' tracking provenance, standardization parameters, and execution metadata.
#'
#' ## Active Fields
#' - `prevalence`: Data frame with crude (unadjusted) prevalence results (read/write)
#' - `stdPrev`: Data frame with standardized prevalence results (read/write)
#' - `incidence`: Data frame with incidence results (read/write)
#' - `drugUsage`: Data frame with drug usage results (read/write)
#' - `metaInfo`: Data frame with analysis metadata (read/write)
#' - `standardizationApplied`: List containing standardization parameters (read-only)
#'
#' ## Private Tracking Fields (visible in print/summary only)
#' - `executionId`: Unique execution identifier
#' - `exportDate`: Timestamp of last export
#'
#' ## Methods
#' - `initialize()`: Create new PrevalenceResults object
#' - `export()`: Save results to directory bundle with manifest
#' - `standardizePrevalence()`: Apply direct method standardization (returns new object)
#' - `validate()`: Check data integrity and consistency
#' - `summary()`: Print summary statistics
#' - `print()`: Display object overview
#'
#' @export
PrevalenceResults <- R6::R6Class(
  "PrevalenceResults",
  public = list(
    #' @description Create new PrevalenceResults object
    #' @param prevalence Data frame with prevalence results
    #' @param incidence Data frame with incidence results
    #' @param drugUsage Data frame with drug usage results
    #' @param metaInfo Data frame with analysis metadata
    #' @param executionId Optional character string for execution tracking (internal use)
    initialize = function(prevalence = NULL,
                         incidence = NULL,
                         drugUsage = NULL,
                         metaInfo = NULL,
                         executionId = NULL) {
      private$.prevalence <- prevalence
      private$.stdPrev <- NULL
      private$.incidence <- incidence
      private$.drugUsage <- drugUsage
      private$.metaInfo <- metaInfo
      private$.executionId <- executionId %||% format(Sys.time(), "%Y%m%d_%H%M%S")
      private$.exportDate <- NULL
      private$.standardizationApplied <- list()
      private$.executedQueries <- list()
    },

    #' @description Export results to directory bundle with manifest
    #' @param outputFolder Character path where bundle directory will be created
    #' @param bundleName Optional name for the bundle directory. Default: auto-generated timestamp
    #' @return Invisibly returns self for chaining
    export = function(outputFolder, bundleName = NULL) {
      if (is.null(outputFolder)) {
        outputFolder <- here::here()
      }

      checkmate::assert_directory_exists(outputFolder)

      # Create bundle directory name
      if (is.null(bundleName)) {
        bundleName <- paste0("prevalence_results_", private$.executionId)
      }

      bundlePath <- file.path(outputFolder, bundleName)
      dir.create(bundlePath, showWarnings = FALSE, recursive = TRUE)

      cli::cat_line()
      cli::cat_rule("Exporting Analysis Results")
      cli::cli_alert_info("Bundle: {bundleName}")

      exportedFiles <- list()

      tryCatch({
        # Export prevalence
        if (!is.null(private$.prevalence) && nrow(private$.prevalence) > 0) {
          prevFile <- file.path(bundlePath, "prevalence.csv")
          readr::write_csv(private$.prevalence, file = prevFile)
          exportedFiles$prevalence <- list(path = "prevalence.csv", rows = nrow(private$.prevalence))
          cli::cli_alert_success("Exported prevalence ({nrow(private$.prevalence)} rows)")
        }

        # Export incidence
        if (!is.null(private$.incidence) && nrow(private$.incidence) > 0) {
          incFile <- file.path(bundlePath, "incidence.csv")
          readr::write_csv(private$.incidence, file = incFile)
          exportedFiles$incidence <- list(path = "incidence.csv", rows = nrow(private$.incidence))
          cli::cli_alert_success("Exported incidence ({nrow(private$.incidence)} rows)")
        }

        # Export drug usage
        if (!is.null(private$.drugUsage) && nrow(private$.drugUsage) > 0) {
          drugFile <- file.path(bundlePath, "drug_usage.csv")
          readr::write_csv(private$.drugUsage, file = drugFile)
          exportedFiles$drugUsage <- list(path = "drug_usage.csv", rows = nrow(private$.drugUsage))
          cli::cli_alert_success("Exported drug usage ({nrow(private$.drugUsage)} rows)")
        }

        # Export standardized prevalence if available
        if (!is.null(private$.stdPrev) && nrow(private$.stdPrev) > 0) {
          stdprevFile <- file.path(bundlePath, "standardized_prevalence.csv")
          readr::write_csv(private$.stdPrev, file = stdprevFile)
          exportedFiles$stdPrev <- list(path = "standardized_prevalence.csv", rows = nrow(private$.stdPrev))
          cli::cli_alert_success("Exported standardized prevalence ({nrow(private$.stdPrev)} rows)")
        }

        # Export metaInfo
        if (!is.null(private$.metaInfo) && nrow(private$.metaInfo) > 0) {
          metaFile <- file.path(bundlePath, "metaInfo.csv")
          readr::write_csv(private$.metaInfo, file = metaFile)
          exportedFiles$metaInfo <- list(path = "metaInfo.csv", rows = nrow(private$.metaInfo))
          cli::cli_alert_success("Exported metaInfo ({nrow(private$.metaInfo)} rows)")
        }

        # Export executed queries
        if (!is.null(private$.executedQueries) && length(private$.executedQueries) > 0) {
          queriesDir <- file.path(bundlePath, "queries")
          dir.create(queriesDir, showWarnings = FALSE, recursive = TRUE)

          executedQueries <- list()
          for (analysisId in names(private$.executedQueries)) {
            sql_string <- private$.executedQueries[[analysisId]]
            sql_checksum <- digest::digest(sql_string, algo = "sha256")

            queryFile <- file.path(queriesDir, paste0("a_", analysisId, "_query.sql"))
            readr::write_file(sql_string, file = queryFile)

            executedQueries[[analysisId]] <- list(
              analysisId = analysisId,
              path = paste0("queries/a_", analysisId, "_query.sql"),
              sql_checksum = sql_checksum
            )
          }

          exportedFiles$executed_queries <- executedQueries
          cli::cli_alert_success("Exported {length(private$.executedQueries)} SQL queries")
        }

        # Create and write manifest
        manifest <- private$.createManifest(exportedFiles)
        manifestFile <- file.path(bundlePath, "manifest.json")
        jsonlite::write_json(manifest, manifestFile, pretty = TRUE)
        cli::cli_alert_success("Manifest created")

        private$.exportDate <- Sys.time()

        cli::cli_alert_success("Bundle exported to {bundlePath}")
        invisible(self)
      }, error = function(e) {
        cli::cli_alert_danger("Export failed: {e$message}")
        stop(e)
      })
    },

    #' @description Apply direct method standardization
    #' @param referencePopulation StandardizationReference object
    #' @param ageMin Numeric minimum age for filtering
    #' @param ageMax Numeric maximum age for filtering
    #' @param ageRightTruncation Numeric age threshold for collapsing
    #' @return New PrevalenceResults object with standardized prevalence
    standardizePrevalence = function(referencePopulation,
                                     ageMin = NULL,
                                     ageMax = NULL,
                                     ageRightTruncation = NULL) {
      if (is.null(private$.prevalence) || nrow(private$.prevalence) == 0) {
        stop("No prevalence data to standardize")
      }

      cli::cat_line()
      cli::cat_rule("Standardizing Prevalence")

      # Call standardization function
      result_df <- standardize_prevalence(
        prevalenceData = private$.prevalence,
        referencePopulation = referencePopulation,
        ageMin = ageMin,
        ageMax = ageMax,
        ageRightTruncation = ageRightTruncation
      )

      cli::cli_alert_success("Standardization complete ({nrow(result_df)} rows)")

      # Store standardized prevalence in stdPrev field
      private$.stdPrev <- result_df

      # Track standardization parameters
      private$.standardizationApplied <- list(
        reference = referencePopulation$name,
        reference_year = referencePopulation$year,
        ageMin = ageMin,
        ageMax = ageMax,
        rightTruncation = ageRightTruncation,
        appliedDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
      )

      invisible(self)
    },

    #' @description Validate data integrity and relationships
    #' @return Logical TRUE if valid, otherwise stops with error
    validate = function() {
      cli::cat_line()
      cli::cli_alert_info("Validating results...")

      # Check crude prevalence structure (if present)
      if (!is.null(private$.prevalence)) {
        # Crude prevalence should have stratified data
        if (nrow(private$.prevalence) > 0) {
          required_cols <- c("analysisId", "spanLabel")
          missing <- setdiff(required_cols, colnames(private$.prevalence))
          if (length(missing) > 0) {
            stop("Prevalence missing required columns: ", paste(missing, collapse = ", "))
          }
        }
      }

      # Check standardized prevalence structure (if present)
      if (!is.null(private$.stdPrev)) {
        if (nrow(private$.stdPrev) > 0) {
          required_cols <- c("analysisId", "spanLabel", "totalNum", "totalDenom", "crudeStat", "stdStat")
          missing <- setdiff(required_cols, colnames(private$.stdPrev))
          if (length(missing) > 0) {
            stop("Standardized prevalence missing columns: ", paste(missing, collapse = ", "))
          }
        }
      }

      # Check metaInfo if present - should match either prevalence or stdPrev
      if (!is.null(private$.metaInfo)) {
        meta_ids <- unique(private$.metaInfo$analysisId)
        
        data_to_check <- if (!is.null(private$.stdPrev) && nrow(private$.stdPrev) > 0) {
          private$.stdPrev
        } else if (!is.null(private$.prevalence) && nrow(private$.prevalence) > 0) {
          private$.prevalence
        } else {
          NULL
        }
        
        if (!is.null(data_to_check)) {
          data_ids <- unique(data_to_check$analysisId)
          unmatched <- setdiff(data_ids, meta_ids)
          if (length(unmatched) > 0) {
            cli::cli_alert_warning(
              "{length(unmatched)} analysisId(s) in data not found in metaInfo"
            )
          }
        }
      }

      cli::cli_alert_success("Validation passed")
      invisible(TRUE)
    },

    #' @description Print summary of results
    #' @return Invisibly returns self
    summary = function() {
      cat("\n=== PrevalenceResults Summary ===\n")
      cat("Execution ID:", private$.executionId, "\n")

      if (!is.null(private$.prevalence)) {
        cat("Prevalence (crude): ", nrow(private$.prevalence), " rows\n", sep = "")
      }

      if (!is.null(private$.stdPrev)) {
        cat("Prevalence (standardized): ", nrow(private$.stdPrev), " rows\n", sep = "")
      }

      if (!is.null(private$.incidence)) {
        cat("Incidence: ", nrow(private$.incidence), " rows\n", sep = "")
      }

      if (!is.null(private$.drugUsage)) {
        cat("Drug Usage: ", nrow(private$.drugUsage), " rows\n", sep = "")
      }

      if (!is.null(private$.metaInfo)) {
        cat("MetaInfo: ", nrow(private$.metaInfo), " rows\n", sep = "")
      }

      if (length(private$.standardizationApplied) > 0) {
        cat("Standardization: ", private$.standardizationApplied$reference, "\n", sep = "")
      }

      cat("\n")
      invisible(self)
    },

    #' @description Print object
    #' @return Invisibly returns self
    print = function() {
      self$summary()
    },

    #' @description Display executed SQL query for an analysis
    #' @param analysisId Character or numeric analysis ID
    #' @return Invisibly returns the SQL string
    show_query = function(analysisId) {
      if (is.null(private$.executedQueries) || length(private$.executedQueries) == 0) {
        cli::cli_alert_warning("No executed queries recorded")
        return(invisible(NULL))
      }

      analysisId_key <- as.character(analysisId)

      if (!analysisId_key %in% names(private$.executedQueries)) {
        available <- paste(names(private$.executedQueries), collapse = ", ")
        cli::cli_alert_warning(
          "Analysis {analysisId} not found. Available: {available}"
        )
        return(invisible(NULL))
      }

      sql_string <- private$.executedQueries[[analysisId_key]]

      cat("\n")
      cli::cli_rule("SQL Query for Analysis {analysisId}")
      cat(sql_string)
      cat("\n\n")

      invisible(sql_string)
    },

    #' @description Add executed query for an analysis (internal use)
    #' @param analysisId Character or numeric analysis ID
    #' @param sql_string Character SQL query string
    #' @return Invisibly returns self
    .addExecutedQuery = function(analysisId, sql_string) {
      private$.executedQueries[[as.character(analysisId)]] <- sql_string
      invisible(self)
    }
    
    # Future method for interactive exploration (e.g., Shiny app)
    # explore = function() {
    #   shiny_explore_prevalence_results(self)
    # }
  ),

  active = list(
    #' @field prevalence Data frame with prevalence results
    prevalence = function(value) {
      if (missing(value)) {
        return(private$.prevalence)
      } else {
        private$.prevalence <- value
      }
    },

    #' @field incidence Data frame with incidence results
    incidence = function(value) {
      if (missing(value)) {
        return(private$.incidence)
      } else {
        private$.incidence <- value
      }
    },

    #' @field drugUsage Data frame with drug usage results
    drugUsage = function(value) {
      if (missing(value)) {
        return(private$.drugUsage)
      } else {
        private$.drugUsage <- value
      }
    },

    #' @field metaInfo Data frame with analysis metadata
    metaInfo = function(value) {
      if (missing(value)) {
        return(private$.metaInfo)
      } else {
        private$.metaInfo <- value
      }
    },

    #' @field stdPrev Data frame with standardized prevalence results
    stdPrev = function(value) {
      if (missing(value)) {
        return(private$.stdPrev)
      } else {
        private$.stdPrev <- value
      }
    },

    #' @field standardizationApplied List of standardization parameters (read-only)
    standardizationApplied = function(value) {
      if (missing(value)) {
        return(private$.standardizationApplied)
      } else {
        private$.standardizationApplied <- value
      }
    }
  ),

  private = list(
    .prevalence = NULL,
    .stdPrev = NULL,
    .incidence = NULL,
    .drugUsage = NULL,
    .metaInfo = NULL,
    .standardizationApplied = NULL,
    .executionId = NULL,
    .exportDate = NULL,
    .executedQueries = NULL,

    # Create manifest JSON for provenance tracking
    .createManifest = function(exportedFiles) {
      list(
        export_date = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        cohortprevalence_version = as.character(utils::packageVersion("CohortPrevalence")),
        r_version = paste0(R.version$major, ".", R.version$minor),
        execution_id = private$.executionId,
        files = exportedFiles,
        standardization_applied = if (length(private$.standardizationApplied) > 0) {
          private$.standardizationApplied
        } else {
          list()
        }
      )
    }
  )
)


#' Load PrevalenceResults from Bundle
#'
#' @description
#' Load a PrevalenceResults object from an exported bundle directory containing
#' CSV files and manifest.json
#'
#' @param bundlePath Character path to bundle directory
#'
#' @return PrevalenceResults object with data restored from bundle
#'
#' @export
loadPrevalenceResults <- function(bundlePath) {
  checkmate::assert_directory_exists(bundlePath)

  cli::cat_line()
  cli::cat_rule("Loading PrevalenceResults Bundle")
  cli::cli_alert_info("Bundle: {basename(bundlePath)}")

  tryCatch({
    # Load manifest
    manifestFile <- file.path(bundlePath, "manifest.json")
    if (!file.exists(manifestFile)) {
      stop("manifest.json not found in bundle")
    }

    manifest <- jsonlite::read_json(manifestFile)
    cli::cli_alert_success("Manifest loaded")

    # Load data files
    prevalence <- NULL
    stdPrev <- NULL
    incidence <- NULL
    drugUsage <- NULL
    metaInfo <- NULL

    if ("prevalence" %in% names(manifest$files)) {
      prevalence <- readr::read_csv(
        file.path(bundlePath, manifest$files$prevalence$path),
        show_col_types = FALSE
      )
      cli::cli_alert_success("Loaded prevalence ({nrow(prevalence)} rows)")
    }

    if ("stdPrev" %in% names(manifest$files)) {
      stdPrev <- readr::read_csv(
        file.path(bundlePath, manifest$files$stdPrev$path),
        show_col_types = FALSE
      )
      cli::cli_alert_success("Loaded standardized prevalence ({nrow(stdPrev)} rows)")
    }

    if ("incidence" %in% names(manifest$files)) {
      incidence <- readr::read_csv(
        file.path(bundlePath, manifest$files$incidence$path),
        show_col_types = FALSE
      )
      cli::cli_alert_success("Loaded incidence ({nrow(incidence)} rows)")
    }

    if ("drugUsage" %in% names(manifest$files)) {
      drugUsage <- readr::read_csv(
        file.path(bundlePath, manifest$files$drugUsage$path),
        show_col_types = FALSE
      )
      cli::cli_alert_success("Loaded drug usage ({nrow(drugUsage)} rows)")
    }

    if ("metaInfo" %in% names(manifest$files)) {
      metaInfo <- readr::read_csv(
        file.path(bundlePath, manifest$files$metaInfo$path),
        show_col_types = FALSE
      )
      cli::cli_alert_success("Loaded metaInfo ({nrow(metaInfo)} rows)")
    }

    # Load executed queries
    executedQueries <- list()
    if ("executed_queries" %in% names(manifest$files)) {
      queries_info <- manifest$files$executed_queries
      if (is.list(queries_info) && length(queries_info) > 0) {
        # Handle both single query and multiple queries
        if (!is.null(names(queries_info[[1]]))) {
          # Single query (list with names)
          queries_info <- list(queries_info)
        }

        for (query_entry in queries_info) {
          analysisId <- query_entry$analysisId
          queryFile <- file.path(bundlePath, query_entry$path)
          if (file.exists(queryFile)) {
            sql_string <- readr::read_file(queryFile)
            executedQueries[[as.character(analysisId)]] <- sql_string
          }
        }

        if (length(executedQueries) > 0) {
          cli::cli_alert_success("Loaded {length(executedQueries)} SQL queries")
        }
      }
    }

    # Create PrevalenceResults object
    results <- PrevalenceResults$new(
      prevalence = prevalence,
      incidence = incidence,
      drugUsage = drugUsage,
      metaInfo = metaInfo,
      executionId = manifest$execution_id
    )

    # Restore standardized prevalence if present
    if (!is.null(stdPrev)) {
      results$stdPrev <- stdPrev
    }

    # Restore standardization info if present
    if (length(manifest$standardization_applied) > 0) {
      results$standardizationApplied <- manifest$standardization_applied
    }

    # Restore executed queries
    if (length(executedQueries) > 0) {
      for (analysisId in names(executedQueries)) {
        results$.addExecutedQuery(analysisId, executedQueries[[analysisId]])
      }
    }

    cli::cli_alert_success("Bundle loaded successfully")
    cli::cat_line()

    return(results)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to load bundle: {e$message}")
    stop(e)
  })
}


# ============================================================================
# Standardization Functions
# ============================================================================

#' Direct Method Standardization of Prevalence Rates
#'
#' @description
#' Applies direct method age-sex standardization to crude prevalence data
#' using a reference population. Supports demographic bounds matching and
#' age truncation for real-world database patterns (e.g., Optum age masking).
#'
#' @param prevalenceData Data frame with stratified prevalence data.
#'   Required columns: age, gender, numerator, denominator
#'
#' @param referencePopulation StandardizationReference object defining
#'   the standard population for weighting
#'
#' @param ageMin Numeric. Minimum age for filtering reference population.
#'   If NULL (default), no lower bound applied.
#'
#' @param ageMax Numeric. Maximum age for filtering reference population.
#'   If NULL (default), no upper bound applied.
#'
#' @param ageRightTruncation Numeric. Optional age threshold for collapsing
#'   ages >= threshold into single "threshold+" group. Useful for handling
#'   database age masking (e.g., Optum: 70+ all collapsed to "70+").
#'   If NULL (default), no truncation applied.
#'
#' @details
#' Algorithm (direct method standardization):
#'
#' **Step 1**: Validate & apply right truncation to reference population
#'   - If ageRightTruncation specified: validate that threshold is at group boundary (fails fast if mid-group)
#'   - Apply truncation + demographic bounds to reference
#'
#' **Step 2**: Filter & prepare crude prevalence
#'   - Convert gender concept IDs (8532→Female, 8507→Male)
#'   - Convert age to numeric, apply ageMin/ageMax filters
#'   - Apply right truncation: ages >= threshold → "threshold+"
#'
#' **Step 3**: Map crude ages → reference group labels
#'   - "N+" values pass through unchanged
#'   - For single-year references: zero-pad to "000", "001", ...
#'   - For grouped references: lookup which age group each value falls into
#'
#' **Step 4**: Summarize by mapped age-gender groups
#'   - Group by analysisId, spanLabel, age, gender; sum numerator and denominator
#'   - Calculate crude rate per 100,000
#'
#' **Step 5**: Re-filter reference to matched crude age-gender combos only, renormalize weights
#'   - Filter ref_base to only age-gender pairs present in prevalence data
#'   - Renormalize weights to sum to 1.0 within matched subset
#'
#' **Step 6**: Join, standardize, and aggregate
#'   - inner_join crude rates to reference weights by (age, gender)
#'   - Calculate stdValue = rate × weight for each stratum
#'   - Aggregate: stdStat = sum(stdValue) per analysisId + spanLabel
#'   - Output includes totalNum, totalDenom, crudeStat, and stdStat
#'
#' @return
#' Data frame with standardized prevalence results. One row per analysis-span combination.
#'   Columns:
#'   - analysisId: Character, unique identifier for the analysis
#'   - spanLabel: Character, label for the time span/period
#'   - totalNum: Integer, total numerator (cases) across all strata
#'   - totalDenom: Integer, total denominator (population) across all strata
#'   - crudeStat: Numeric, crude prevalence rate (per 100,000)
#'   - stdStat: Numeric, age-sex standardized prevalence rate (per 100,000)
#'   - reference_name: Character, name of the reference population used
#'   - reference_year: Integer, year of the reference population
#'
#' @keywords internal
standardize_prevalence <- function(
    prevalenceData,
    referencePopulation,
    ageMin = NULL,
    ageMax = NULL,
    ageRightTruncation = NULL) {

  # Validate inputs
  if (!inherits(referencePopulation, "StandardizationReference")) {
    stop("referencePopulation must be a StandardizationReference object")
  }

  if (!is.data.frame(prevalenceData)) {
    stop("prevalenceData must be a data frame")
  }

  # Validate required columns (standard CohortPrevalence output)
  required_cols <- c("analysisId", "spanLabel", "age", "gender", "numerator", "denominator")
  missing_cols <- setdiff(required_cols, colnames(prevalenceData))
  if (length(missing_cols) > 0) {
    stop(
      "prevalenceData must be output from CohortPrevalence functions. Missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # ── Step 1: Validate & apply right truncation to reference ──
  if (!is.null(ageRightTruncation)) {
    # Fail fast if truncation is invalid
    referencePopulation$validateRightTruncation(ageRightTruncation)
    # Get reference with truncation + bounds applied
    ref_base <- referencePopulation$getAdjustedReference(
      ageMin = ageMin,
      ageMax = ageMax,
      rightTruncation = ageRightTruncation
    )
  } else {
    # Get reference with just bounds applied
    ref_base <- referencePopulation$getFilteredReference(
      ageMin = ageMin,
      ageMax = ageMax
    )
  }

  # ── Step 2: Filter & prepare crude prevalence ──
  prev_clean <- prevalenceData |>
    dplyr::select(
      analysisId, spanLabel, age, gender, numerator, denominator
    ) |>
    dplyr::filter(
      gender %in% c(8532, 8507)
    ) |>
    dplyr::mutate(
      gender = dplyr::case_when(
        gender == 8532 ~ "Female",
        gender == 8507 ~ "Male",
        TRUE ~ as.character(gender)
      ),
      age = as.numeric(age)
    ) |>
    dplyr::filter(
      is.null(ageMin) || age >= ageMin,
      is.null(ageMax) || age <= ageMax
    ) |>
    dplyr::mutate(
      # Apply right truncation: ages >= threshold become "threshold+"
      age = ifelse(
        !is.na(ageRightTruncation) & age >= ageRightTruncation,
        paste0(ageRightTruncation, "+"),
        as.character(age)
      )
    )

  # ── Step 3: Map crude ages → reference group labels ──
  # Handles: "N+" pass-through, numeric → single-year zero-pad, numeric → grouped lookup
  prev_clean$age <- referencePopulation$mapAgesToReference(prev_clean$age)

  # ── Step 4: Summarize by mapped groups ──
  prev_grouped <- prev_clean |>
    dplyr::group_by(analysisId, spanLabel, age, gender) |>
    dplyr::summarize(
      numerator = sum(numerator),
      denominator = sum(denominator),
      .groups = "keep"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stat = (numerator / denominator) * 100000
    )

  # ── Step 5: Re-filter reference to matched crude age-gender combos, renormalize ──
  matched_ages   <- unique(prev_grouped$age)
  matched_gender <- unique(prev_grouped$gender)
  ref_adjusted <- ref_base |>
    dplyr::filter(
      age %in% matched_ages,
      gender %in% matched_gender
    ) |>
    dplyr::mutate(
      weight = population / sum(population)
    )

  # ── Step 6: Join & standardize ──
  # inner_join ensures only matched groups contribute to standardized rate
  standardized_data <- prev_grouped |>
    dplyr::inner_join(
      ref_adjusted |> dplyr::select(age, gender, weight),
      by = c("age", "gender")
    ) |>
    dplyr::mutate(
      stdValue = stat * weight
    ) |>
    dplyr::group_by(analysisId, spanLabel) |>
    dplyr::summarize(
      totalNum = sum(numerator),
      totalDenom = sum(denominator),
      crudeStat = (totalNum / totalDenom) * 100000,
      stdStat = sum(stdValue),
      .groups = "keep"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      reference_name = referencePopulation$name,
      reference_year = referencePopulation$year
    )

  return(standardized_data)
}


#' Calculate Wilson Score Exact Confidence Interval
#'
#' @param rate Numeric. The rate estimate (between 0 and 1)
#' @param total_n Numeric. Total denominator size
#'
#' @return List with lower and upper CI bounds
#'
#' @keywords internal
.calculate_wilson_ci <- function(rate, total_n, conf_level = 0.95) {
  # Wilson score exact method for CI
  # Following:
  # Newcombe, R.G. (1998). Two-sided confidence intervals for the single proportion:
  # Comparison of seven methods. Statistics in Medicine, 17, 857-872.

  # For standardized rate with heterogeneous strata, we estimate CI using
  # the crude rate as proxy with effective denominator
  q <- (1 - conf_level) / 2
  z_alpha_2 <- qnorm(q)  # 95% CI

  # Convert to counts for Wilson formula
  successes <- rate * total_n
  failures <- total_n - successes

  # Wilson score CI calculation
  denominator <- 1 + (z_alpha_2^2 / total_n)

  center <- (successes + (z_alpha_2^2 / 2)) / n_eff

  margin <- z_alpha_2 * sqrt((successes * failures) / n_eff^2 + (z_alpha_2^2) / (4 * n_eff^2)) / n_eff

  lower <- (center - margin) / denominator
  upper <- (center + margin) / denominator

  # Bound to [0, 1]
  lower <- max(0, lower)
  upper <- min(1, upper)

  ll <- list(lower = lower, upper = upper)
  return(ll)
}


#' StandardizationReference R6 Class
#'
#' @description
#' Encapsulates a reference population for direct method standardization.
#' Stores population weights by age and gender with metadata.
#'
#' @details
#' The StandardizationReference class provides a structured way to manage
#' reference populations used for age-sex standardization. It validates
#' data structure, auto-calculates weights as proportions, and provides
#' methods for accessing and manipulating reference data.
#'
#' @examples
#' \dontrun{
#'   # Create a custom reference
#'   ref_data <- data.frame(
#'     age = c("0", "1", "2", "100+", "0", "1", "2", "100+"),
#'     gender = c(rep("Male", 4), rep("Female", 4)),
#'     population = c(100000, 95000, 92000, 50000, 98000, 93000, 90000, 48000)
#'   )
#'
#'   my_ref <- StandardizationReference$new(
#'     name = "Custom Reference",
#'     country = "Custom",
#'     year = 2020L,
#'     source = "My Data",
#'     data = ref_data
#'   )
#'
#'   # View reference
#'   my_ref$viewReference()
#'
#'   # Get total population
#'   my_ref$getTotalPopulation()
#'
#'   # Filter to age range
#'   filtered <- my_ref$getFilteredReference(ageMin = 18, ageMax = 65)
#' }
#'
#' @export
StandardizationReference <- R6::R6Class(
  "StandardizationReference",
  public = list(
    #' @description Create a new StandardizationReference
    #'
    #' @param name Character. Name of reference (e.g., "USA Census 2020")
    #' @param country Character. Country/region name
    #' @param year Integer. Reference year
    #' @param source Character. Data source description
    #' @param reference Character. URL or reference to access the source data
    #' @param data Data frame with columns: age, gender, population
    #'
    #' @return A new StandardizationReference object
    initialize = function(name, country, year, source, data, reference = NULL) {
      # Validate inputs
      if (!is.data.frame(data)) {
        stop("'data' must be a data frame")
      }

      required_cols <- c("age", "gender", "population")
      missing_cols <- setdiff(required_cols, colnames(data))
      if (length(missing_cols) > 0) {
        stop(
          "Data frame must have columns: ",
          paste(required_cols, collapse = ", "),
          ". Missing: ",
          paste(missing_cols, collapse = ", ")
        )
      }

      # Check for missing values in required columns
      if (any(is.na(data$age)) || any(is.na(data$gender)) || any(is.na(data$population))) {
        stop("Data cannot contain NA values in age, gender, or population columns")
      }

      # Check population is numeric and positive
      if (!is.numeric(data$population) || any(data$population < 0)) {
        stop("Population column must be numeric and non-negative")
      }

      # Store metadata in private fields
      private$.name <- name
      private$.country <- country
      private$.year <- as.integer(year)
      private$.source <- source
      private$.reference <- reference

      # Calculate weights and store data
      private$.data <- data |>
        dplyr::mutate(
          age = as.character(age),
          gender = as.character(gender),
          population = as.numeric(population)
        ) |>
        dplyr::mutate(
          weight = population / sum(population)
        ) |>
        dplyr::arrange(gender, age)
    },

    #' @description View the reference population data
    viewReference = function() {
      cat("\n=== ", private$.name, " ===\n", sep = "")
      cat("Country:  ", private$.country, "\n", sep = "")
      cat("Year:     ", private$.year, "\n", sep = "")
      cat("Source:   ", private$.source, "\n", sep = "")
      if (!is.null(private$.reference)) {
        cat("Reference:", private$.reference, "\n", sep = " ")
      }
      cat("\nData (first 10 rows):\n")
      print(head(private$.data, 10))
      cat(
        "\nTotal rows: ", nrow(private$.data),
        " | Total population: ",
        format(sum(private$.data$population), big.mark = ","),
        "\n\n",
        sep = ""
      )
    },

    #' @description Get the full reference data frame
    #' @return Data frame with columns: age, gender, population, weight
    getData = function() {
      private$.data
    },

    #' @description Get total population
    #' @return Numeric value
    getTotalPopulation = function() {
      sum(private$.data$population)
    },

    #' @description Get unique ages
    #' @return Character vector of age values
    getAgeValues = function() {
      sort(unique(private$.data$age))
    },

    #' @description Get unique genders
    #' @return Character vector of gender values
    getGenderValues = function() {
      sort(unique(private$.data$gender))
    },

    #' @description Get metadata as list
    #' @return List with name, country, year, source, reference
    getMetadata = function() {
      list(
        name = private$.name,
        country = private$.country,
        year = private$.year,
        source = private$.source,
        reference = private$.reference
      )
    },

    #' @description Filter reference to demographic bounds and re-normalize weights
    #'
    #' @param ageMin Minimum age (inclusive)
    #' @param ageMax Maximum age (inclusive)
    #'
    #' @return Data frame filtered and re-normalized to bounds
    getFilteredReference = function(ageMin = NULL, ageMax = NULL) {
      result <- private$.data

      # Convert ages to numeric for comparison
      if (!is.null(ageMin) || !is.null(ageMax)) {
        ages_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", result$age)))

        if (!is.null(ageMin)) {
          result <- result[ages_numeric >= ageMin | is.na(ages_numeric), ]
        }

        if (!is.null(ageMax)) {
          result <- result[ages_numeric <= ageMax | is.na(ages_numeric), ]
        }
      }

      # Re-normalize weights to filtered subset
      result <- result |>
        dplyr::mutate(
          weight = population / sum(population)
        )

      result
    },

    #' @description Apply both bounds filtering and age truncation
    #'
    #' @param ageMin Minimum age (inclusive)
    #' @param ageMax Maximum age (inclusive)
    #' @param rightTruncation Age threshold for truncation (optional)
    #'
    #' @return Data frame with both transformations applied
    getAdjustedReference = function(ageMin = NULL, ageMax = NULL, rightTruncation = NULL) {
      result <- private$.data

      # First apply age truncation if specified
      if (!is.null(rightTruncation)) {
        result <- result |>
          dplyr::mutate(
            age_numeric = suppressWarnings(as.numeric(gsub("\\+", "", age)))
          ) |>
          dplyr::mutate(
            age = dplyr::case_when(
              is.na(age_numeric) ~ age,
              age_numeric >= rightTruncation ~ paste0(rightTruncation, "+"),
              TRUE ~ age
            )
          ) |>
          dplyr::select(-age_numeric) |>
          dplyr::group_by(age, gender) |>
          dplyr::summarise(
            population = sum(population),
            .groups = "drop"
          )
      }

      # Then apply demographic bounds
      if (!is.null(ageMin) || !is.null(ageMax)) {
        ages_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", result$age)))

        if (!is.null(ageMin)) {
          result <- result[ages_numeric >= ageMin | is.na(ages_numeric), ]
        }

        if (!is.null(ageMax)) {
          result <- result[ages_numeric <= ageMax | is.na(ages_numeric), ]
        }
      }

      # Re-normalize weights
      result <- result |>
        dplyr::mutate(
          weight = population / sum(population)
        )

      result
    },

    #' @description Validate right truncation point against reference age groups
    #'
    #' For single-year references, any integer age is valid. For grouped references,
    #' the truncation point must fall exactly at the start of an age group (e.g.,
    #' "85" for "85+" group, "80" for "80-84" group). Fails fast if truncation
    #' falls mid-group.
    #'
    #' @param rightTruncation Numeric. Age threshold to validate.
    #' @return Numeric. The validated truncation point (or stop with error)
    validateRightTruncation = function(rightTruncation) {
      if (!is.numeric(rightTruncation) || length(rightTruncation) != 1) {
        stop("rightTruncation must be a single numeric value")
      }

      ref_ages <- unique(private$.data$age)

      has_range  <- any(grepl("-", ref_ages))
      has_plus   <- any(grepl("\\+", ref_ages))
      has_text   <- any(grepl("[A-Za-z]", ref_ages))
      is_grouped <- has_range || has_plus || has_text

      if (!is_grouped) {
        # Single-year reference: any integer is valid
        return(rightTruncation)
      }

      # For grouped references, truncation must be at group start
      .parse_label <- function(label) {
        if (grepl("\\+", label)) {
          as.numeric(gsub("\\+", "", label))
        } else if (grepl("-", label)) {
          parts <- as.numeric(strsplit(label, "-")[[1]])
          parts[1]
        } else if (grepl("(?i)under", label)) {
          0
        } else {
          as.numeric(label)
        }
      }

      group_starts <- sapply(ref_ages, .parse_label)

      if (!(rightTruncation %in% group_starts)) {
        # Find which group contains this age and provide helpful error
        .parse_full <- function(label) {
          if (grepl("\\+", label)) {
            list(min = as.numeric(gsub("\\+", "", label)), max = Inf, label = label)
          } else if (grepl("-", label)) {
            parts <- as.numeric(strsplit(label, "-")[[1]])
            list(min = parts[1], max = parts[2], label = label)
          } else if (grepl("(?i)under", label)) {
            num <- as.numeric(gsub("(?i)under\\s*", "", label))
            list(min = 0, max = num - 1, label = label)
          } else {
            num <- as.numeric(label)
            list(min = num, max = num, label = label)
          }
        }

        groups <- lapply(ref_ages, .parse_full)
        conflicting <- NULL
        for (g in groups) {
          if (rightTruncation >= g$min && rightTruncation <= g$max) {
            conflicting <- g
            break
          }
        }

        if (!is.null(conflicting)) {
          stop(
            "Cannot truncate at age ", rightTruncation, ". ",
            "It falls within group '", conflicting$label, "' (",
            conflicting$min, "-", conflicting$max, "). ",
            "Valid truncation points: ", paste(sort(group_starts), collapse = ", ")
          )
        } else {
          stop(
            "Age ", rightTruncation, " is outside reference population range. ",
            "Valid truncation points: ", paste(sort(group_starts), collapse = ", ")
          )
        }
      }

      return(rightTruncation)
    },

    #' @description Get valid truncation points for this reference
    #'
    #' Returns a numeric vector of all valid age thresholds where right truncation
    #' is allowed (start of each age group for grouped references, all ages for
    #' single-year references).
    #'
    #' @return Numeric vector of valid truncation points
    getValidTruncationPoints = function() {
      ref_ages <- unique(private$.data$age)

      has_range  <- any(grepl("-", ref_ages))
      has_plus   <- any(grepl("\\+", ref_ages))
      has_text   <- any(grepl("[A-Za-z]", ref_ages))
      is_grouped <- has_range || has_plus || has_text

      if (!is_grouped) {
        # Single-year: all unique numeric ages are valid
        return(sort(as.numeric(ref_ages)))
      }

      # Grouped: only group starts are valid
      .parse_label <- function(label) {
        if (grepl("\\+", label)) {
          as.numeric(gsub("\\+", "", label))
        } else if (grepl("-", label)) {
          parts <- as.numeric(strsplit(label, "-")[[1]])
          parts[1]
        } else if (grepl("(?i)under", label)) {
          0
        } else {
          as.numeric(label)
        }
      }

      sort(unique(sapply(ref_ages, .parse_label)))
    },

    #' @description Map single-year ages to reference age group labels
    #'
    #' Converts a character/numeric vector of ages (some possibly \"N+\" labels from
    #' right truncation) to the reference's age label format so prevalence data can
    #' be joined to reference weights by (age, gender).
    #'
    #' Works generically for any reference type:
    #'   - Already-truncated \"N+\" values pass through unchanged
    #'   - Single-year references (Decennial Census): zero-pad numeric values
    #'   - Grouped references (ACS, WHO): lookup which group each numeric age falls into
    #'
    #' @param age_values Character/numeric vector. May contain \"N+\" suffix values or numeric strings.
    #' @return Character vector of reference-formatted age labels
    mapAgesToReference = function(age_values) {
      # Convert to character if not already
      age_values <- as.character(age_values)

      ref_ages <- unique(private$.data$age)

      has_range  <- any(grepl("-", ref_ages))
      has_plus   <- any(grepl("\\+", ref_ages))
      has_text   <- any(grepl("[A-Za-z]", ref_ages))
      is_grouped <- has_range || has_plus || has_text

      # Process each age value
      result <- sapply(age_values, function(age_str) {
        # If already contains "+", it's already truncated — pass through
        if (grepl("\\+", age_str)) {
          return(age_str)
        }

        # Convert to numeric for mapping
        age_num <- suppressWarnings(as.numeric(age_str))
        if (is.na(age_num)) return(NA_character_)

        if (!is_grouped) {
          # Single-year reference: zero-pad to "000", "001", etc.
          return(sprintf("%03d", age_num))
        }

        # Grouped reference: lookup which group this age falls into
        .parse_label <- function(label) {
          if (grepl("\\+", label)) {
            list(min = as.numeric(gsub("\\+", "", label)), max = Inf)
          } else if (grepl("-", label)) {
            parts <- as.numeric(strsplit(label, "-")[[1]])
            list(min = parts[1], max = parts[2])
          } else if (grepl("(?i)under", label)) {
            num <- as.numeric(gsub("(?i)under\\s*", "", label))
            list(min = 0, max = num - 1)
          } else {
            num <- as.numeric(label)
            list(min = num, max = num)
          }
        }

        parsed <- lapply(ref_ages, .parse_label)
        lookup <- data.frame(
          age_label = ref_ages,
          min_age = sapply(parsed, `[[`, "min"),
          max_age = sapply(parsed, `[[`, "max"),
          stringsAsFactors = FALSE
        )

        idx <- which(age_num >= lookup$min_age & age_num <= lookup$max_age)
        if (length(idx) == 0) return(NA_character_)
        return(lookup$age_label[idx[1]])
      }, USE.NAMES = FALSE)

      unname(result)
    }
  ),

  private = list(
    .data = NULL,
    .name = NULL,
    .country = NULL,
    .year = NULL,
    .source = NULL,
    .reference = NULL
  ),

  active = list(
    #' @field name Name of the reference population (active)
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      }
      private$.name <- value
    },

    #' @field country Country or region (active)
    country = function(value) {
      if (missing(value)) {
        return(private$.country)
      }
      private$.country <- value
    },

    #' @field year Year of reference (numeric) (active)
    year = function(value) {
      if (missing(value)) {
        return(private$.year)
      }
      private$.year <- as.integer(value)
    },

    #' @field source Source/citation (active)
    source = function(value) {
      if (missing(value)) {
        return(private$.source)
      }
      private$.source <- value
    },

    #' @field reference URL or reference to access source data (active)
    reference = function(value) {
      if (missing(value)) {
        return(private$.reference)
      }
      private$.reference <- value
    }
  )
)
