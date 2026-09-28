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
        cli::cli_abort(conditionMessage(e), .parent = e)
      })
    },

    #' @description Apply direct method standardization
    #' @param referencePopulation StandardizationReference object
    #' @param ageMin Deprecated. Numeric minimum age for filtering. Demographic
    #'   eligibility should be applied when defining the analysis.
    #' @param ageMax Deprecated. Numeric maximum age for filtering. Demographic
    #'   eligibility should be applied when defining the analysis.
    #' @param ageRightTruncation Numeric age threshold for collapsing
    #' @return New PrevalenceResults object with standardized prevalence
    standardizePrevalence = function(referencePopulation,
                                     ageMin = NULL,
                                     ageMax = NULL,
                                     ageRightTruncation = NULL) {

      if (!is.null(ageMin) || !is.null(ageMax)) {
        cli::cli_warn(c(
          "`ageMin` and `ageMax` are deprecated for standardization.",
          "i" = "Set demographic eligibility on the analysis; these bounds are retained temporarily for compatibility."
        ))
      }

      if (is.null(private$.prevalence) || nrow(private$.prevalence) == 0) {
        cli::cli_abort("No prevalence data to standardize")
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
            cli::cli_abort(c(
              "Prevalence is missing required columns:",
              stats::setNames(missing, rep("x", length(missing)))
            ))
          }
        }
      }

      # Check standardized prevalence structure (if present)
      if (!is.null(private$.stdPrev)) {
        if (nrow(private$.stdPrev) > 0) {
          required_cols <- c("analysisId", "spanLabel", "totalNum", "totalDenom", "crudeStat", "stdStat")
          missing <- setdiff(required_cols, colnames(private$.stdPrev))
          if (length(missing) > 0) {
            cli::cli_abort(c(
              "Standardized prevalence is missing required columns:",
              stats::setNames(missing, rep("x", length(missing)))
            ))
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
      cli::cli_abort("manifest.json not found in bundle")
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
    cli::cli_abort(conditionMessage(e), .parent = e)
  })
}


# ============================================================================
# Standardization Functions
# ============================================================================

#' Direct Method Standardization of Prevalence Rates
#'
#' @description
#' Applies direct method age-sex standardization to crude prevalence data
#' using a reference population. Analysis-specific demographic eligibility is
#' represented by the age/gender strata in `prevalenceData`. Age truncation is
#' supported for real-world database patterns (e.g., Optum age masking).
#'
#' @param prevalenceData Data frame with stratified prevalence data.
#'   Required columns: age, gender, numerator, denominator
#'
#' @param referencePopulation StandardizationReference object defining
#'   the standard population for weighting
#'
#' @param ageMin Deprecated. Numeric minimum age for filtering. If supplied,
#'   it is temporarily applied for compatibility; define eligibility on the
#'   analysis instead.
#'
#' @param ageMax Deprecated. Numeric maximum age for filtering. If supplied,
#'   it is temporarily applied for compatibility; define eligibility on the
#'   analysis instead.
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
#'   - Apply truncation to reference; legacy age bounds are deprecated
#'
#' **Step 2**: Filter & prepare crude prevalence
#'   - Convert gender concept IDs (8532→Female, 8507→Male)
#'   - Derive analysis-specific support from observed age/gender strata
#'   - Apply deprecated ageMin/ageMax arguments only when supplied
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
#' **Step 5**: Build matched reference weights separately for each analysis
#'   - Match reference age-gender pairs to each analysis's observed strata
#'   - Renormalize weights to sum to 1.0 within each analysis
#'   - Require each analysis-span to contain the same strata; fail on gaps
#'
#' **Step 6**: Join, standardize, and aggregate
#'   - Join crude rates to analysis-specific reference weights by analysisId, age, and gender
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
    cli::cli_abort("referencePopulation must be a StandardizationReference object")
  }

  if (!is.data.frame(prevalenceData)) {
    cli::cli_abort("prevalenceData must be a data frame")
  }

  # Validate required columns (standard CohortPrevalence output)
  required_cols <- c("analysisId", "spanLabel", "age", "gender", "numerator", "denominator")
  missing_cols <- setdiff(required_cols, colnames(prevalenceData))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "prevalenceData is missing required columns:",
      stats::setNames(missing_cols, rep("x", length(missing_cols)))
    ))
  }

  supported_gender_ids <- c("8507", "8532")
  prevalence_gender_ids <- as.character(prevalenceData$gender)
  unsupported_gender_ids <- setdiff(
    unique(prevalence_gender_ids),
    supported_gender_ids
  )

  if (length(unsupported_gender_ids) > 0) {
    unsupported_gender_labels <- unsupported_gender_ids
    unsupported_gender_labels[is.na(unsupported_gender_labels)] <- "<NA>"

    cli::cli_abort(c(
      "prevalenceData contains unsupported gender concept IDs:",
      stats::setNames(
        unsupported_gender_labels,
        rep("x", length(unsupported_gender_labels))
      ),
      "i" = "Supported OHDSI gender concept IDs are 8507 (Male) and 8532 (Female)."
    ))
  }

  # ── Step 1: Validate & apply right truncation to reference ──
  if (!is.null(ageRightTruncation)) {
    # Fail fast if truncation is invalid
    referencePopulation$validateRightTruncation(ageRightTruncation)
    # Get reference with age truncation applied.
    ref_base <- referencePopulation$getAdjustedReference(
      rightTruncation = ageRightTruncation
    )
  } else {
    # Use the full reference; matched strata and weights are selected per analysis below.
    ref_base <- referencePopulation$getData()
  }

  # ── Step 2: Filter & prepare crude prevalence ──
  prev_clean <- prevalenceData |>
    dplyr::select(
      analysisId, spanLabel, age, gender, numerator, denominator
    ) |>
    dplyr::mutate(
      gender = as.character(gender),
      gender = dplyr::case_when(
        gender == 8532 ~ "Female",
        gender == 8507 ~ "Male",
        TRUE ~ as.character(gender)
      ),
      age = as.numeric(age)
    )

  # Apply legacy age bounds only when supplied; otherwise retain all
  # analysis-eligible strata already present in prevalenceData.
  age_is_in_bounds <- rep(TRUE, nrow(prev_clean))

  if (!is.null(ageMin)) {
    age_is_in_bounds <- age_is_in_bounds & prev_clean$age >= ageMin
  }

  if (!is.null(ageMax)) {
    age_is_in_bounds <- age_is_in_bounds & prev_clean$age <= ageMax
  }

  prev_clean <- prev_clean |>
    dplyr::filter(age_is_in_bounds)

  # Apply right truncation outside the dplyr pipeline.
  if (is.null(ageRightTruncation)) {
    prev_clean$age <- as.character(prev_clean$age)
  } else {
    prev_clean$age <- dplyr::if_else(
      prev_clean$age >= ageRightTruncation,
      paste0(ageRightTruncation, "+"),
      as.character(prev_clean$age)
    )
  }

  # ── Step 3: Map crude ages → reference group labels ──
  ages_before_mapping <- prev_clean$age
  mapped_ages <- referencePopulation$mapAgesToReference(
    age_values = ages_before_mapping,
    rightTruncation = ageRightTruncation
  )
  unmapped_ages <- is.na(mapped_ages)

  if (any(unmapped_ages)) {
    unmapped_description <- paste0(
      "analysisId=", prev_clean$analysisId[unmapped_ages],
      ", spanLabel=", prev_clean$spanLabel[unmapped_ages],
      ", age=", ages_before_mapping[unmapped_ages]
    )
    cli::cli_abort(c(
      "Prevalence ages could not be mapped to the reference population:",
      stats::setNames(unmapped_description, rep("x", length(unmapped_description)))
    ))
  }

  prev_clean$age <- mapped_ages

  # ── Step 4: Summarize by mapped groups ──
  prev_grouped <- prev_clean |>
    dplyr::group_by(analysisId, spanLabel, age, gender) |>
    dplyr::summarize(
      numerator = sum(numerator),
      denominator = sum(denominator),
      .groups = "keep"
    ) |>
    dplyr::ungroup()

  invalid_denominators <- prev_grouped |>
    dplyr::filter(!is.finite(denominator) | denominator <= 0)

  if (nrow(invalid_denominators) > 0) {
    invalid_description <- paste0(
      "analysisId=", invalid_denominators$analysisId,
      ", spanLabel=", invalid_denominators$spanLabel,
      ", age=", invalid_denominators$age,
      ", gender=", invalid_denominators$gender
    )
    cli::cli_abort(c(
      "Cannot standardize strata with non-finite or non-positive denominators:",
      stats::setNames(invalid_description, rep("x", length(invalid_description)))
    ))
  }

  prev_grouped <- prev_grouped |>
    dplyr::mutate(stat = (numerator / denominator) * 100000)

  # ── Step 5: Build a consistent reference distribution per analysis ──
  if (nrow(prev_grouped) == 0) {
    cli::cli_abort("No supported age/gender prevalence strata remain after filtering")
  }

  if (anyNA(prev_grouped$age) || anyNA(prev_grouped$gender)) {
    cli::cli_abort(
      "Prevalence contains age/gender strata that could not be mapped to the reference population"
    )
  }

  analysis_strata <- prev_grouped |>
    dplyr::distinct(analysisId, age, gender)
  analysis_spans <- prev_grouped |>
    dplyr::distinct(analysisId, spanLabel)

  expected_span_strata <- purrr::map_dfr(seq_len(nrow(analysis_spans)), function(i) {
    analysis_id <- analysis_spans$analysisId[[i]]
    analysis_strata |>
      dplyr::filter(analysisId == analysis_id) |>
      dplyr::mutate(spanLabel = analysis_spans$spanLabel[[i]]) |>
      dplyr::select(analysisId, spanLabel, age, gender)
  })

  observed_span_strata <- prev_grouped |>
    dplyr::distinct(analysisId, spanLabel, age, gender)
    
  missing_span_strata <- expected_span_strata |>
    dplyr::anti_join(
      observed_span_strata,
      by = c("analysisId", "spanLabel", "age", "gender")
    )

  if (nrow(missing_span_strata) > 0) {
    missing_description <- paste0(
      "analysisId=", missing_span_strata$analysisId,
      ", spanLabel=", missing_span_strata$spanLabel,
      ", age=", missing_span_strata$age,
      ", gender=", missing_span_strata$gender
    )
    cli::cli_abort(c(
      "Cannot standardize because analysis-spans are missing required strata:",
      stats::setNames(missing_description, rep("x", length(missing_description))),
      "i" = "Each span must contain the same strata for its analysis."
    ))
  }

  reference_strata <- ref_base |>
    dplyr::select(age, gender, population)
  unmatched_strata <- analysis_strata |>
    dplyr::anti_join(reference_strata, by = c("age", "gender"))

  if (nrow(unmatched_strata) > 0) {
    unmatched_description <- paste0(
      "analysisId=", unmatched_strata$analysisId,
      ", age=", unmatched_strata$age,
      ", gender=", unmatched_strata$gender
    )
    cli::cli_abort(c(
      "Prevalence strata have no matching reference population:",
      stats::setNames(unmatched_description, rep("x", length(unmatched_description)))
    ))
  }

  ref_adjusted <- analysis_strata |>
    dplyr::inner_join(reference_strata, by = c("age", "gender"))

  invalid_reference_totals <- ref_adjusted |>
    dplyr::group_by(analysisId) |>
    dplyr::summarize(referencePopulation = sum(population), .groups = "drop") |>
    dplyr::filter(!is.finite(referencePopulation) | referencePopulation <= 0)

  if (nrow(invalid_reference_totals) > 0) {
    invalid_analysis_ids <- paste0(
      "analysisId=",
      invalid_reference_totals$analysisId
    )
    cli::cli_abort(c(
      "Matched reference population must have a finite, positive total for each analysis:",
      stats::setNames(invalid_analysis_ids, rep("x", length(invalid_analysis_ids)))
    ))
  }

  ref_adjusted <- ref_adjusted |>
    dplyr::group_by(analysisId) |>
    dplyr::mutate(weight = population / sum(population)) |>
    dplyr::ungroup()

  # ── Step 6: Join & standardize ──
  standardized_data <- prev_grouped |>
    dplyr::inner_join(
      ref_adjusted |> dplyr::select(analysisId, age, gender, weight),
      by = c("analysisId", "age", "gender")
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


#' Parse reference age labels into numeric intervals
#'
#' @noRd
.parse_age_labels <- function(labels) {
  labels <- trimws(as.character(labels))

  if (length(labels) == 0 || anyNA(labels) || any(labels == "")) {
    cli::cli_abort("Reference age labels must be non-empty.")
  }

  is_single <- grepl("^[0-9]+$", labels)
  is_range <- grepl("^[0-9]+[[:space:]]*-[[:space:]]*[0-9]+$", labels)
  is_open <- grepl("^[0-9]+[[:space:]]*\\+$", labels)
  is_under <- grepl("^under[[:space:]]+[0-9]+$", labels, ignore.case = TRUE)
  is_supported <- is_single | is_range | is_open | is_under

  if (any(!is_supported)) {
    invalid_labels <- labels[!is_supported]
    cli::cli_abort(c(
      "Unsupported or malformed reference age label(s):",
      stats::setNames(invalid_labels, rep("x", length(invalid_labels)))
    ))
  }

  min_age <- max_age <- rep(NA_real_, length(labels))
  age_type <- rep(NA_character_, length(labels))

  for (i in seq_along(labels)) {
    label <- labels[[i]]

    if (is_single[[i]]) {
      min_age[[i]] <- as.numeric(label)
      max_age[[i]] <- min_age[[i]]
      age_type[[i]] <- "single"
    } else if (is_range[[i]]) {
      bounds <- as.numeric(strsplit(
        gsub("[[:space:]]", "", label),
        "-",
        fixed = TRUE
      )[[1]])
      min_age[[i]] <- bounds[[1]]
      max_age[[i]] <- bounds[[2]]
      age_type[[i]] <- "range"
    } else if (is_open[[i]]) {
      min_age[[i]] <- as.numeric(gsub("[+]", "", label))
      max_age[[i]] <- Inf
      age_type[[i]] <- "open"
    } else {
      threshold <- as.numeric(sub(
        "^under[[:space:]]+",
        "",
        label,
        ignore.case = TRUE
      ))
      min_age[[i]] <- 0
      max_age[[i]] <- threshold - 1
      age_type[[i]] <- "under"
    }
  }

  invalid_intervals <- min_age < 0 |
    max_age < min_age |
    !is.finite(min_age) |
    (!is.finite(max_age) & age_type != "open")

  if (any(invalid_intervals)) {
    invalid_labels <- labels[invalid_intervals]
    cli::cli_abort(c(
      "Invalid reference age interval(s):",
      stats::setNames(invalid_labels, rep("x", length(invalid_labels)))
    ))
  }

  parsed <- data.frame(
    label = labels,
    min_age = min_age,
    max_age = max_age,
    age_type = age_type,
    stringsAsFactors = FALSE
  )

  ordered <- parsed[order(parsed$min_age, parsed$max_age), , drop = FALSE]

  if (nrow(ordered) > 1) {
    for (i in 2:nrow(ordered)) {
      overlapping <- which(
        ordered$max_age[seq_len(i - 1)] >= ordered$min_age[[i]]
      )

      if (length(overlapping) > 0) {
        previous_label <- ordered$label[[overlapping[[1]]]]
        cli::cli_abort(paste0(
          "Overlapping reference age bands: '",
          previous_label,
          "' and '",
          ordered$label[[i]],
          "'."
        ))
      }
    }
  }

  return(parsed)
}


#' StandardizationReference R6 Class
#'
#' @description
#' Encapsulates a reference population for direct method standardization.
#' Stores population counts by age and gender with metadata. Weights are
#' calculated by the standardization procedure for each analysis.
#'
#' @details
#' The StandardizationReference class provides a structured way to manage
#' reference populations used for age-sex standardization. It validates
#' unique age/gender cells, supported non-overlapping age bands, and finite,
#' non-negative population counts with a finite positive total. It provides
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
    #'   Age labels must be single ages, inclusive ranges, open-ended bands, or
    #'   Under N bands; age/gender pairs must be unique.
    #'
    #' @return A new StandardizationReference object
    initialize = function(name, country, year, source, data, reference = NULL) {
      # Validate inputs
      if (!is.data.frame(data)) {
        cli::cli_abort("'data' must be a data frame")
      }

      required_cols <- c("age", "gender", "population")
      missing_cols <- setdiff(required_cols, colnames(data))
      if (length(missing_cols) > 0) {
        cli::cli_abort(c(
          "Reference data is missing required columns:",
          stats::setNames(missing_cols, rep("x", length(missing_cols))),
          "i" = paste0("Required columns: ", paste(required_cols, collapse = ", "))
        ))
      }

      if (nrow(data) == 0) {
        cli::cli_abort("Reference data must contain at least one population cell.")
      }

      # Check for missing values in required columns
      if (any(is.na(data$age)) || any(is.na(data$gender)) || any(is.na(data$population))) {
        cli::cli_abort("Data cannot contain NA values in age, gender, or population columns")
      }

      if (!is.numeric(data$population)) {
        cli::cli_abort("Population column must be numeric.")
      }

      data$age <- trimws(as.character(data$age))
      data$gender <- trimws(as.character(data$gender))

      if (any(data$age == "") || any(data$gender == "")) {
        cli::cli_abort("Age and gender values must be non-empty.")
      }

      .parse_age_labels(unique(data$age))

      duplicate_keys <- data |>
        dplyr::count(age, gender, name = "cell_count") |>
        dplyr::filter(cell_count > 1)

      if (nrow(duplicate_keys) > 0) {
        duplicate_descriptions <- paste0(
          "age=", duplicate_keys$age,
          ", gender=", duplicate_keys$gender
        )
        cli::cli_abort(c(
          "Reference data contains duplicate age/gender cells:",
          stats::setNames(
            duplicate_descriptions,
            rep("x", length(duplicate_descriptions))
          )
        ))
      }

      invalid_population <- !is.finite(data$population) | data$population < 0

      if (any(invalid_population)) {
        invalid_descriptions <- paste0(
          "age=", data$age[invalid_population],
          ", gender=", data$gender[invalid_population],
          ", population=", data$population[invalid_population]
        )
        cli::cli_abort(c(
          "Population values must be finite and non-negative:",
          stats::setNames(
            invalid_descriptions,
            rep("x", length(invalid_descriptions))
          )
        ))
      }

      total_population <- sum(data$population)

      if (!is.finite(total_population) || total_population <= 0) {
        cli::cli_abort(
          "Reference population must have a finite, positive total."
        )
      }

      # Store metadata in private fields
      private$.name <- name
      private$.country <- country
      private$.year <- as.integer(year)
      private$.source <- source
      private$.reference <- reference

      # Store reference counts; standardization calculates weights per analysis.
      private$.data <- data |>
        dplyr::mutate(
          population = as.numeric(population)
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
    #' @return Data frame with columns: age, gender, and population
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

    #' @description Filter the reference to demographic bounds.
    #' This helper is deprecated and will be removed in a future release; use
    #' `getData()` and apply filtering explicitly.
    #'
    #' @param ageMin Minimum age (inclusive)
    #' @param ageMax Maximum age (inclusive)
    #'
    #' @return Filtered data frame with age, gender, and population columns
    getFilteredReference = function(ageMin = NULL, ageMax = NULL) {

      cli::cli_warn(c(
        "`getFilteredReference()` is deprecated and will be removed in a future release.",
        "i" = "Use `getData()` and apply filtering explicitly."
      ))

      result <- private$.data

      # Convert ages to numeric for comparison
      if (!is.null(ageMin) || !is.null(ageMax)) {
        ages_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", result$age)))
        age_is_in_bounds <- rep(TRUE, length(ages_numeric))

        if (!is.null(ageMin)) {
          age_is_in_bounds <- age_is_in_bounds & ages_numeric >= ageMin
        }

        if (!is.null(ageMax)) {
          age_is_in_bounds <- age_is_in_bounds & ages_numeric <= ageMax
        }

        age_is_in_bounds <- age_is_in_bounds | is.na(ages_numeric)
        result <- dplyr::filter(result, age_is_in_bounds)
      }

      result
    },

    #' @description Apply age truncation and aggregate population counts
    #'
    #' @param rightTruncation Numeric age threshold for truncation
    #'   Must be a finite, non-negative integer supported by this reference.
    #'
    #' @return Data frame with truncated age groups and aggregated population counts
    getAdjustedReference = function(rightTruncation) {
      self$validateRightTruncation(rightTruncation)

      result <- private$.data
      parsed_ages <- .parse_age_labels(unique(result$age))

      # Collapse reference ages at or above the truncation threshold.
      result <- result |>
        dplyr::left_join(
          parsed_ages |> dplyr::select(label, min_age),
          by = c("age" = "label")
        )

      truncate_age <- !is.na(result$min_age) & result$min_age >= rightTruncation
      result$age[truncate_age] <- paste0(rightTruncation, "+")

      result <- result |>
        dplyr::select(-min_age) |>
        dplyr::group_by(age, gender) |>
        dplyr::summarise(
          population = sum(population),
          .groups = "drop"
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
      if (!is.numeric(rightTruncation) ||
          length(rightTruncation) != 1 ||
          !is.finite(rightTruncation) ||
          rightTruncation < 0 ||
          rightTruncation != floor(rightTruncation)) {
        cli::cli_abort(
          "rightTruncation must be a single finite, non-negative integer age."
        )
      }

      parsed_ages <- .parse_age_labels(unique(private$.data$age))
      is_grouped <- any(parsed_ages$age_type != "single")

      if (!is_grouped) {
        valid_range <- range(parsed_ages$min_age)

        if (rightTruncation < valid_range[[1]] ||
            rightTruncation > valid_range[[2]]) {
          cli::cli_abort(c(
            paste0(
              "Age ", rightTruncation,
              " is outside the single-year reference population range."
            ),
            "i" = paste0(
              "Supported ages range from ", valid_range[[1]],
              " to ", valid_range[[2]], "."
            )
          ))
        }

        return(rightTruncation)
      }

      # For grouped references, truncation must be at a group start.
      group_starts <- parsed_ages$min_age

      if (!(rightTruncation %in% group_starts)) {
        # Find which band contains this age and provide a helpful error.
        conflicting <- which(
          rightTruncation >= parsed_ages$min_age &
            rightTruncation <= parsed_ages$max_age
        )

        if (length(conflicting) > 0) {
          conflicting <- parsed_ages[conflicting[[1]], , drop = FALSE]
          cli::cli_abort(c(
            paste0("Cannot truncate at age ", rightTruncation, "."),
            "x" = paste0(
              "It falls within group '", conflicting$label, "' (",
              conflicting$min_age, "-", conflicting$max_age, ")."
            ),
            "i" = paste0(
              "Valid truncation points: ",
              paste(sort(group_starts), collapse = ", ")
            )
          ))
        } else {
          cli::cli_abort(c(
            paste0("Age ", rightTruncation, " is outside the reference population range."),
            "i" = paste0(
              "Valid truncation points: ",
              paste(sort(group_starts), collapse = ", ")
            )
          ))
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
      parsed_ages <- .parse_age_labels(unique(private$.data$age))
      is_grouped <- any(parsed_ages$age_type != "single")

      if (!is_grouped) {
        # Single-year: all unique numeric ages are valid
        return(sort(parsed_ages$min_age))
      }

      # Grouped: only group starts are valid
      sort(unique(parsed_ages$min_age))
    },

    #' @description Map single-year ages to reference age group labels
    #'
    #' Converts ages to the exact labels used by the reference population. Numeric
    #' ages are matched to parsed reference intervals, including single-year labels
    #' with or without zero padding. Plus-suffixed values are accepted only when
    #' they match a native open-ended label or the requested right truncation.
    #'
    #' @param age_values Character/numeric vector. May contain \"N+\" suffix values or numeric strings.
    #' @param rightTruncation Optional numeric threshold used to create a supported \"N+\" label.
    #' @return Character vector of reference-formatted age labels
    mapAgesToReference = function(age_values, rightTruncation = NULL) {
      # Convert to character if not already
      age_values <- as.character(age_values)

      parsed_ages <- .parse_age_labels(unique(private$.data$age))

      # Process each age value
      result <- sapply(age_values, function(age_str) {
        if (is.na(age_str)) {
          return(NA_character_)
        }

        # Preserve only plus-suffixed labels supported by this reference/truncation.
        if (grepl("\\+$", age_str)) {
          matching_open_label <- which(
            parsed_ages$label == age_str & parsed_ages$age_type == "open"
          )

          if (length(matching_open_label) > 0) {
            if (!is.null(rightTruncation) &&
                parsed_ages$min_age[matching_open_label[[1]]] >= rightTruncation) {
              return(paste0(rightTruncation, "+"))
            }

            return(age_str)
          }

          if (!is.null(rightTruncation) &&
              identical(age_str, paste0(rightTruncation, "+"))) {
            return(age_str)
          }

          return(NA_character_)
        }

        # Convert to numeric for mapping
        age_num <- suppressWarnings(as.numeric(age_str))

        if (is.na(age_num)) {
          return(NA_character_)
        }

        # Find the reference interval containing this age and use its actual label.
        idx <- which(
          age_num >= parsed_ages$min_age & age_num <= parsed_ages$max_age
        )
        if (length(idx) == 0) {
          return(NA_character_)
        }

        if (!is.null(rightTruncation) && age_num >= rightTruncation) {
          return(paste0(rightTruncation, "+"))
        }

        return(parsed_ages$label[idx[1]])
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
