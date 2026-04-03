runPrevalence <- function(prevalenceAnalysisClass, executionSettings) {

  cli::cat_line()
  cli::cat_bullet("Assembling SQL...", bullet = "info")

  tryCatch({
    sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
    cli::cli_alert_success("SQL assembled successfully")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to assemble SQL: {e$message}")
    stop(e)
  })

  tryCatch({
    sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)
    cli::cli_alert_success("SQL rendered and translated successfully")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to render SQL: {e$message}")
    stop(e)
  })

  cli::cat_line()
  cli::cat_rule(glue::glue("Executing Analysis {prevalenceAnalysisClass$analysisId}"))

  tryCatch({
    DatabaseConnector::executeSql(
      connection = executionSettings$getConnection(),
      sql = sql2
    )
    cli::cli_alert_success("Analysis executed successfully")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to execute SQL: {e$message}")
    stop(e)
  })

  cli::cat_line()
  cli::cat_bullet("Collecting results...", bullet = "info")

  tryCatch({
    results <- prevalenceAnalysisClass$collectResults(
      connection = executionSettings$getConnection(),
      executionSettings = executionSettings
    )
    cli::cli_alert_success("Results collected successfully")

    # Summary of collected tables
    outputTypes <- paste(names(results), collapse = ", ")
    cli::cli_alert_info("Result tables collected: {outputTypes}")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to collect results: {e$message}")
    stop(e)
  })

  return(results)
}


#' Generate Prevalence Analysis Results
#'
#' Unified function for running one or more prevalence analyses with comprehensive result management.
#' Replaces separate generateSinglePrevalence and generateMultiplePrevalence functions.
#'
#' @param prevalenceAnalysisList A `CohortPrevalenceAnalysis` R6 object or list of such objects
#' @param executionSettings An `executionSettings` R6 object with connection and schema details
#' @param captureSql Logical. If TRUE (default), capture rendered SQL queries for audit trail
#'
#' @return A `PrevalenceResults` R6 object containing all results with full provenance tracking
#'
#' @details
#' This function consolidates analysis execution and result collection into a single workflow.
#' Results include prevalence, incidence, and drug usage data as configured in the analysis objects.
#' SQL queries are captured with SHA256 checksums for reproducibility verification.
#'
#' ## Result Tracking
#' - prevalence data frame: Main prevalence estimates
#' - incidence data frame: Incidence rates if requested
#' - drugUsage data frame: Drug usage patterns if requested
#' - metaInfo data frame: Analysis metadata and configuration
#'
#' ## Query Audit Trail (Level 1)
#' Each executed query is captured and stored with SHA256 checksum in the PrevalenceResults
#' object. Access via `results$show_query(analysisId)` for inspection.
#'
#' @export
#'
generatePrevalence <- function(prevalenceAnalysisList,
                               executionSettings,
                               captureSql = TRUE) {

  # Normalize input - handle single analysis or list
  if (!is.list(prevalenceAnalysisList)) {
    prevalenceAnalysisList <- list(prevalenceAnalysisList)
  }

  # Validate inputs
  checkmate::assert_list(prevalenceAnalysisList, min.len = 1)
  checkmate::assert_class(executionSettings, classes = "ExecutionSettings")
  checkmate::assert_logical(captureSql, len = 1)

  # Establish connection if needed
  tryCatch({
    if (is.null(executionSettings$getConnection())) {
      cli::cli_alert_info("Establishing database connection...")
      executionSettings$connect()
      cli::cli_alert_success("Database connection established")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to database: {e$message}")
    stop(e)
  })

  on.exit({
    tryCatch({
      cli::cli_alert_info("Closing database connection...")
      executionSettings$disconnect()
      cli::cli_alert_success("Connection closed")
    }, error = function(e) {
      cli::cli_alert_warning("Could not cleanly close connection: {e$message}")
    })
  }, add = TRUE)

  cli::cat_line()
  cli::cat_boxx(
    glue::glue("Running {length(prevalenceAnalysisList)} Prevalence Analysis{if(length(prevalenceAnalysisList) > 1) 'es' else ''}"),
    col = "cyan"
  )

  # Determine which output types are requested (use first analysis as reference)
  outputTypes <- prevalenceAnalysisList[[1]]$outputTypes

  # Initialize result storage
  prevResultsList <- if ("prevalence" %in% outputTypes) list() else NULL
  incResultsList <- if ("incidence" %in% outputTypes) list() else NULL
  drugResultsList <- if ("drugs" %in% outputTypes) list() else NULL
  metaInfoList <- list()
  executedQueries <- list()
  executionErrors <- list()

  # Run each analysis with tolerant error handling
  for (i in seq_along(prevalenceAnalysisList)) {
    prevalenceAnalysisClass <- prevalenceAnalysisList[[i]]
    analysisId <- as.character(prevalenceAnalysisClass$analysisId)

    tryCatch({
      cli::cat_line()
      cli::cat_boxx(
        glue::glue("Analysis {i}/{length(prevalenceAnalysisList)}: {analysisId} - {prevalenceAnalysisClass$analysisTag}"),
        col = "cyan"
      )

      cli::cat_rule("Configuration", line = 1)

      tryCatch({
        prevalenceAnalysisClass$viewAnalysisInfo()
      }, error = function(e) {
        cli::cli_alert_warning("Could not display analysis info: {e$message}")
      })

      # Assemble and render SQL
      cli::cat_line()
      cli::cat_bullet("Assembling and rendering SQL...", bullet = "info")

      sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
      sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

      # Capture SQL if requested
      if (captureSql && !is.null(sql2)) {
        executedQueries[[analysisId]] <- sql2
      }

      cli::cli_alert_success("SQL rendered successfully")

      # Execute analysis
      cli::cat_line()
      cli::cat_rule(glue::glue("Executing Analysis {analysisId}"))

      DatabaseConnector::executeSql(
        connection = executionSettings$getConnection(),
        sql = sql2
      )

      cli::cli_alert_success("Analysis executed successfully")

      # Collect results
      cli::cat_line()
      cli::cat_bullet("Collecting results...", bullet = "info")

      analysisResults <- prevalenceAnalysisClass$collectResults(
        connection = executionSettings$getConnection(),
        executionSettings = executionSettings
      )

      # Organize results by type
      if ("prevalence" %in% outputTypes && !is.null(analysisResults$prevalence)) {
        prevResultsList[[i]] <- analysisResults$prevalence
        cli::cli_alert_success("Prevalence: {nrow(analysisResults$prevalence)} rows")
      }

      if ("incidence" %in% outputTypes && !is.null(analysisResults$incidence)) {
        incResultsList[[i]] <- analysisResults$incidence
        cli::cli_alert_success("Incidence: {nrow(analysisResults$incidence)} rows")
      }

      if ("drugs" %in% outputTypes && !is.null(analysisResults$drugUsage)) {
        drugResultsList[[i]] <- analysisResults$drugUsage
        cli::cli_alert_success("Drug usage: {nrow(analysisResults$drugUsage)} rows")
      }

      # Capture metaInfo
      if (!is.null(analysisResults$metaInfo)) {
        metaInfoList[[i]] <- analysisResults$metaInfo
      }

    }, error = function(e) {
      executionErrors[[analysisId]] <- e$message
      cli::cli_alert_danger("Analysis {analysisId} failed: {e$message}")
    })
  }

  # Combine results
  cli::cat_line()
  cli::cat_rule("Combining Results")

  combinedResults <- list()

  if ("prevalence" %in% outputTypes && !is.null(prevResultsList) && length(prevResultsList) > 0) {
    combinedResults$prevalence <- do.call('rbind', prevResultsList)
    cli::cli_alert_success("Combined prevalence: {nrow(combinedResults$prevalence)} total rows")
  }

  if ("incidence" %in% outputTypes && !is.null(incResultsList) && length(incResultsList) > 0) {
    combinedResults$incidence <- do.call('rbind', incResultsList)
    cli::cli_alert_success("Combined incidence: {nrow(combinedResults$incidence)} total rows")
  }

  if ("drugs" %in% outputTypes && !is.null(drugResultsList) && length(drugResultsList) > 0) {
    combinedResults$drugUsage <- do.call('rbind', drugResultsList)
    cli::cli_alert_success("Combined drug usage: {nrow(combinedResults$drugUsage)} total rows")
  }

  if (length(metaInfoList) > 0) {
    metaInfoData <- do.call('rbind', metaInfoList) |>
      dplyr::distinct()
    combinedResults$metaInfo <- metaInfoData
    cli::cli_alert_success("Combined metaInfo: {nrow(metaInfoData)} unique analyses")
  }

  # Create PrevalenceResults object
  cli::cat_line()
  cli::cat_rule("Creating Result Object")

  results <- PrevalenceResults$new(
    prevalence = combinedResults$prevalence,
    incidence = combinedResults$incidence,
    drugUsage = combinedResults$drugUsage,
    metaInfo = combinedResults$metaInfo
  )

  # Add executed queries to result object
  if (captureSql && length(executedQueries) > 0) {
    for (analysisId in names(executedQueries)) {
      results$.addExecutedQuery(analysisId, executedQueries[[analysisId]])
    }
    cli::cli_alert_success("Captured {length(executedQueries)} SQL queries with checksums")
  }

  # Report any execution errors
  if (length(executionErrors) > 0) {
    cli::cat_line()
    cli::cli_alert_warning("Execution completed with {length(executionErrors)} error(s):")
    for (analysisId in names(executionErrors)) {
      cli::cli_alert_warning("  - Analysis {analysisId}: {executionErrors[[analysisId]]}")
    }
  }

  cli::cat_line()
  cli::cat_rule("Analysis Complete", line = 2)

  return(results)
}


#' Export Prevalence Query
#'
#' Exports the full SQL query of a `CohortPrevalenceAnalysis` analysis.
#'
#' @param prevalenceAnalysisClass A `CohortPrevalenceAnalysis` R6 object with analysis settings (required).
#' @param executionSettings An `ExecutionSettings` R6 object with database details (required).
#' @param outputFolder Character string specifying the path to the folder where the output files will be saved. If left `NULL`, will default to current working directory (optional).
#'
#' @return Invisibly returns the rendered SQL string.
#' @export
#'
exportPrevalenceQuery <- function(prevalenceAnalysisClass,
                                  executionSettings,
                                  outputFolder = NULL) {

  checkmate::assert_class(prevalenceAnalysisClass, classes = "CohortPrevalenceAnalysis")
  checkmate::assert_class(executionSettings, classes = "ExecutionSettings")

  if(is.null(outputFolder)){
    outputFolder <- here::here()
  }

  checkmate::assert_directory_exists(outputFolder)

  cli::cat_line()
  cli::cat_rule("Exporting SQL Query")

  tryCatch({
    cli::cli_alert_info("Assembling SQL...")
    sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)

    cli::cli_alert_info("Rendering SQL...")
    sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

    outputFile <- file.path(
      outputFolder,
      paste0("a_", prevalenceAnalysisClass$analysisId, "_id_", prevalenceAnalysisClass$prevalentCohort$id(), ".sql")
    )

    cli::cli_alert_info("Writing to file: {outputFile}")
    readr::write_file(x = sql2, file = outputFile)

    cli::cli_alert_success("SQL query exported successfully")
    cli::cli_alert_info("File size: {format(file.size(outputFile), units = 'auto')}")

    invisible(sql2)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export SQL: {e$message}")
    stop(e)
  })
}
