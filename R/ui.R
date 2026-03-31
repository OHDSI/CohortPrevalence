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


runIncidence <- function(incidenceAnalysisClass, executionSettings) {

  # make sql
  sql1 <- incidenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- incidenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  # run analysis
  cli::cat_line(
    glue::glue("== Execute Rassen Incidence Analysis =============")
  )
  DatabaseConnector::executeSql(
    connection = executionSettings$getConnection(),
    sql = sql2
  )

  cli::cat_line(
    glue::glue("== Collect Incidence Analysis =============")
  )
  #metaInfo
  meta <- incidenceAnalysisClass$tabulateAnalysisSettings()

  # pull results and prepare for save
  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = executionSettings$getConnection(),
    sql = "SELECT * FROM #incidence;",
    tempEmulationSchema = executionSettings$tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::arrange(dplyr::.data$spanLabel) |>
    dplyr::mutate( # add meta info on prevalent cohort and db
      databaseId = executionSettings$cdmSourceName,
      statType = "Incidence Rate",
      cohortId = incidenceAnalysisClass$targetCohort$id(),
      cohortName = incidenceAnalysisClass$targetCohort$name(),
      .before = 1
    ) |>
    dplyr::inner_join(
      meta, by = c("cohortId", "cohortName")
    )
  return(results)
}

#' Run Single Prevalence Analysis
#'
#' Runs a single prevalence analysis with specified `CohortPrevalenceAnalysis` settings
#'
#' @param prevalenceAnalysisClass A `CohortPrevalenceAnalysis` R6 object with analysis settings.
#' @param executionSettings An `executionSettings` R6 object with connection and schema details.
#'
#' @return A named list of results dataframes: $prevalence, $incidence (if requested), $drugUsage (if requested).
#' @export
#'
generateSinglePrevalence <- function(prevalenceAnalysisClass, executionSettings) {

  # Validate inputs
  checkmate::assert_class(prevalenceAnalysisClass, classes = "CohortPrevalenceAnalysis")
  checkmate::assert_class(executionSettings, classes = "ExecutionSettings")

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

  cli::cat_boxx(
    glue::glue("Analysis {prevalenceAnalysisClass$analysisId}: {prevalenceAnalysisClass$analysisTag}"),
    col = "cyan"
  )
  
  cli::cat_line()
  cli::cat_rule("Analysis Configuration")
  
  tryCatch({
    prevalenceAnalysisClass$viewAnalysisInfo()
  }, error = function(e) {
    cli::cli_alert_warning("Could not display analysis info: {e$message}")
  })

  cli::cat_line()
  
  # Run analysis
  tryCatch({
    resultList <- runPrevalence(
      prevalenceAnalysisClass = prevalenceAnalysisClass,
      executionSettings = executionSettings
    )
  }, error = function(e) {
    cli::cli_alert_danger("Analysis execution failed")
    executionSettings$disconnect()
    stop(e)
  })

  # Disconnect
  cli::cat_line()
  cli::cli_alert_info("Closing database connection...")
  tryCatch({
    executionSettings$disconnect()
    cli::cli_alert_success("Connection closed")
  }, error = function(e) {
    cli::cli_alert_warning("Could not cleanly close connection: {e$message}")
  })

  cli::cat_line()
  cli::cat_rule("Analysis Complete", line = 2)

  return(resultList)
}


#' Run Single Incidence Analysis
#'
#' Runs a single incidence analysis with specified `IncidenceAnalysis` settings
#'
#' @param incidenceAnalysisClass A `IncidenceAnalysis` R6 object with analysis settings.
#' @param executionSettings An `executionSettings` R6 object with connection and schema details.
#'
#' @return A results dataframe with incidence rates and strata.
#' @export
#'
generateSingleRassenIncidence <- function(incidenceAnalysisClass, executionSettings) {


  if (is.null(executionSettings$getConnection())) {
    executionSettings$connect()
  }

  cli::cat_boxx(
    glue::glue("Incidence Analysis id: {incidenceAnalysisClass$analysisId}")
  )
  cli::cat_line(
    glue::glue("== Analysis Description =============")
  )
  incidenceAnalysisClass$viewAnalysisInfo()

  # run analysis
  results <- runIncidence(
    incidenceAnalysisClass = incidenceAnalysisClass,
    executionSettings = executionSettings
  )
  # TODO
  # Add formal formatting step
  # add clean up tables step


  #close out and complete
  cli::cat_line("\n\n")
  executionSettings$disconnect()

  return(results)
}

#' Run Multiple Prevalence Analyses
#'
#' Runs multiple prevalence analysis with a list of specified `CohortPrevalenceAnalysis` settings
#'
#' @param prevalenceAnalysisList A list `CohortPrevalenceAnalysis` R6 object with analysis settings.
#' @param executionSettings An `executionSettings` R6 object with connection and schema details.
#'
#' @return A named list of results dataframes: $prevalence, $incidence (if requested), $drugUsage (if requested).
#'   Each dataframe contains results from all analyses, combined with analysisId column.
#' @export
#'
generateMultiplePrevalence <- function(prevalenceAnalysisList, executionSettings) {

  # Validate inputs
  checkmate::assert_list(prevalenceAnalysisList, min.len = 1)
  checkmate::assert_class(executionSettings, classes = "ExecutionSettings")

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

  cli::cat_boxx(
    glue::glue("Running {length(prevalenceAnalysisList)} Prevalence Analyses")
  )

  # Determine which output types are requested (use first analysis as reference)
  outputTypes <- prevalenceAnalysisList[[1]]$outputTypes
  
  resultList <- list()
  
  # Initialize result storage for each output type
  prevResultsList <- if ("prevalence" %in% outputTypes) list() else NULL
  incResultsList <- if ("incidence" %in% outputTypes) list() else NULL
  drugResultsList <- if ("drugs" %in% outputTypes) list() else NULL
  
  for (i in seq_along(prevalenceAnalysisList)) {
    tryCatch({
      prevalenceAnalysisClass <- prevalenceAnalysisList[[i]]

      cli::cat_line()
      cli::cat_boxx(
        glue::glue("Analysis {i} of {length(prevalenceAnalysisList)}: ID {prevalenceAnalysisClass$analysisId}"),
        col = "cyan"
      )
      
      cli::cat_rule("Configuration", line = 1)
      prevalenceAnalysisClass$viewAnalysisInfo()

      # run analysis and collect results
      cli::cat_line()
      analysisResults <- runPrevalence(
        prevalenceAnalysisClass = prevalenceAnalysisClass,
        executionSettings = executionSettings
      )
      
      # Organize results by type
      if ("prevalence" %in% outputTypes && !is.null(analysisResults$prevalence)) {
        prevResultsList[[i]] <- analysisResults$prevalence |>
          dplyr::mutate(analysisId = prevalenceAnalysisClass$analysisId, .before = 1)
        cli::cli_alert_success("Prevalence results: {nrow(analysisResults$prevalence)} rows")
      }
      
      if ("incidence" %in% outputTypes && !is.null(analysisResults$incidence)) {
        incResultsList[[i]] <- analysisResults$incidence |>
          dplyr::mutate(analysisId = prevalenceAnalysisClass$analysisId, .before = 1)
        cli::cli_alert_success("Incidence results: {nrow(analysisResults$incidence)} rows")
      }
      
      if ("drugs" %in% outputTypes && !is.null(analysisResults$drugUsage)) {
        drugResultsList[[i]] <- analysisResults$drugUsage |>
          dplyr::mutate(analysisId = prevalenceAnalysisClass$analysisId, .before = 1)
        cli::cli_alert_success("Drug results: {nrow(analysisResults$drugUsage)} rows")
      }
    }, error = function(e) {
      cli::cli_alert_danger("Analysis {i} failed: {e$message}")
      stop(e)
    })
  }
  
  cli::cat_line()
  cli::cat_rule("Combining Results")
  
  # Combine results by type
  if ("prevalence" %in% outputTypes) {
    resultList$prevalence <- do.call('rbind', prevResultsList)
    cli::cli_alert_success("Combined prevalence: {nrow(resultList$prevalence)} total rows")
  }
  
  if ("incidence" %in% outputTypes) {
    resultList$incidence <- do.call('rbind', incResultsList)
    cli::cli_alert_success("Combined incidence: {nrow(resultList$incidence)} total rows")
  }
  
  if ("drugs" %in% outputTypes) {
    resultList$drugUsage <- do.call('rbind', drugResultsList)
    cli::cli_alert_success("Combined drugs: {nrow(resultList$drugUsage)} total rows")
  }

  # Disconnect
  cli::cat_line()
  cli::cli_alert_info("Closing database connection...")
  tryCatch({
    executionSettings$disconnect()
    cli::cli_alert_success("Connection closed")
  }, error = function(e) {
    cli::cli_alert_warning("Could not cleanly close connection: {e$message}")
  })

  cli::cat_line()
  cli::cat_rule("All Analyses Complete", line = 2)

  return(resultList)
}

#' Run Multiple Rassen Incidence Analyses
#'
#' Runs multiple incidence analysis with a list of specified `IncidenceAnalysis` settings
#'
#' @param incidenceAnalysisList A list `IncidenceAnalysis` R6 object with analysis settings.
#' @param executionSettings An `executionSettings` R6 object with connection and schema details.
#'
#' @return A results dataframe with incidence rates and strata per analysis id.
#' @export
#'
generateMultipleRassenIncidence <- function(incidenceAnalysisList, executionSettings) {

  if (is.null(executionSettings$getConnection())) {
    executionSettings$connect()
  }

  incResultsList <- vector('list', length = length(incidenceAnalysisList))
  for (i in seq_along(incResultsList)) {
    #pluck analysis Class
    incidenceAnalysisClass <- incidenceAnalysisList[[i]]

    # print description
    analysisId <- incidenceAnalysisClass$analysisId
    cli::cat_boxx(
      glue::glue("Rassen Incidence Analysis id: {analysisId}")
    )
    cli::cat_line(
      glue::glue("== Analysis Description =============")
    )
    incidenceAnalysisClass$viewAnalysisInfo()

    # run analysis
    incResultsList[[i]] <- runIncidence(
      incidenceAnalysisClass = incidenceAnalysisClass,
      executionSettings = executionSettings
    ) |>
      dplyr::mutate(
        analysisId = incidenceAnalysisClass$analysisId,
        .before = 1
      )
  }
  results <- do.call('rbind', incResultsList)
  # Add formal formatting step
  # add clean up tables step

  executionSettings$disconnect()

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

#' Export Prevalence Results
#'
#' Exports results from a prevalence analysis to CSV files in the specified folder.
#'
#' @param resultList A named list containing $prevalence, $incidence, $drugUsage tables (required).
#' @param executionSettings An `ExecutionSettings` R6 object with database details (optional, for metadata).
#' @param outputFolder Character string specifying the path to the folder where the output files will be saved. If left `NULL`, will default to current working directory (optional).
#'
#' @return Invisibly returns the result list that was exported.
#' @export
#'
exportPrevalenceResults <- function(resultList,
                                     executionSettings = NULL,
                                     outputFolder = NULL) {
  
  checkmate::assert_list(resultList)
  
  if(is.null(outputFolder)){
    outputFolder <- here::here()
  }
  
  checkmate::assert_directory_exists(outputFolder)
  
  cli::cat_line()
  cli::cat_rule("Exporting Analysis Results")
  
  outputFiles <- list()
  
  tryCatch({
    # Export prevalence results if present
    if(!is.null(resultList$prevalence) && nrow(resultList$prevalence) > 0) {
      cli::cli_alert_info("Processing prevalence results ({nrow(resultList$prevalence)} rows)...")
      
      # Create filename with timestamp for uniqueness
      analysisId <- if("analysisId" %in% names(resultList$prevalence)) {
        resultList$prevalence$analysisId[1]
      } else {
        format(Sys.time(), "%Y%m%d_%H%M%S")
      }
      
      prevalenceFile <- file.path(
        outputFolder,
        paste0("prevalence_", analysisId, ".csv")
      )
      
      readr::write_csv(resultList$prevalence, file = prevalenceFile)
      outputFiles$prevalence <- prevalenceFile
      cli::cli_alert_success("Exported prevalence to {basename(prevalenceFile)}")
    }
    
    # Export incidence results if present
    if(!is.null(resultList$incidence) && nrow(resultList$incidence) > 0) {
      cli::cli_alert_info("Processing incidence results ({nrow(resultList$incidence)} rows)...")
      
      analysisId <- if("analysisId" %in% names(resultList$incidence)) {
        resultList$incidence$analysisId[1]
      } else {
        format(Sys.time(), "%Y%m%d_%H%M%S")
      }
      
      incidenceFile <- file.path(
        outputFolder,
        paste0("incidence_", analysisId, ".csv")
      )
      
      readr::write_csv(resultList$incidence, file = incidenceFile)
      outputFiles$incidence <- incidenceFile
      cli::cli_alert_success("Exported incidence to {basename(incidenceFile)}")
    }
    
    # Export drug usage results if present
    if(!is.null(resultList$drugUsage) && nrow(resultList$drugUsage) > 0) {
      cli::cli_alert_info("Processing drug usage results ({nrow(resultList$drugUsage)} rows)...")
      
      analysisId <- if("analysisId" %in% names(resultList$drugUsage)) {
        resultList$drugUsage$analysisId[1]
      } else {
        format(Sys.time(), "%Y%m%d_%H%M%S")
      }
      
      drugFile <- file.path(
        outputFolder,
        paste0("drug_usage_", analysisId, ".csv")
      )
      
      readr::write_csv(resultList$drugUsage, file = drugFile)
      outputFiles$drugUsage <- drugFile
      cli::cli_alert_success("Exported drug usage to {basename(drugFile)}")
    }
    
    if(length(outputFiles) > 0) {
      cli::cli_alert_success("All results exported successfully to {outputFolder}")
      cli::cli_alert_info("Exported {length(outputFiles)} result file(s)")
    } else {
      cli::cli_alert_warning("No results to export")
    }
    
    invisible(resultList)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to export results: {e$message}")
    if(length(outputFiles) > 0) {
      cli::cli_alert_warning("Partially exported {length(outputFiles)} file(s) before failure")
    }
    stop(e)
  })
}
