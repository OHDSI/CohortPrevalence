#' Run Prevalence Analysis
#'
#' Runs a prevalence analysis with specified `CohortPrevalenceAnalysis` settings
#'
#' @param prevalenceAnalysisClass A `CohortPrevalenceAnalysis` R6 object with analysis settings.
#' @param executionSettings An `executionSettings` R6 object with connection and schema details.
#'
#' @return A results dataframe with prevalence rates and strata.
#' @export
#'
generateSinglePrevalence <- function(prevalenceAnalysisClass, executionSettings) {


  sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  if (is.null(executionSettings$getConnection())) {
    executionSettings$connect()
  }
  cli::cat_rule(glue::glue_col("{green Analysis Description}"))
  prevalenceAnalysisClass$viewAnalysisInfo()

  # run analysis
  cli::cat_rule(glue::glue_col("{green Execute Prevalence Analysis}"))
  DatabaseConnector::executeSql(
    connection = executionSettings$getConnection(),
    sql = sql2
  )

  cli::cat_rule(glue::glue_col("{green Collect Prevalence Analysis}"))
  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = executionSettings$getConnection(),
    sql = "SELECT * FROM #prevalence;",
    tempEmulationSchema = executionSettings$tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::arrange(calendarYear, age, genderConceptId) #TODO: strata params

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
#' @param outputFolder Character string specifying the path to the folder where the output files will be saved. If left `NULL`, will default to current working directory (optional).
#'
#' @export
#'
exportPrevalenceQuery <- function(prevalenceAnalysisClass,
                                  outputFolder = NULL) {
  if(is.null(outputFolder)){
    outputFolder <- here::here()
  }
  sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  outputFile <- file.path(
    outputFolder,
    paste0("a_", prevalenceAnalysisClass$analysisId, "_id_", prevalenceAnalysisClass$prevalentCohort$id(), ".sql")
  )

  readr::write_file(
    x = sql2,
    file = outputFile
    )

  cli::cat_bullet("Saved SQL query to ", outputFile)

  invisible(sql2)
}

#' Export Prevalence Query
#'
#' Saves the results of a `CohortPrevalenceAnalysis` analysis as a .csv.
#'
#' @param results Dataframe: Result of a `generateSinglePrevalence` analysis.
#' @param outputFolder Character string specifying the path to the folder where the output files will be saved. If left `NULL`, will default to current working directory (optional).
#'
#' @export
#'
exportPrevalenceResults <- function(results, outputFolder = NULL){
  if(is.null(outputFolder)){
    outputFolder <- here::here()
  }
  outputFile <- file.path(
    outputFolder,
    paste0("a_", prevalenceAnalysisClass$analysisId, "_id_", prevalenceAnalysisClass$prevalentCohort$id(), ".rds")
    )

  write.csv(
    x = results,
    file = outputFile,
    quote = FALSE,
    col.names = FALSE
  )
  cli::cat_bullet("Saved prevalence results to ", outputFile)

  invisible(results)
}
