runPrevalence <- function(prevalenceAnalysisClass, executionSettings) {

  # make sql
  sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  # run analysis
  cli::cat_line(
    glue::glue_col("{yellow == Execute Prevalence Analysis =============}")
  )
  DatabaseConnector::executeSql(
    connection = executionSettings$getConnection(),
    sql = sql2
  )

  cli::cat_line(
    glue::glue_col("{yellow == Collect Prevalence Analysis =============}")
  )
  #metaInfo
  meta <- prevalenceAnalysisClass$tabulateAnalysisSettings()

  # pull results and prepare for save
  results <- DatabaseConnector::renderTranslateQuerySql(
    connection = executionSettings$getConnection(),
    sql = "SELECT * FROM #prevalence;",
    tempEmulationSchema = executionSettings$tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::arrange(spanLabel) |>
    dplyr::mutate( # add meta info on prevalent cohort and db
      databaseId = executionSettings$cdmSourceName,
      statType = "Prevalence",
      cohortId = prevalenceAnalysisClass$prevalentCohort$id(),
      cohortName = prevalenceAnalysisClass$prevalentCohort$name(),
      .before = 1
    ) |>
    dplyr::inner_join(
      meta, by = c("cohortId", "cohortName")
    ) |>
    dplyr::select(
      databaseId, analysisId, statType, cohortId, cohortName, spanLabel,
      numerator, denominator, prevalenceRate,
      poi, lookBackDays, num, denom, obsPeriod, demoConAge, demoConGender
    )

  return(results)
}


runIncidence <- function(incidenceAnalysisClass, executionSettings) {

  # make sql
  sql1 <- incidenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- incidenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  # run analysis
  cli::cat_line(
    glue::glue_col("{yellow == Execute Rassen Incidence Analysis =============}")
  )
  DatabaseConnector::executeSql(
    connection = executionSettings$getConnection(),
    sql = sql2
  )

  cli::cat_line(
    glue::glue_col("{yellow == Collect Incidence Analysis =============}")
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
    dplyr::arrange(spanLabel) |>
    dplyr::mutate( # add meta info on prevalent cohort and db
      databaseId = executionSettings$cdmSourceName,
      statType = "Incidence Rate",
      cohortId = incidenceAnalysisClass$targetCohort$id(),
      cohortName = incidenceAnalysisClass$targetCohort$name(),
      .before = 1
    ) |>
    dplyr::inner_join(
      meta, by = c("cohortId", "cohortName")
    ) |>
    dplyr::select(
      databaseId, analysisId, statType, cohortId, cohortName, spanLabel,
      numerator, denominator, incidenceRate,
      poi, lookBackDays, num, denom, obsPeriod, demoConAge, demoConGender
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
#' @return A results dataframe with prevalence rates and strata.
#' @export
#'
generateSinglePrevalence <- function(prevalenceAnalysisClass, executionSettings) {


  if (is.null(executionSettings$getConnection())) {
    executionSettings$connect()
  }
  analysisId <- prevalenceAnalysisClass$analysisId
  cli::cat_boxx(
    glue::glue_col("{yellow Prevalence Analysis id: {analysisId}}")
  )
  cli::cat_line(
    glue::glue_col("{yellow == Analysis Description =============}")
  )
  prevalenceAnalysisClass$viewAnalysisInfo()

  # run analysis
  results <- runPrevalence(
    prevalenceAnalysisClass = prevalenceAnalysisClass,
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
  analysisId <- incidenceAnalysisClass$analysisId
  cli::cat_boxx(
    glue::glue_col("{yellow Incidence Analysis id: {analysisId}}")
  )
  cli::cat_line(
    glue::glue_col("{yellow == Analysis Description =============}")
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
#' @return A results dataframe with prevalence rates and strata per analysis id.
#' @export
#'
generateMultiplePrevalence <- function(prevalenceAnalysisList, executionSettings) {

  if (is.null(executionSettings$getConnection())) {
    executionSettings$connect()
  }

  prevResultsList <- vector('list', length = length(prevalenceAnalysisList))
  for (i in seq_along(prevResultsList)) {
    #pluck analysis Class
    prevalenceAnalysisClass <- prevalenceAnalysisList[[i]]

    # print description
    analysisId <- prevalenceAnalysisClass$analysisId
    cli::cat_boxx(
      glue::glue_col("{yellow Prevalence Analysis id: {analysisId}}")
    )
    cli::cat_line(
      glue::glue_col("{yellow == Analysis Description =============}")
    )
    prevalenceAnalysisClass$viewAnalysisInfo()

    # run analysis
    prevResultsList[[i]] <- runPrevalence(
      prevalenceAnalysisClass = prevalenceAnalysisClass,
      executionSettings = executionSettings
    ) |>
      dplyr::mutate(
        analysisId = prevalenceAnalysisClass$analysisId,
        .before = 1
      )
  }
  results <- do.call('rbind', prevResultsList)
  # Add formal formatting step
  # add clean up tables step

  executionSettings$disconnect()

  return(results)
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
      glue::glue_col("{yellow Rassen Incidence Analysis id: {analysisId}}")
    )
    cli::cat_line(
      glue::glue_col("{yellow == Analysis Description =============}")
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
#' @param outputFolder Character string specifying the path to the folder where the output files will be saved. If left `NULL`, will default to current working directory (optional).
#'
#' @export
#'
exportPrevalenceQuery <- function(prevalenceAnalysisClass,
                                  executionSettings,
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
    paste0("a_", prevalenceAnalysisClass$analysisId, "_id_", prevalenceAnalysisClass$prevalentCohort$id(), ".csv")
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
