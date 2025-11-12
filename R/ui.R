generateSinglePrevalence <- function(prevalenceAnalysisClass, executionSettings) {
  sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql2
  )

  # retrieve step
  # format
}

showPrevalenceQuery <- function() {
  sql1 <- prevalenceAnalysisClass$assembleSql(executionSettings)
  sql2 <- prevalenceAnalysisClass$renderAssembledSql(sql = sql1, executionSettings)

  readr::write_file(
    x = sql2,
    file = here::here("name_of_file.sql")
  )

  cli::cat_bullet("Saved file to path")


  invisible(sql2)
}
