# Clean and annotate the demographics summary returned by SQL.
cleanDemographicsResults <- function(demographicsData,
                                     ageGroups = NULL,
                                     analysisId,
                                     cohortId,
                                     cohortName,
                                     databaseId) {
  checkmate::assert_data_frame(demographicsData)
  checkmate::assert_names(
    names(demographicsData),
    must.include = c(
      "spanLabel", "demographic", "demographicValue", "caseCount",
      "totalCases", "proportion"
    )
  )
  validateAgeGroups(ageGroups)

  results <- as.data.frame(demographicsData)
  results$demographicValue <- as.character(results$demographicValue)
  results$demographicLabel <- results$demographicValue

  conceptLabels <- list(
    gender = c("8507" = "Male", "8532" = "Female", "0" = "No matching concept"),
    race = c(
      "8515" = "Asian",
      "8516" = "Black or African American",
      "8527" = "White",
      "8557" = "American Indian or Alaska Native",
      "8522" = "Native Hawaiian or Other Pacific Islander",
      "8657" = "Other Race",
      "0" = "No matching concept"
    ),
    ethnicity = c(
      "38003563" = "Hispanic or Latino",
      "38003564" = "Not Hispanic or Latino",
      "0" = "No matching concept"
    )
  )

  for (dimension in names(conceptLabels)) {
    dimensionRows <- which(results$demographic == dimension)

    if (length(dimensionRows) > 0) {
      rawValues <- results$demographicValue[dimensionRows]
      mappedLabels <- unname(conceptLabels[[dimension]][rawValues])
      hasLabel <- !is.na(mappedLabels)
      labels <- rawValues
      labels[hasLabel] <- mappedLabels[hasLabel]
      results$demographicLabel[dimensionRows] <- labels
    }
  }

  missingValues <- is.na(results$demographicValue)
  results$demographicLabel[missingValues] <- "Missing"

  if (!is.null(ageGroups)) {
    ageRows <- which(results$demographic == "age")

    if (length(ageRows) > 0) {
      ages <- suppressWarnings(as.numeric(results$demographicValue[ageRows]))
      ageLabels <- rep("Other/Unmapped", length(ages))
      ageLabels[is.na(ages)] <- "Missing"

      for (i in seq_along(ageGroups)) {
        ageRange <- ageGroups[[i]]
        inRange <- !is.na(ages) & ages >= ageRange[[1]] & ages <= ageRange[[2]]
        ageLabels[inRange] <- names(ageGroups)[[i]]
      }

      ageResults <- results[ageRows, , drop = FALSE]
      ageResults$demographicValue <- ageLabels
      ageResults$demographicLabel <- ageLabels
      ageResults <- ageResults |>
        dplyr::group_by(
          .data$spanLabel,
          .data$demographic,
          .data$demographicValue,
          .data$demographicLabel
        ) |>
        dplyr::summarise(
          caseCount = sum(.data$caseCount),
          totalCases = max(.data$totalCases),
          .groups = "drop"
        )
      ageResults$proportion <- 1.0 * ageResults$caseCount / ageResults$totalCases

      nonAgeRows <- results[-ageRows, , drop = FALSE]
      results <- dplyr::bind_rows(nonAgeRows, ageResults)
    }
  }

  results <- results |>
    dplyr::mutate(
      analysisId = analysisId,
      cohortId = cohortId,
      cohortName = cohortName,
      databaseId = databaseId,
      statType = "Demographics",
      .before = 1
    ) |>
    dplyr::arrange(.data$spanLabel, .data$demographic, .data$demographicValue)

  results
}
