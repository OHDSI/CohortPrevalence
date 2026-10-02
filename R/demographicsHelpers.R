# Summarize age values represented as single-year counts without expanding counts into rows.
summarizeWeightedAge <- function(ages, caseCounts) {
  valid <- !is.na(ages) & !is.na(caseCounts) & caseCounts > 0
  ages <- ages[valid]
  caseCounts <- caseCounts[valid]

  if (length(ages) == 0) {

    return(c(
      ageMean = NA_real_,
      ageSd = NA_real_,
      ageMin = NA_real_,
      ageMedian = NA_real_,
      ageMax = NA_real_
    ))

  }

  order <- order(ages)
  ages <- ages[order]
  caseCounts <- caseCounts[order]
  n <- sum(caseCounts)
  meanAge <- sum(ages * caseCounts) / n
  sdAge <- NA_real_

  if (n > 1) {
    sdAge <- sqrt(sum(caseCounts * (ages - meanAge)^2) / (n - 1))

  }

  cumulativeCounts <- cumsum(caseCounts)
  lowerMedianRank <- floor((n + 1) / 2)
  upperMedianRank <- ceiling((n + 1) / 2)
  lowerMedian <- ages[which(cumulativeCounts >= lowerMedianRank)[[1]]]
  upperMedian <- ages[which(cumulativeCounts >= upperMedianRank)[[1]]]

  c(
    ageMean = meanAge,
    ageSd = sdAge,
    ageMin = min(ages),
    ageMedian = (lowerMedian + upperMedian) / 2,
    ageMax = max(ages)
  )
}

formatDemographicsResults <- function(demographics) {
  resultColumns <- c(
    "analysisId", "cohortId", "cohortName", "statType", "measureType", "spanLabel",
    "demographic", "demographicId", "demographicLabel", "stat", "value"
  )
  demographics <- as.data.frame(demographics)
  checkmate::assert_names(
    names(demographics),
    must.include = c(
      "analysisId", "cohortId", "cohortName", "statType", "spanLabel",
      "demographic", "demographicValue", "demographicLabel", "caseCount",
      "totalCases", "proportion"
    )
  )

  demographics$demographicId <- as.character(demographics$demographicValue)
  missingIds <- is.na(demographics$demographicId)
  demographics$demographicId[missingIds] <- "Missing"

  metadataColumns <- c(
    "analysisId", "cohortId", "cohortName", "statType", "measureType",
    "spanLabel", "demographic"
  )

  makeStatRows <- function(data, statName, statValue, id = data$demographicId) {
    rows <- data[metadataColumns]
    rows$demographicId <- id
    rows$demographicLabel <- data$demographicLabel
    rows$stat <- statName
    rows$value <- as.character(statValue)
    rows[resultColumns]
  }

  statRows <- list()
  for (statName in c("caseCount", "totalCases", "proportion")) {

    if (statName %in% names(demographics)) {

      statRows[[length(statRows) + 1L]] <- makeStatRows(
        demographics,
        statName,
        demographics[[statName]]
      )

    }

  }

  ageRows <- demographics[demographics$demographic == "age", , drop = FALSE]
  ageStatColumns <- c(
    ageMean = "ageMean",
    ageStd = "ageSd",
    ageMin = "ageMin",
    ageMedian = "ageMedian",
    ageMax = "ageMax"
  )

  if (nrow(ageRows) > 0) {

    ageRows <- ageRows[
      !duplicated(ageRows[c(
        "analysisId", "cohortId", "cohortName", "measureType", "spanLabel"
      )]),
      ,
      drop = FALSE
    ]
    for (statName in names(ageStatColumns)) {
      sourceColumn <- ageStatColumns[[statName]]

      if (sourceColumn %in% names(ageRows)) {

        statRows[[length(statRows) + 1L]] <- makeStatRows(
          ageRows,
          statName,
          ageRows[[sourceColumn]],
          id = rep("All ages", nrow(ageRows))
        )
        statRows[[length(statRows)]]$demographicLabel <- rep("All ages", nrow(ageRows))
        statRows[[length(statRows)]] <- statRows[[length(statRows)]][resultColumns]

      }

    }

  }

  if (length(statRows) == 0) {

    return(data.frame(
      analysisId = demographics$analysisId[0],
      cohortId = demographics$cohortId[0],
      cohortName = demographics$cohortName[0],
      statType = demographics$statType[0],
      measureType = demographics$measureType[0],
      spanLabel = demographics$spanLabel[0],
      demographic = demographics$demographic[0],
      demographicId = character(),
      demographicLabel = character(),
      stat = character(),
      value = character(),
      stringsAsFactors = FALSE
    ))

  }

  results <- dplyr::bind_rows(statRows)
  results <- results[resultColumns]
  results <- results[order(
    results$measureType,
    results$spanLabel,
    results$demographic,
    results$demographicId,
    results$stat
  ), , drop = FALSE]
  rownames(results) <- NULL
  results
}

normalizeDemographicsResults <- function(demographics) {
  if (is.null(demographics)) {

    return(NULL)

  }

  checkmate::assert_data_frame(demographics)
  resultColumns <- c(
    "analysisId", "cohortId", "cohortName", "statType", "measureType", "spanLabel",
    "demographic", "demographicId", "demographicLabel", "stat", "value"
  )

  if (all(resultColumns %in% names(demographics))) {

    demographics <- as.data.frame(demographics)[resultColumns]
    demographics$value <- as.character(demographics$value)
    return(demographics)

  }

  demographics <- as.data.frame(demographics)

  if ("value" %in% names(demographics)) {

    demographics$value <- as.character(demographics$value)

  }

  demographics
}

# Clean and annotate the demographics summary returned by SQL.
cleanDemographicsResults <- function(demographicsData,
                                     ageGroups = NULL,
                                     analysisId,
                                     cohortId,
                                     cohortName) {
  checkmate::assert_data_frame(demographicsData)
  results <- as.data.frame(demographicsData)
  checkmate::assert_names(
    names(results),
    must.include = c(
      "measureType", "spanLabel", "demographic", "demographicValue", "caseCount",
      "totalCases", "proportion"
    )
  )
  validateAgeGroups(ageGroups)

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

  ageRows <- which(results$demographic == "age")

  if (length(ageRows) > 0) {

    ageResults <- results[ageRows, , drop = FALSE]
    ages <- suppressWarnings(as.numeric(ageResults$demographicValue))
    ageSummaries <- lapply(
      split(
        seq_len(nrow(ageResults)),
        interaction(ageResults$measureType, ageResults$spanLabel, drop = TRUE, lex.order = TRUE)
      ),
      function(rows) {
        summary <- summarizeWeightedAge(ages[rows], ageResults$caseCount[rows])
        data.frame(
          measureType = ageResults$measureType[rows[[1]]],
          spanLabel = ageResults$spanLabel[rows[[1]]],
          ageMean = summary[["ageMean"]],
          ageSd = summary[["ageSd"]],
          ageMin = summary[["ageMin"]],
          ageMedian = summary[["ageMedian"]],
          ageMax = summary[["ageMax"]],
          stringsAsFactors = FALSE
        )
      }
    ) |>
      dplyr::bind_rows()

    ageLabels <- rep("Other/Unmapped", length(ages))
    ageLabels[is.na(ages)] <- "Missing"

    if (is.null(ageGroups)) {

      ageLabels[!is.na(ages)] <- "All ages"

    } else {

      for (i in seq_along(ageGroups)) {
        ageRange <- ageGroups[[i]]
        inRange <- !is.na(ages) & ages >= ageRange[[1]] & ages <= ageRange[[2]]
        ageLabels[inRange] <- names(ageGroups)[[i]]
      }

    }

    ageResults$demographicValue <- ageLabels
    ageResults$demographicLabel <- ageLabels
    ageResults <- ageResults |>
      dplyr::group_by(
        .data$measureType,
        .data$spanLabel,
        .data$demographic,
        .data$demographicValue,
        .data$demographicLabel
      ) |>
      dplyr::summarise(
        caseCount = sum(.data$caseCount),
        totalCases = max(.data$totalCases),
        .groups = "drop"
      ) |>
      dplyr::left_join(ageSummaries, by = c("measureType", "spanLabel"))
    ageResults$proportion <- 1.0 * ageResults$caseCount / ageResults$totalCases

    nonAgeRows <- results[-ageRows, , drop = FALSE]
    for (summaryColumn in c("ageMean", "ageSd", "ageMin", "ageMedian", "ageMax")) {
      nonAgeRows[[summaryColumn]] <- rep(NA_real_, nrow(nonAgeRows))
    }

    results <- dplyr::bind_rows(nonAgeRows, ageResults)

  } else {

    results$ageMean <- rep(NA_real_, nrow(results))
    results$ageSd <- rep(NA_real_, nrow(results))
    results$ageMin <- rep(NA_real_, nrow(results))
    results$ageMedian <- rep(NA_real_, nrow(results))
    results$ageMax <- rep(NA_real_, nrow(results))

  }

  results <- results |>
    dplyr::mutate(
      analysisId = analysisId,
      cohortId = cohortId,
      cohortName = cohortName,
      statType = "Demographics",
      .before = 1
    ) |>
    dplyr::arrange(
      .data$measureType,
      .data$spanLabel,
      .data$demographic,
      .data$demographicValue
    )

  formatDemographicsResults(results)
}
