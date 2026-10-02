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

  ageRows <- which(results$demographic == "age")

  if (length(ageRows) > 0) {

    ageResults <- results[ageRows, , drop = FALSE]
    ages <- suppressWarnings(as.numeric(ageResults$demographicValue))
    ageSummaries <- lapply(split(seq_len(nrow(ageResults)), ageResults$spanLabel), function(rows) {
      summary <- summarizeWeightedAge(ages[rows], ageResults$caseCount[rows])
      data.frame(
        spanLabel = ageResults$spanLabel[rows[[1]]],
        ageMean = summary[["ageMean"]],
        ageSd = summary[["ageSd"]],
        ageMin = summary[["ageMin"]],
        ageMedian = summary[["ageMedian"]],
        ageMax = summary[["ageMax"]],
        stringsAsFactors = FALSE
      )
    }) |>
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
      dplyr::left_join(ageSummaries, by = "spanLabel")
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
      databaseId = databaseId,
      statType = "Demographics",
      .before = 1
    ) |>
    dplyr::arrange(.data$spanLabel, .data$demographic, .data$demographicValue)

  results
}
