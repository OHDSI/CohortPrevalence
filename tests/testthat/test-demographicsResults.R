make_demographics_result <- function(value = "Male", analysisId = 1L) {
  demographicValue <- value

  if (value == "Male") {

    demographicValue <- "8507"

  }

  tibble::tibble(
    analysisId = analysisId,
    cohortId = 2L,
    cohortName = "Example cohort",
    statType = "Demographics",
    measureType = "prevalence",
    spanLabel = "2020",
    demographic = "gender",
    demographicId = demographicValue,
    demographicLabel = value,
    stat = "caseCount",
    value = "3"
  )
}

test_that("PrevalenceResults stores, validates, and summarizes demographics", {
  demographics <- make_demographics_result()
  results <- PrevalenceResults$new(demographics = demographics)

  expect_equal(as.data.frame(results$demographics), as.data.frame(demographics))
  expect_invisible(results$validate())
  summaryText <- capture.output(results$summary())
  expect_true(any(grepl("Demographics: 1 rows", summaryText, fixed = TRUE)))

  invalidResults <- PrevalenceResults$new(
    demographics = tibble::tibble(
      analysisId = 1L,
      stat = "caseCount",
      value = "1"
    )
  )
  expect_error(invalidResults$validate(), "Demographics is missing required columns")
})

test_that("demographics results combine across successful analyses", {
  first <- make_demographics_result("Male", analysisId = 1L)
  second <- make_demographics_result("Unknown", analysisId = 2L)

  combined <- bindDemographicsResults(list(first, NULL, second))

  expect_equal(nrow(combined), 2L)
  expect_equal(combined$analysisId, c(1L, 2L))
  expect_equal(bindDemographicsResults(list(NULL, NULL)), NULL)
})

test_that("generatePrevalence output selections must match across analyses", {
  commonArgs <- list(
    prevalentCohort = createTargetCohort(1, "Example cohort"),
    periodOfInterest = createYearlyRange(2020),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365),
    strata = "age"
  )
  prevalenceOnly <- do.call(
    createCohortPrevalenceAnalysis,
    c(list(analysisId = 1L), commonArgs)
  )
  withDemographics <- do.call(
    createCohortPrevalenceAnalysis,
    c(
      list(analysisId = 2L),
      commonArgs,
      list(outputTypes = c("prevalence", "demographics"))
    )
  )

  expect_equal(
    validateCommonPrevalenceOutputTypes(list(withDemographics, withDemographics)),
    c("prevalence", "demographics")
  )
  expect_error(
    validateCommonPrevalenceOutputTypes(list(prevalenceOnly, withDemographics)),
    "must request the same outputTypes"
  )
})

test_that("demographics result bundles round-trip and old bundles remain compatible", {
  outputFolder <- tempfile("demographics-bundles-")
  dir.create(outputFolder)
  on.exit(unlink(outputFolder, recursive = TRUE), add = TRUE)

  demographics <- make_demographics_result()
  results <- PrevalenceResults$new(
    demographics = demographics,
    executionId = "demographics-round-trip"
  )
  results$export(outputFolder, bundleName = "with-demographics")
  loaded <- loadPrevalenceResults(file.path(outputFolder, "with-demographics"))

  expect_equal(as.data.frame(loaded$demographics), as.data.frame(demographics))
  expect_true(file.exists(file.path(outputFolder, "with-demographics", "demographics.csv")))

  legacyResults <- PrevalenceResults$new(executionId = "legacy-bundle")
  legacyResults$export(outputFolder, bundleName = "without-demographics")
  legacyLoaded <- loadPrevalenceResults(file.path(outputFolder, "without-demographics"))

  expect_null(legacyLoaded$demographics)
})

test_that("PrevalenceResults requires measureType in the tidy demographics schema", {
  wideDemographics <- tibble::tibble(
    analysisId = 1L,
    cohortId = 2L,
    cohortName = "Example cohort",
    databaseId = "Example database",
    statType = "Demographics",
    spanLabel = "2020",
    demographic = "age",
    demographicValue = "18",
    demographicLabel = "All ages",
    caseCount = 3L,
    totalCases = 5L,
    proportion = 0.6,
    ageMean = 18,
    ageSd = NA_real_,
    ageMin = 18,
    ageMedian = 18,
    ageMax = 18
  )

  priorTidyDemographics <- tibble::tibble(
    analysisId = c(1L, 1L),
    cohortId = c(2L, 2L),
    cohortName = c("Example cohort", "Example cohort"),
    statType = c("Demographics", "Demographics"),
    spanLabel = c("2020", "2020"),
    demographic = c("gender", "gender"),
    demographicId = c("8507", "8507"),
    demographicLabel = c("Male", "Male"),
    stat = c("caseCount", "demographicLabel"),
    value = c("3", "Male")
  )

  wideResults <- PrevalenceResults$new(demographics = wideDemographics)
  untaggedResults <- PrevalenceResults$new(demographics = priorTidyDemographics)

  expect_error(wideResults$validate(), "Demographics is missing required columns")
  expect_error(untaggedResults$validate(), "measureType")
})
