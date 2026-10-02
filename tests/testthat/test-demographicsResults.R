make_demographics_result <- function(value = "Male", analysisId = 1L) {
  demographicValue <- value

  if (value == "Male") {

    demographicValue <- "8507"

  }

  tibble::tibble(
    analysisId = analysisId,
    cohortId = 2L,
    cohortName = "Example cohort",
    databaseId = "Example database",
    statType = "Demographics",
    spanLabel = "2020",
    demographic = "gender",
    demographicValue = demographicValue,
    demographicLabel = value,
    caseCount = 3L,
    totalCases = 5L,
    proportion = 0.6,
    ageMean = NA_real_,
    ageSd = NA_real_,
    ageMin = NA_real_,
    ageMedian = NA_real_,
    ageMax = NA_real_
  )
}

test_that("PrevalenceResults stores, validates, and summarizes demographics", {
  demographics <- make_demographics_result()
  results <- PrevalenceResults$new(demographics = demographics)

  expect_equal(results$demographics, demographics)
  expect_invisible(results$validate())
  summaryText <- capture.output(results$summary())
  expect_true(any(grepl("Demographics: 1 rows", summaryText, fixed = TRUE)))

  invalidResults <- PrevalenceResults$new(
    demographics = tibble::tibble(analysisId = 1L, spanLabel = "2020")
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

  expect_equal(loaded$demographics, demographics)
  expect_true(file.exists(file.path(outputFolder, "with-demographics", "demographics.csv")))

  legacyResults <- PrevalenceResults$new(executionId = "legacy-bundle")
  legacyResults$export(outputFolder, bundleName = "without-demographics")
  legacyLoaded <- loadPrevalenceResults(file.path(outputFolder, "without-demographics"))

  expect_null(legacyLoaded$demographics)
})
