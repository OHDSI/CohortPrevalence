test_that("CohortPrevalenceExperiment carries observation settings into spec and analyses", {
  exp <- CohortPrevalenceExperiment$new("observation settings")

  exp$addCohorts(tibble::tibble(
    cohortId = 1,
    cohortName = "Test cohort"
  ))

  exp$addPrevalenceTypes(list(
    createPrevalenceType("point_prevalence", lookBackDays = 365L)
  ))

  exp$addDemographicConstraints(list(
    createDemographicConstraints(ageMin = 18L, ageMax = 120L, genderIds = c(8507L, 8532L))
  ))

  exp$addPeriodsOfInterest(list(
    createYearlyRange(2020:2021)
  ))

  exp$setCommonParameters(
    strata = c("age", "gender"),
    outputTypes = "prevalence",
    minimumObservationLength = 365L,
    useOnlyFirstObservationPeriod = TRUE
  )

  spec <- exp$getSpecification()
  expect_equal(spec$minimumObservationLength, 365L)
  expect_true(spec$useOnlyFirstObservationPeriod)

  analyses <- exp$define()
  expect_length(analyses, 1)
  expect_equal(analyses[[1]]$minimumObservationLength, 365L)
  expect_true(analyses[[1]]$useOnlyFirstObservationPeriod)
})


test_that("CohortPrevalenceExperiment reconstructs span periods in define", {
  exp <- CohortPrevalenceExperiment$new("span reconstruction")

  exp$addCohorts(tibble::tibble(
    cohortId = 1,
    cohortName = "Test cohort"
  ))

  exp$addPrevalenceTypes(list(
    createPrevalenceType("period_prevalence_pd2", lookBackDays = 365L)
  ))

  exp$addDemographicConstraints(list(
    createDemographicConstraints(ageMin = 0L, ageMax = 150L, genderIds = c(8507L, 8532L))
  ))

  exp$addPeriodsOfInterest(list(
    createSpan(
      startDates = as.Date("2020-01-01"),
      endDates = as.Date("2020-12-31")
    )
  ))

  exp$setCommonParameters(outputTypes = "prevalence")

  spec <- exp$getSpecification()
  expect_equal(spec$poiType, "span")
  expect_equal(spec$poiStart, as.Date("2020-01-01"))
  expect_equal(spec$poiEnd, as.Date("2020-12-31"))

  analyses <- exp$define()
  poi <- analyses[[1]]$periodOfInterest$poiRange
  expect_equal(poi$calendar_start_date, as.Date("2020-01-01"))
  expect_equal(poi$calendar_end_date, as.Date("2020-12-31"))
})
