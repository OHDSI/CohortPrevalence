test_that("CohortPrevalenceExperiment carries observation settings into spec and analyses", {
  exp <- CohortPrevalenceExperiment$new("observation settings")

  exp$addCohorts(tibble::tibble(
    cohortId = 1,
    cohortName = "Test cohort"
  ))

  exp$addPrevalenceTypes(list(
    createPrevalenceType("point_prevalence", lookBackDays = 365L, leadInDays = 365L)
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
    useOnlyFirstObservationPeriod = TRUE
  )

  spec <- exp$getSpecification()
  expect_equal(spec$leadInDays, 365L)
  expect_true(spec$useOnlyFirstObservationPeriod)

  analyses <- exp$define()
  expect_length(analyses, 1)
  expect_equal(analyses[[1]]$prevalenceType$leadInDays, 365L)
  expect_true(analyses[[1]]$useOnlyFirstObservationPeriod)
})

test_that("CohortPrevalenceExperiment propagates demographics and ageGroups", {
  exp <- CohortPrevalenceExperiment$new("demographics settings")
  exp$addCohorts(tibble::tibble(cohortId = 1, cohortName = "Test cohort"))
  exp$addPrevalenceTypes(list(
    createPrevalenceType("point_prevalence", lookBackDays = 365L)
  ))
  exp$addDemographicConstraints(list(
    createDemographicConstraints(ageMin = 18L, ageMax = 120L)
  ))
  exp$addPeriodsOfInterest(list(createYearlyRange(2020:2021)))
  exp$setCommonParameters(
    strata = c("age", "gender"),
    outputTypes = c("prevalence", "demographics"),
    ageGroups = list("18-22" = c(18, 22), "23+" = c(23, Inf))
  )

  spec <- exp$getSpecification()
  expect_equal(spec$strata[[1]], c("age", "gender"))
  expect_equal(spec$outputTypes[[1]], c("prevalence", "demographics"))
  expect_equal(spec$ageGroups[[1]], list("18-22" = c(18, 22), "23+" = c(23, Inf)))

  analysis <- exp$define()[[1]]
  expect_equal(analysis$outputTypes, c("prevalence", "demographics"))
  expect_equal(analysis$ageGroups, list("18-22" = c(18, 22), "23+" = c(23, Inf)))
})

test_that("CohortPrevalenceExperiment validates demographics prerequisites", {
  exp <- CohortPrevalenceExperiment$new("invalid demographics settings")
  exp$addCohorts(tibble::tibble(cohortId = 1, cohortName = "Test cohort"))
  exp$addPrevalenceTypes(list(createPrevalenceType("point_prevalence", lookBackDays = 365L)))
  exp$addDemographicConstraints(list(createDemographicConstraints()))
  exp$addPeriodsOfInterest(list(createYearlyRange(2020:2021)))

  expect_error(
    exp$setCommonParameters(outputTypes = c("prevalence", "demographics")),
    "requires at least one demographic stratum"
  )
})

test_that("CohortPrevalenceExperiment warns when minimumObservationLength is passed to setCommonParameters", {
  exp <- CohortPrevalenceExperiment$new("removed common parameter")

  expect_warning(
    exp$setCommonParameters(outputTypes = "prevalence", minimumObservationLength = 365L),
    "removed"
  )
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
