# Test createCohortPrevalenceAnalysis
test_that("createCohortPrevalenceAnalysis creates valid object with required parameters", {
  prevalentCohort <- createPrevalenceCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  lookBackOptions <- createLookBackOptions(365)
  denominatorType <- createDenominatorType("pd3")
  
  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    lookBackOptions = lookBackOptions,
    numeratorType = "pn1",
    denominatorType = denominatorType
  )
  
  expect_r6_class(analysis, "CohortPrevalenceAnalysis")
  expect_equal(analysis$analysisId, 1)
  expect_equal(analysis$numeratorType, "pn1")
})

test_that("createCohortPrevalenceAnalysis uses default parameters", {
  prevalentCohort <- createPrevalenceCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  lookBackOptions <- createLookBackOptions(365)
  denominatorType <- createDenominatorType("pd3")
  
  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    lookBackOptions = lookBackOptions,
    numeratorType = "pn1",
    denominatorType = denominatorType
  )
  
  expect_equal(analysis$minimumObservationLength, 0L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, FALSE)
  expect_equal(analysis$multiplier, 100000L)
  expect_null(analysis$strata)
})

test_that("createCohortPrevalenceAnalysis accepts custom parameters", {
  prevalentCohort <- createPrevalenceCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  lookBackOptions <- createLookBackOptions(365)
  denominatorType <- createDenominatorType("pd3")
  demographicConstraints <- createDemographicConstraints(ageMin = 18, ageMax = 65)
  populationCohort <- createPopulationCohort(99, "Population")
  
  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 2,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    lookBackOptions = lookBackOptions,
    numeratorType = "pn2",
    denominatorType = denominatorType,
    minimumObservationLength = 365L,
    useOnlyFirstObservationPeriod = TRUE,
    multiplier = 1000000L,
    strata = "age",
    demographicConstraints = demographicConstraints,
    populationCohort = populationCohort
  )
  
  expect_equal(analysis$minimumObservationLength, 365L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, TRUE)
  expect_equal(analysis$multiplier, 1000000L)
  expect_equal(analysis$strata, "age")
})

test_that("createCohortPrevalenceAnalysis accepts multiple strata", {
  prevalentCohort <- createPrevalenceCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  lookBackOptions <- createLookBackOptions(365)
  denominatorType <- createDenominatorType("pd3")
  
  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    lookBackOptions = lookBackOptions,
    numeratorType = "pn1",
    denominatorType = denominatorType,
    strata = c("age", "gender", "race")
  )
  
  expect_equal(analysis$strata, c("age", "gender", "race"))
})

# Test createRassenIncidenceAnalysis
test_that("createRassenIncidenceAnalysis creates valid object with required parameters", {
  targetCohort <- createTargetCohort(1, "Target Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  
  analysis <- createRassenIncidenceAnalysis(
    analysisId = 1,
    targetCohort = targetCohort,
    periodOfInterest = periodOfInterest
  )
  
  expect_r6_class(analysis, "IncidenceAnalysis")
  expect_equal(analysis$analysisId, 1)
})

test_that("createRassenIncidenceAnalysis uses default parameters", {
  targetCohort <- createTargetCohort(1, "Target Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  
  analysis <- createRassenIncidenceAnalysis(
    analysisId = 1,
    targetCohort = targetCohort,
    periodOfInterest = periodOfInterest
  )
  
  expect_equal(analysis$minimumObservationLength, 0L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, FALSE)
  expect_equal(analysis$multiplier, 100000L)
  expect_null(analysis$strata)
})

test_that("createRassenIncidenceAnalysis accepts custom parameters", {
  targetCohort <- createTargetCohort(1, "Target Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  demographicConstraints <- createDemographicConstraints(ageMin = 21, ageMax = 75)
  populationCohort <- createPopulationCohort(99, "Population")
  
  analysis <- createRassenIncidenceAnalysis(
    analysisId = 2,
    targetCohort = targetCohort,
    periodOfInterest = periodOfInterest,
    minimumObservationLength = 730L,
    useOnlyFirstObservationPeriod = TRUE,
    multiplier = 1000000L,
    strata = c("age", "gender"),
    demographicConstraints = demographicConstraints,
    populationCohort = populationCohort
  )
  
  expect_equal(analysis$minimumObservationLength, 730L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, TRUE)
  expect_equal(analysis$multiplier, 1000000L)
})

# Test createPrevalenceCohort
test_that("createPrevalenceCohort creates CohortInfo object", {
  cohort <- createPrevalenceCohort(5, "Diabetes")
  
  expect_r6_class(cohort, "CohortInfo")
  expect_equal(cohort$id(), 5L)
  expect_equal(cohort$name(), "Diabetes")
})

test_that("createPrevalenceCohort coerces cohortId to integer", {
  cohort <- createPrevalenceCohort("10", "Heart Disease")
  
  expect_equal(cohort$id(), 10L)
  expect_type(cohort$id(), "integer")
})

# Test createTargetCohort
test_that("createTargetCohort creates CohortInfo object", {
  cohort <- createTargetCohort(7, "Hypertension")
  
  expect_r6_class(cohort, "CohortInfo")
  expect_equal(cohort$id(), 7L)
  expect_equal(cohort$name(), "Hypertension")
})

# Test createPopulationCohort
test_that("createPopulationCohort creates CohortInfo object", {
  cohort <- createPopulationCohort(15, "General Population")
  
  expect_r6_class(cohort, "CohortInfo")
  expect_equal(cohort$id(), 15L)
  expect_equal(cohort$name(), "General Population")
})

# Test createLookBackOptions
test_that("createLookBackOptions creates object with default parameters", {
  lbo <- createLookBackOptions()
  
  expect_r6_class(lbo, "LookBackOptions")
  expect_equal(lbo$lookBackDays, 99999L)
  expect_equal(lbo$useObservedTimeOnly, FALSE)
})

test_that("createLookBackOptions creates object with custom parameters", {
  lbo <- createLookBackOptions(lookBackDays = 365, useObservedTimeOnly = TRUE)
  
  expect_equal(lbo$lookBackDays, 365L)
  expect_equal(lbo$useObservedTimeOnly, TRUE)
})

test_that("createLookBackOptions accepts various lookBackDays values", {
  lbo1 <- createLookBackOptions(30)
  lbo2 <- createLookBackOptions(1095)
  lbo3 <- createLookBackOptions(0)
  
  expect_equal(lbo1$lookBackDays, 30L)
  expect_equal(lbo2$lookBackDays, 1095L)
  expect_equal(lbo3$lookBackDays, 0L)
})

# Test createYearlyRange
test_that("createYearlyRange creates PeriodOfInterest object", {
  poi <- createYearlyRange(2020:2022)
  
  expect_r6_class(poi, "PeriodOfInterest")
  expect_equal(poi$poiType, "yearly")
  expect_equal(poi$poiRange, 2020:2022)
})

test_that("createYearlyRange accepts single year", {
  poi <- createYearlyRange(2021)
  
  expect_equal(poi$poiRange, 2021)
})

test_that("createYearlyRange accepts multiple years", {
  poi <- createYearlyRange(c(2018, 2019, 2020, 2021, 2022))
  
  expect_length(poi$poiRange, 5)
})

# Test createSpan
test_that("createSpan creates PeriodOfInterest object with numeric dates", {
  poi <- createSpan(c(2020, 2021), c(2020, 2021))
  
  expect_r6_class(poi, "PeriodOfInterest")
  expect_equal(poi$poiType, "span")
  expect_equal(nrow(poi$poiRange), 2)
})

test_that("createSpan converts numeric years to Date format", {
  poi <- createSpan(2020, 2020)
  
  expect_equal(poi$poiRange$calendar_start_date, as.Date("2020-01-01"))
  expect_equal(poi$poiRange$calendar_end_date, as.Date("2020-12-31"))
})

test_that("createSpan handles Date inputs directly", {
  start <- as.Date("2020-06-01")
  end <- as.Date("2020-12-31")
  poi <- createSpan(start, end)
  
  expect_equal(poi$poiRange$calendar_start_date, start)
  expect_equal(poi$poiRange$calendar_end_date, end)
})

test_that("createSpan creates correct span labels", {
  poi <- createSpan(c(2020, 2021), c(2020, 2021))
  
  expect_equal(poi$poiRange$span_label[1], "2020 - 2020")
  expect_equal(poi$poiRange$span_label[2], "2021 - 2021")
})

test_that("createSpan handles mixed numeric and Date inputs", {
  end_dates <- as.Date(c("2020-12-31", "2021-12-31"))
  poi <- createSpan(c(2020, 2021), end_dates)
  
  expect_equal(poi$poiRange$calendar_start_date[1], as.Date("2020-01-01"))
  expect_equal(poi$poiRange$calendar_end_date[1], as.Date("2020-12-31"))
})

# Test createDemographicConstraints
test_that("createDemographicConstraints creates object with default parameters", {
  dc <- createDemographicConstraints()
  
  expect_s3_class(dc, "DemoConstraint")
  expect_equal(dc$ageMin, 0)
  expect_equal(dc$ageMax, 150)
  expect_equal(dc$genderIds, c(8507, 8532))
})

test_that("createDemographicConstraints creates object with custom parameters", {
  dc <- createDemographicConstraints(ageMin = 18, ageMax = 65, genderIds = c(8507))
  
  expect_equal(dc$ageMin, 18)
  expect_equal(dc$ageMax, 65)
  expect_equal(dc$genderIds, c(8507))
})

test_that("createDemographicConstraints accepts various age ranges", {
  dc1 <- createDemographicConstraints(ageMin = 0, ageMax = 18)
  dc2 <- createDemographicConstraints(ageMin = 65, ageMax = 150)
  
  expect_equal(dc1$ageMin, 0)
  expect_equal(dc1$ageMax, 18)
  expect_equal(dc2$ageMin, 65)
  expect_equal(dc2$ageMax, 150)
})

test_that("createDemographicConstraints accepts single gender", {
  dc <- createDemographicConstraints(genderIds = 8507)
  
  expect_equal(dc$genderIds, 8507)
})

# Test createDenominatorType
test_that("createDenominatorType creates object with pd1", {
  dt <- createDenominatorType("pd1")
  
  expect_r6_class(dt, "DenominatorType")
  expect_equal(dt$getDenomType(), "pd1")
  expect_null(dt$getSufficientDays())
})

test_that("createDenominatorType creates object with pd2", {
  dt <- createDenominatorType("pd2")
  
  expect_equal(dt$getDenomType(), "pd2")
})

test_that("createDenominatorType creates object with pd3", {
  dt <- createDenominatorType("pd3")
  
  expect_equal(dt$getDenomType(), "pd3")
})

test_that("createDenominatorType creates object with pd4 and sufficientDays", {
  dt <- createDenominatorType("pd4", sufficientDays = 365)
  
  expect_equal(dt$getDenomType(), "pd4")
  expect_equal(dt$getSufficientDays(), 365)
})
