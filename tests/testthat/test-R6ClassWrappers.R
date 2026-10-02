# Test createPrevalenceType
test_that("createPrevalenceType creates valid object", {
  pt <- createPrevalenceType("point_prevalence", lookBackDays = 365)

  expect_r6_class(pt, "PrevalenceType")
  expect_equal(pt$prevalenceType, "point_prevalence")
  expect_equal(pt$lookBackDays, 365L)
})

test_that("createPrevalenceType defaults leadInDays to zero and accepts a value", {
  pt <- createPrevalenceType("point_prevalence", lookBackDays = 365)
  expect_equal(pt$leadInDays, 0L)
  expect_match(pt$getLeadInLabel(), "No lead-in")

  pt <- createPrevalenceType("point_prevalence", lookBackDays = 365, leadInDays = 730L)
  expect_equal(pt$leadInDays, 730L)
  expect_match(pt$getLeadInLabel(), "730-day lead-in")

  expect_error(createPrevalenceType("point_prevalence", lookBackDays = 365, leadInDays = -1L))
})

test_that("createPrevalenceType accepts all valid types", {
  expect_r6_class(createPrevalenceType("point_prevalence", 365), "PrevalenceType")
  expect_r6_class(createPrevalenceType("period_prevalence_pd2", Inf), "PrevalenceType")
  expect_r6_class(createPrevalenceType("period_prevalence_pd3", Inf), "PrevalenceType")
  expect_r6_class(createPrevalenceType("period_prevalence_pd4", Inf, sufficientDays = 90L), "PrevalenceType")
  expect_error(createPrevalenceType("period_prevalence_pd4", Inf), "sufficientDays")
  expect_error(createPrevalenceType("period_prevalence_pd4", Inf, sufficientDays = 0L))
  expect_error(createPrevalenceType("period_prevalence_pd2", Inf, sufficientDays = 90L))
})

test_that("PD4 sufficientDays is retained and displayed", {
  pt <- createPrevalenceType("period_prevalence_pd4", 365, sufficientDays = 90L)

  expect_equal(pt$getSufficientDays(), 90L)
  expect_equal(pt$sufficientDays, 90L)
  expect_match(pt$viewPrevalenceType(), "Minimum observed days in POI: 90")
})

test_that("PD4 can be selected after setting its threshold", {
  pt <- createPrevalenceType("period_prevalence_pd3", 365)

  pt$sufficientDays <- 90L
  pt$prevalenceType <- "period_prevalence_pd4"

  expect_equal(pt$getSufficientDays(), 90L)
})

test_that("createPrevalenceType coerces 0 to Inf", {
  pt <- createPrevalenceType("point_prevalence", 0)
  expect_true(is.infinite(pt$lookBackDays))

  pt <- createPrevalenceType("period_prevalence_pd2", 0)
  expect_true(is.infinite(pt$lookBackDays))

  pt <- createPrevalenceType("period_prevalence_pd3", 0)
  expect_true(is.infinite(pt$lookBackDays))

  pt <- createPrevalenceType("period_prevalence_pd4", 0, sufficientDays = 90L)
  expect_true(is.infinite(pt$lookBackDays))
})

# Test createCohortPrevalenceAnalysis
test_that("createCohortPrevalenceAnalysis creates valid object with required parameters", {
  prevalentCohort <- createTargetCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  prevalenceType <- createPrevalenceType("point_prevalence", lookBackDays = 365)

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType
  )

  expect_r6_class(analysis, "CohortPrevalenceAnalysis")
  expect_equal(analysis$analysisId, 1)
})

test_that("createCohortPrevalenceAnalysis uses default parameters", {
  prevalentCohort <- createTargetCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  prevalenceType <- createPrevalenceType("point_prevalence", lookBackDays = 365)

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType
  )

  expect_equal(analysis$prevalenceType$leadInDays, 0L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, FALSE)
  expect_equal(analysis$multiplier, 100000L)
  expect_null(analysis$strata)
})

test_that("createCohortPrevalenceAnalysis accepts custom parameters", {
  prevalentCohort <- createTargetCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  prevalenceType <- createPrevalenceType("period_prevalence_pd3", lookBackDays = 0, leadInDays = 365L)
  demographicConstraints <- createDemographicConstraints(ageMin = 18, ageMax = 65)
  populationCohort <- createPopulationCohort(99, "Population")

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 2,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType,
    useOnlyFirstObservationPeriod = TRUE,
    multiplier = 1000000L,
    strata = "age",
    demographicConstraints = demographicConstraints,
    populationCohort = populationCohort
  )

  expect_equal(analysis$prevalenceType$leadInDays, 365L)
  expect_equal(analysis$useOnlyFirstObservationPeriod, TRUE)
  expect_equal(analysis$multiplier, 1000000L)
  expect_equal(analysis$strata, "age")
})

test_that("createCohortPrevalenceAnalysis accepts multiple strata", {
  prevalentCohort <- createTargetCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2022)
  prevalenceType <- createPrevalenceType("point_prevalence", lookBackDays = 365)

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType,
    strata = c("age", "gender", "race")
  )

  expect_equal(analysis$strata, c("age", "gender", "race"))
})

test_that("demographics output requires prevalence and at least one demographic stratum", {
  commonArgs <- list(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365)
  )

  expect_error(
    do.call(createCohortPrevalenceAnalysis, c(commonArgs, list(outputTypes = "demographics", strata = "age"))),
    "requires 'prevalence'"
  )
  expect_error(
    do.call(createCohortPrevalenceAnalysis, c(commonArgs, list(outputTypes = c("prevalence", "demographics")))),
    "requires at least one demographic stratum"
  )

  analysis <- do.call(
    createCohortPrevalenceAnalysis,
    c(commonArgs, list(outputTypes = c("prevalence", "demographics"), strata = "gender"))
  )
  expect_equal(analysis$outputTypes, c("prevalence", "demographics"))

  expect_error(analysis$outputTypes <- "demographics", "requires 'prevalence'")
  expect_error(analysis$strata <- NULL, "requires at least one demographic stratum")
})

test_that("ageGroups accepts ordered non-overlapping inclusive ranges", {
  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365),
    strata = "age",
    outputTypes = c("prevalence", "demographics"),
    ageGroups = list("18-22" = c(18, 22), "23+" = c(23, Inf))
  )

  expect_equal(analysis$ageGroups, list("18-22" = c(18, 22), "23+" = c(23, Inf)))

  commonArgs <- list(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365)
  )

  expect_error(
    do.call(createCohortPrevalenceAnalysis, c(commonArgs, list(ageGroups = list("overlap1" = c(18, 25), "overlap2" = c(25, 40))))),
    "must not overlap"
  )
  expect_error(
    do.call(createCohortPrevalenceAnalysis, c(commonArgs, list(ageGroups = list("later" = c(30, 40), "earlier" = c(18, 29))))),
    "ordered by increasing lower bound"
  )
  expect_error(
    do.call(createCohortPrevalenceAnalysis, c(commonArgs, list(strata = "gender", ageGroups = list("18+" = c(18, Inf))))),
    "only be supplied"
  )
})

test_that("assembleSql exposes race and ethnicity columns and respects requested strata", {
  # Minimal stand-in for ExecutionSettings (defined in the companion
  # ClinicalCharacteristics package, not a dependency here). assembleSql()
  # only needs getDbms() and tempEmulationSchema to render the year-interval
  # insert statement.
  executionSettings <- structure(
    list(
      getDbms = function() "postgresql",
      tempEmulationSchema = NULL
    ),
    class = "ExecutionSettings"
  )

  prevalentCohort <- createTargetCohort(1, "Test Cohort")
  periodOfInterest <- createYearlyRange(2020:2021)
  prevalenceType <- createPrevalenceType("point_prevalence", lookBackDays = 365)

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = prevalentCohort,
    periodOfInterest = periodOfInterest,
    prevalenceType = prevalenceType,
    strata = c("age", "gender", "race", "ethnicity")
  )

  sql <- analysis$assembleSql(executionSettings)

  # obsPopYear always exposes race/ethnicity from the person table,
  # regardless of which strata were requested for this analysis.
  expect_match(sql, "race_concept_id AS race", fixed = TRUE)
  expect_match(sql, "ethnicity_concept_id AS ethnicity", fixed = TRUE)

  # the requested strata must drive both the pd1 denominator SELECT/GROUP BY...
  expect_match(
    sql,
    "GROUP BY subject_id, span_label, calendar_start_date, calendar_end_date,age, gender, race, ethnicity",
    fixed = TRUE
  )

  # ...and the final #prevalence aggregation SELECT/GROUP BY
  expect_match(sql, "SELECT\n  span_label,age, gender, race, ethnicity", fixed = TRUE)
  expect_match(sql, "GROUP BY span_label,age, gender, race, ethnicity;", fixed = TRUE)
})

test_that("assembleSql omits ungrouped strata from the aggregation GROUP BY", {
  executionSettings <- structure(
    list(
      getDbms = function() "postgresql",
      tempEmulationSchema = NULL
    ),
    class = "ExecutionSettings"
  )

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365),
    strata = "ethnicity"
  )

  sql <- analysis$assembleSql(executionSettings)

  # race/ethnicity columns are always selected upstream in obsPopYear...
  expect_match(sql, "race_concept_id AS race", fixed = TRUE)
  expect_match(sql, "ethnicity_concept_id AS ethnicity", fixed = TRUE)

  # ...but only the requested stratum reaches the final aggregation
  expect_match(sql, "GROUP BY span_label,ethnicity;", fixed = TRUE)
  expect_no_match(sql, "GROUP BY span_label,ethnicity, race")
})

test_that("assembleSql bounds incidence person-time by observation and event dates", {
  executionSettings <- structure(
    list(
      getDbms = function() "postgresql",
      tempEmulationSchema = NULL
    ),
    class = "ExecutionSettings"
  )

  analysis <- createCohortPrevalenceAnalysis(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = createPrevalenceType("point_prevalence", lookBackDays = 365),
    strata = "gender",
    outputTypes = "incidence"
  )

  sql <- analysis$assembleSql(executionSettings)

  expect_match(sql, "observation_period_start_date", fixed = TRUE)
  expect_match(sql, "observation_period_end_date", fixed = TRUE)
  expect_match(sql, "DATEADD(day, -1, event_status.incident_date)", fixed = TRUE)
  expect_match(sql, "MAX(inc_event) AS inc_event", fixed = TRUE)
  expect_match(sql, "SUM(time_at_risk) AS time_at_risk", fixed = TRUE)
  expect_match(sql, "FROM #denomInc", fixed = TRUE)
  expect_match(
    sql,
    "CAST(SUM(inc_event) AS FLOAT) / NULLIF(SUM(time_at_risk) / 365.25, 0)",
    fixed = TRUE
  )
})

test_that("assembleSql builds only selected demographic summaries after #allEvents", {
  executionSettings <- structure(
    list(
      getDbms = function() "postgresql",
      tempEmulationSchema = NULL
    ),
    class = "ExecutionSettings"
  )

  prevalenceType <- createPrevalenceType("point_prevalence", lookBackDays = 365)
  baseArgs <- list(
    analysisId = 1,
    prevalentCohort = createTargetCohort(1, "Test Cohort"),
    periodOfInterest = createYearlyRange(2020:2021),
    prevalenceType = prevalenceType,
    strata = "gender"
  )

  prevalenceOnlyAnalysis <- do.call(createCohortPrevalenceAnalysis, baseArgs)
  prevalenceOnlySql <- prevalenceOnlyAnalysis$assembleSql(executionSettings)

  expect_no_match(prevalenceOnlySql, "#demographics")

  demographicsAnalysis <- do.call(
    createCohortPrevalenceAnalysis,
    c(baseArgs, list(outputTypes = c("prevalence", "demographics")))
  )
  sql <- demographicsAnalysis$assembleSql(executionSettings)

  expect_match(sql, "CREATE TEMP TABLE #demographics AS", fixed = TRUE)
  expect_match(sql, "'gender' AS demographic", fixed = TRUE)
  expect_no_match(sql, "'age' AS demographic")
  expect_no_match(sql, "'race' AS demographic")
  expect_no_match(sql, "'ethnicity' AS demographic")

  allEventsPosition <- regexpr("CREATE TEMP TABLE #allEvents AS", sql, fixed = TRUE)[[1]]
  prevalencePosition <- regexpr("CREATE TABLE #prevalence AS", sql, fixed = TRUE)[[1]]
  demographicsPosition <- regexpr("CREATE TEMP TABLE #demographics AS", sql, fixed = TRUE)[[1]]
  expect_lt(allEventsPosition, prevalencePosition)
  expect_lt(prevalencePosition, demographicsPosition)
})

test_that("demographics SQL works with each PD era template and selected dimensions", {
  executionSettings <- structure(
    list(
      getDbms = function() "postgresql",
      tempEmulationSchema = NULL
    ),
    class = "ExecutionSettings"
  )

  prevalenceTypes <- list(
    createPrevalenceType("point_prevalence", lookBackDays = 365),
    createPrevalenceType("period_prevalence_pd2", lookBackDays = 365),
    createPrevalenceType("period_prevalence_pd3", lookBackDays = 365),
    createPrevalenceType("period_prevalence_pd4", lookBackDays = 365, sufficientDays = 30)
  )

  for (prevalenceType in prevalenceTypes) {
    analysis <- createCohortPrevalenceAnalysis(
      analysisId = 1,
      prevalentCohort = createTargetCohort(1, "Test Cohort"),
      periodOfInterest = createYearlyRange(2020:2021),
      prevalenceType = prevalenceType,
      strata = c("age", "ethnicity"),
      outputTypes = c("prevalence", "demographics")
    )

    sql <- analysis$assembleSql(executionSettings)

    expect_match(sql, "'age' AS demographic", fixed = TRUE)
    expect_match(sql, "'ethnicity' AS demographic", fixed = TRUE)
    expect_no_match(sql, "'gender' AS demographic")
    expect_no_match(sql, "'race' AS demographic")
  }
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

# Test createTargetCohort
test_that("createTargetCohort creates TargetCohort object", {
  cohort <- createTargetCohort(5, "Diabetes")

  expect_r6_class(cohort, "TargetCohort")
  expect_equal(cohort$id(), 5L)
  expect_equal(cohort$name(), "Diabetes")
})

test_that("createTargetCohort defaults to era mode", {
  cohort <- createTargetCohort(1, "Derived Cohort")

  expect_equal(cohort$getCircePattern(), "era")
})

test_that("createTargetCohort coerces cohortId to integer", {
  cohort <- createTargetCohort("10", "Heart Disease")

  expect_equal(cohort$id(), 10L)
  expect_type(cohort$id(), "integer")
})

# Test createPopulationCohort
test_that("createPopulationCohort creates PopulationCohort object", {
  cohort <- createPopulationCohort(15, "General Population")

  expect_r6_class(cohort, "PopulationCohort")
  expect_equal(cohort$id(), 15L)
  expect_equal(cohort$name(), "General Population")
})

test_that("createPopulationCohort coerces cohortId to integer", {
  cohort <- createPopulationCohort("20", "Population")

  expect_equal(cohort$id(), 20L)
  expect_type(cohort$id(), "integer")
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

  expect_r6_class(dc, "DemoConstraint")
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
