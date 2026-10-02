test_that("cleanDemographicsResults returns tidy statistics with labels and metadata", {
  demographicsData <- tibble::tibble(
    measureType = rep("prevalence", 10),
    spanLabel = rep("2020", 10),
    demographic = c(
      rep("age", 5),
      rep("gender", 3),
      "race",
      "ethnicity"
    ),
    demographicValue = c(
      "18", "22", "23", "17", NA_character_,
      "8507", "999", NA_character_,
      "8527",
      "38003563"
    ),
    caseCount = c(2L, 3L, 4L, 1L, 1L, 6L, 2L, 1L, 5L, 7L),
    totalCases = rep(11L, 10),
    proportion = c(2, 3, 4, 1, 1, 6, 2, 1, 5, 7) / 11
  )

  results <- cleanDemographicsResults(
    demographicsData = demographicsData,
    ageGroups = list("18-22" = c(18, 22), "23+" = c(23, Inf)),
    analysisId = 42L,
    cohortId = 7L,
    cohortName = "Example cohort"
  )

  expect_named(
    results,
    c(
      "analysisId", "cohortId", "cohortName", "statType", "measureType", "spanLabel",
      "demographic", "demographicId", "demographicLabel", "stat", "value"
    )
  )
  expect_equal(unique(results$measureType), "prevalence")
  expect_equal(unique(results$analysisId), 42L)
  expect_equal(unique(results$cohortId), 7L)
  expect_equal(unique(results$cohortName), "Example cohort")
  expect_equal(unique(results$statType), "Demographics")

  get_value <- function(demographic, demographicId, stat) {
    results$value[
      results$demographic == demographic &
        results$demographicId == demographicId &
        results$stat == stat
    ]
  }

  expect_equal(as.numeric(get_value("age", "18-22", "caseCount")), 5)
  expect_equal(as.numeric(get_value("age", "23+", "caseCount")), 4)
  expect_equal(as.numeric(get_value("age", "Other/Unmapped", "caseCount")), 1)
  expect_equal(as.numeric(get_value("age", "Missing", "caseCount")), 1)
  expect_equal(as.numeric(get_value("age", "18-22", "proportion")), 5 / 11)
  expect_equal(as.numeric(get_value("age", "All ages", "ageMean")), 21.1)
  expect_equal(as.numeric(get_value("age", "All ages", "ageMin")), 17)
  expect_equal(as.numeric(get_value("age", "All ages", "ageMedian")), 22)
  expect_equal(as.numeric(get_value("age", "All ages", "ageMax")), 23)
  expect_equal(
    as.numeric(get_value("age", "All ages", "ageStd")),
    stats::sd(c(18, 18, 22, 22, 22, 23, 23, 23, 23, 17))
  )

  get_label <- function(demographic, demographicId) {
    unique(results$demographicLabel[
      results$demographic == demographic & results$demographicId == demographicId
    ])
  }

  expect_equal(get_label("gender", "8507"), "Male")
  expect_equal(get_label("gender", "999"), "999")
  expect_equal(get_label("gender", "Missing"), "Missing")
  expect_equal(get_label("race", "8527"), "White")
  expect_equal(get_label("ethnicity", "38003563"), "Hispanic or Latino")
})

test_that("cleanDemographicsResults summarizes continuous age without configured groups", {
  demographicsData <- tibble::tibble(
    measureType = rep("prevalence", 3),
    spanLabel = rep("2020", 3),
    demographic = rep("age", 3),
    demographicValue = c("18", "20", NA_character_),
    caseCount = c(2L, 1L, 1L),
    totalCases = rep(4L, 3),
    proportion = c(0.5, 0.25, 0.25)
  )

  results <- cleanDemographicsResults(
    demographicsData = demographicsData,
    analysisId = 1L,
    cohortId = 2L,
    cohortName = "Example cohort"
  )

  allAges <- results[
    results$demographic == "age" & results$demographicId == "All ages",
  ]
  get_value <- function(stat) allAges$value[allAges$stat == stat]

  expect_equal(as.numeric(get_value("caseCount")), 3)
  expect_equal(as.numeric(get_value("proportion")), 0.75)
  expect_equal(as.numeric(get_value("ageMean")), 18 + 2 / 3)
  expect_equal(as.numeric(get_value("ageMin")), 18)
  expect_equal(as.numeric(get_value("ageMedian")), 18)
  expect_equal(as.numeric(get_value("ageMax")), 20)
  expect_equal(as.numeric(get_value("ageStd")), stats::sd(c(18, 18, 20)))
  expect_equal(
    as.numeric(results$value[results$demographicId == "Missing" & results$stat == "caseCount"]),
    1
  )
})

test_that("cleanDemographicsResults retains raw concept IDs and known labels", {
  demographicsData <- tibble::tibble(
    measureType = rep("prevalence", 4),
    spanLabel = rep("2020", 4),
    demographic = c("race", "race", "race", "ethnicity"),
    demographicValue = c("8516", "8522", "8657", "38003564"),
    caseCount = c(3L, 1L, 1L, 4L),
    totalCases = rep(5L, 4),
    proportion = c(0.6, 0.2, 0.2, 0.8)
  )

  results <- cleanDemographicsResults(
    demographicsData = demographicsData,
    analysisId = 1L,
    cohortId = 2L,
    cohortName = "Example cohort"
  )

  raceLabels <- unique(results[results$demographic == "race", c("demographicId", "demographicLabel")])
  ethnicityLabel <- unique(results[results$demographic == "ethnicity", c("demographicId", "demographicLabel")])
  expect_equal(raceLabels$demographicId, c("8516", "8522", "8657"))
  expect_equal(raceLabels$demographicLabel, c(
    "Black or African American",
    "Native Hawaiian or Other Pacific Islander",
    "Other Race"
  ))
  expect_equal(ethnicityLabel$demographicId, "38003564")
  expect_equal(ethnicityLabel$demographicLabel, "Not Hispanic or Latino")
})

test_that("cleanDemographicsResults requires an explicit measureType", {
  demographicsData <- tibble::tibble(
    spanLabel = "2020",
    demographic = "gender",
    demographicValue = "8507",
    caseCount = 1L,
    totalCases = 1L,
    proportion = 1
  )

  expect_error(
    cleanDemographicsResults(
      demographicsData = demographicsData,
      analysisId = 1L,
      cohortId = 2L,
      cohortName = "Example cohort"
    ),
    "measureType"
  )
})

test_that("age summaries and proportions remain separate by measure", {
  demographicsData <- tibble::tibble(
    measureType = rep(c("prevalence", "incidence"), each = 3),
    spanLabel = rep("2020", 6),
    demographic = rep("age", 6),
    demographicValue = rep(c("10", "20", NA_character_), 2),
    caseCount = c(2L, 1L, 1L, 1L, 3L, 2L),
    totalCases = c(rep(4L, 3), rep(6L, 3)),
    proportion = caseCount / totalCases
  )

  results <- cleanDemographicsResults(
    demographicsData = demographicsData,
    analysisId = 1L,
    cohortId = 2L,
    cohortName = "Example cohort"
  )

  allAges <- results[
    results$demographic == "age" & results$demographicId == "All ages",
  ]
  prevalenceRows <- allAges[allAges$measureType == "prevalence", ]
  incidenceRows <- allAges[allAges$measureType == "incidence", ]

  expect_equal(as.numeric(prevalenceRows$value[prevalenceRows$stat == "caseCount"]), 3)
  expect_equal(as.numeric(incidenceRows$value[incidenceRows$stat == "caseCount"]), 4)
  expect_equal(as.numeric(prevalenceRows$value[prevalenceRows$stat == "proportion"]), 0.75)
  expect_equal(as.numeric(incidenceRows$value[incidenceRows$stat == "proportion"]), 4 / 6)
  expect_equal(as.numeric(prevalenceRows$value[prevalenceRows$stat == "ageMean"]), 40 / 3)
  expect_equal(as.numeric(incidenceRows$value[incidenceRows$stat == "ageMean"]), 17.5)
})
