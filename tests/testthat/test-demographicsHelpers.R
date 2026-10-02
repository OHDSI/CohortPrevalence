test_that("cleanDemographicsResults adds known labels and analysis metadata", {
  demographicsData <- tibble::tibble(
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
    cohortName = "Example cohort",
    databaseId = "Example database"
  )

  ageResults <- results[results$demographic == "age", ]
  expect_equal(ageResults$demographicValue, c("18-22", "23+", "Missing", "Other/Unmapped"))
  expect_equal(ageResults$demographicLabel, ageResults$demographicValue)
  expect_equal(ageResults$caseCount, c(5, 4, 1, 1))
  expect_equal(ageResults$totalCases, rep(11, 4))
  expect_equal(ageResults$proportion, c(5, 4, 1, 1) / 11)
  expect_equal(ageResults$ageMean, rep(21.1, 4))
  expect_equal(ageResults$ageMin, rep(17, 4))
  expect_equal(ageResults$ageMedian, rep(22, 4))
  expect_equal(ageResults$ageMax, rep(23, 4))
  expect_equal(ageResults$ageSd, rep(stats::sd(c(18, 18, 22, 22, 22, 23, 23, 23, 23, 17)), 4))

  genderResults <- results[results$demographic == "gender", ]
  expect_equal(genderResults$demographicValue, c("8507", "999", NA_character_))
  expect_equal(genderResults$demographicLabel, c("Male", "999", "Missing"))
  expect_equal(genderResults$demographicValue[genderResults$demographicLabel == "Male"], "8507")

  raceResult <- results[results$demographic == "race", ]
  ethnicityResult <- results[results$demographic == "ethnicity", ]
  expect_equal(raceResult$demographicLabel, "White")
  expect_equal(ethnicityResult$demographicLabel, "Hispanic or Latino")

  expect_equal(unique(results$analysisId), 42L)
  expect_equal(unique(results$cohortId), 7L)
  expect_equal(unique(results$cohortName), "Example cohort")
  expect_equal(unique(results$databaseId), "Example database")
  expect_equal(unique(results$statType), "Demographics")
})

test_that("cleanDemographicsResults summarizes continuous age when age groups are omitted", {
  demographicsData <- tibble::tibble(
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
    cohortName = "Example cohort",
    databaseId = "Example database"
  )

  expect_equal(results$demographicValue, c("All ages", "Missing"))
  expect_equal(results$demographicLabel, c("All ages", "Missing"))
  expect_equal(results$caseCount, c(3, 1))
  expect_equal(results$proportion, c(0.75, 0.25))
  expect_equal(results$ageMean, c(18 + 2 / 3, 18 + 2 / 3))
  expect_equal(results$ageMin, c(18, 18))
  expect_equal(results$ageMedian, c(18, 18))
  expect_equal(results$ageMax, c(20, 20))
  expect_equal(results$ageSd, rep(stats::sd(c(18, 18, 20)), 2))
})

test_that("cleanDemographicsResults preserves raw values and labels known race and ethnicity IDs", {
  demographicsData <- tibble::tibble(
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
    cohortName = "Example cohort",
    databaseId = "Example database"
  )

  expect_equal(results$demographicValue, c("38003564", "8516", "8522", "8657"))
  expect_equal(
    results$demographicLabel,
    c("Not Hispanic or Latino", "Black or African American", "Native Hawaiian or Other Pacific Islander", "Other Race")
  )
})
