make_standardization_test_reference <- function(population_overrides = NULL,
                                                omit_strata = NULL) {
  age_population <- c("005" = 100, "018" = 200, "030" = 300, "080" = 400)
  reference_data <- expand.grid(
    age = names(age_population),
    gender = c("Female", "Male"),
    stringsAsFactors = FALSE
  )
  reference_data$population <- unname(age_population[reference_data$age])

  if (!is.null(population_overrides)) {
    for (stratum in names(population_overrides)) {
      cells <- strsplit(stratum, "/", fixed = TRUE)[[1]]
      reference_data$population[
        reference_data$age == cells[1] & reference_data$gender == cells[2]
      ] <- population_overrides[[stratum]]
    }
  }

  if (!is.null(omit_strata)) {
    for (stratum in omit_strata) {
      cells <- strsplit(stratum, "/", fixed = TRUE)[[1]]
      reference_data <- reference_data[
        !(reference_data$age == cells[1] & reference_data$gender == cells[2]),
        ,
        drop = FALSE
      ]
    }
  }

  createStandardizationReference(
    name = "Test reference",
    country = "Test",
    year = 2020L,
    source = "Unit test",
    data = reference_data
  )
}

make_standardization_test_prevalence <- function() {
  prevalence <- expand.grid(
    analysisId = c(1L, 2L),
    spanLabel = c("2020", "2021"),
    age = c(5, 18, 30, 80),
    gender = c(8507L, 8532L),
    KEEP.OUT.ATTRS = FALSE
  )
  prevalence <- prevalence[
    prevalence$analysisId == 2L | prevalence$age %in% c(18, 30),
    ,
    drop = FALSE
  ]
  prevalence$numerator <- dplyr::case_when(
    prevalence$age == 5 ~ 5,
    prevalence$age == 18 & prevalence$gender == 8507L ~ 1,
    prevalence$age == 18 & prevalence$gender == 8532L ~ 2,
    prevalence$age == 30 & prevalence$gender == 8507L ~ 3,
    prevalence$age == 30 & prevalence$gender == 8532L ~ 4,
    prevalence$age == 80 ~ 6
  )
  prevalence$denominator <- 1000
  prevalence
}

test_that("standardization uses separate reference weights per analysis", {
  result <- CohortPrevalence:::standardize_prevalence(
    prevalenceData = make_standardization_test_prevalence(),
    referencePopulation = make_standardization_test_reference()
  )

  adult_results <- result |>
    dplyr::filter(analysisId == 1L) |>
    dplyr::arrange(spanLabel)
  all_ages_results <- result |>
    dplyr::filter(analysisId == 2L) |>
    dplyr::arrange(spanLabel)

  expect_equal(adult_results$stdStat, c(270, 270))
  expect_equal(all_ages_results$stdStat, c(425, 425))
  expect_equal(as.character(adult_results$spanLabel), c("2020", "2021"))
})

test_that("reference data access does not calculate weights", {
  reference <- make_standardization_test_reference()
  reference_data <- reference$getData()

  expect_false("weight" %in% names(reference_data))
  expect_named(reference_data, c("age", "gender", "population"))
})

test_that("ACS age-sex mapping returns counts without precomputed weights", {
  groups <- CohortPrevalence:::acs_age_groups()
  acs_raw <- data.frame(
    variable = c(groups$male_variable, groups$female_variable),
    estimate = rep(100, 46),
    year = rep(2020L, 46),
    stringsAsFactors = FALSE
  )

  mapped_data <- CohortPrevalence:::map_acs_b01001_to_age_sex(acs_raw)

  expect_equal(nrow(mapped_data), 46L)
  expect_false("weight" %in% names(mapped_data))
})

test_that("age labels parse into inclusive numeric intervals", {
  parsed <- CohortPrevalence:::.parse_age_labels(
    c("018", "5-9", "85+", "Under 5")
  )

  expect_equal(parsed$min_age, c(18, 5, 85, 0))
  expect_equal(parsed$max_age, c(18, 9, Inf, 4))
  expect_equal(parsed$age_type, c("single", "range", "open", "under"))
})

test_that("reference construction rejects malformed and overlapping age bands", {
  make_reference <- function(ages) {
    CohortPrevalence:::StandardizationReference$new(
      name = "Invalid reference",
      country = "Test",
      year = 2020L,
      source = "Unit test",
      data = data.frame(
        age = ages,
        gender = rep("Female", length(ages)),
        population = rep(100, length(ages)),
        stringsAsFactors = FALSE
      )
    )
  }

  expect_error(make_reference(c("0-4", "five")), "malformed")
  expect_error(make_reference(c("5-9", "8-12")), "Overlapping")
  expect_error(make_reference("9-5"), "Invalid reference age interval")
})

test_that("mapping and truncation use the same parsed age bands", {
  reference <- CohortPrevalence:::StandardizationReference$new(
    name = "Grouped reference",
    country = "Test",
    year = 2020L,
    source = "Unit test",
    data = data.frame(
      age = c("Under 5", "5-9", "10+"),
      gender = "Female",
      population = c(50, 50, 100),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    reference$mapAgesToReference(c(2, 7, 10)),
    c("Under 5", "5-9", "10+")
  )
  expect_equal(reference$mapAgesToReference("10+"), "10+")
  expect_true(is.na(reference$mapAgesToReference("11+")))
  expect_equal(reference$getValidTruncationPoints(), c(0, 5, 10))
  expect_equal(reference$validateRightTruncation(5), 5)
  expect_error(reference$validateRightTruncation(7), "falls within group '5-9'")

  truncated <- reference$getAdjustedReference(rightTruncation = 5)
  expect_setequal(truncated$age, c("Under 5", "5+"))
  expect_equal(truncated$population[truncated$age == "5+"], 150)
})

test_that("single-year ages map to each reference's exact age labels", {
  label_cases <- list(
    list(labels = c("018", "019"), expected = c("018", "019")),
    list(labels = c("18", "19"), expected = c("18", "19"))
  )

  for (case in label_cases) {
    reference <- CohortPrevalence:::StandardizationReference$new(
      name = "Single-year reference",
      country = "Test",
      year = 2020L,
      source = "Unit test",
      data = data.frame(
        age = case$labels,
        gender = "Female",
        population = c(100, 100),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(reference$mapAgesToReference(c(18, 19)), case$expected)
    expect_true(is.na(reference$mapAgesToReference(20)))
    expect_true(is.na(reference$mapAgesToReference(NA_character_)))
    expect_equal(
      reference$mapAgesToReference("18+", rightTruncation = 18),
      "18+"
    )
    expect_true(is.na(
      reference$mapAgesToReference("19+", rightTruncation = 18)
    ))
  }
})

test_that("standardization maps generated truncation labels to adjusted reference cells", {
  result <- CohortPrevalence:::standardize_prevalence(
    prevalenceData = make_standardization_test_prevalence(),
    referencePopulation = make_standardization_test_reference(),
    ageRightTruncation = 30
  )

  expect_equal(nrow(result), 4L)
  expect_true(all(is.finite(result$stdStat)))
})

test_that("standardization reports unmapped ages with analysis context", {
  prevalence <- make_standardization_test_prevalence()
  prevalence$age[1] <- 99
  expected_detail <- paste0(
    "analysisId=", prevalence$analysisId[[1]],
    ", spanLabel=", prevalence$spanLabel[[1]],
    ", age=99"
  )

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = prevalence,
      referencePopulation = make_standardization_test_reference()
    ),
    expected_detail
  )
})

test_that("standardization errors when a span is missing an analysis stratum", {
  prevalence <- make_standardization_test_prevalence()
  prevalence <- prevalence[
    !(prevalence$analysisId == 1L &
        prevalence$spanLabel == "2021" &
        prevalence$age == 30 &
        prevalence$gender == 8532L),
    ,
    drop = FALSE
  ]

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = prevalence,
      referencePopulation = make_standardization_test_reference()
    ),
    "analysisId=1, spanLabel=2021, age=030, gender=Female"
  )
})

test_that("standardization errors when prevalence strata are absent from the reference", {
  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = make_standardization_test_prevalence(),
      referencePopulation = make_standardization_test_reference(
        omit_strata = "030/Female"
      )
    ),
    "analysisId=1, age=030, gender=Female"
  )
})

test_that("standardization errors when matched reference population totals zero", {
  reference <- make_standardization_test_reference(
    population_overrides = c(
      "018/Female" = 0,
      "018/Male" = 0,
      "030/Female" = 0,
      "030/Male" = 0
    )
  )

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = make_standardization_test_prevalence(),
      referencePopulation = reference
    ),
    "finite, positive total.*1"
  )
})

test_that("standardization rejects explicitly represented zero-denominator strata", {
  prevalence <- make_standardization_test_prevalence()
  prevalence$denominator[[1]] <- 0

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = prevalence,
      referencePopulation = make_standardization_test_reference()
    ),
    "non-finite or non-positive denominators.*age=005"
  )
})

test_that("age bounds are deprecated on the public standardization method", {
  results <- PrevalenceResults$new(
    prevalence = make_standardization_test_prevalence()
  )

  expect_warning(
    results$standardizePrevalence(
      referencePopulation = make_standardization_test_reference(),
      ageMin = 18,
      ageMax = 30
    ),
    "ageMin.*ageMax.*deprecated"
  )

  expect_identical(results$standardizationApplied$ageMin, 18)
  expect_identical(results$standardizationApplied$ageMax, 30)
  expect_equal(nrow(results$stdPrev), 4L)
})

test_that("filtered reference helper warns while remaining available", {
  reference <- make_standardization_test_reference()

  expect_warning(
    filtered_reference <- reference$getFilteredReference(ageMin = 18, ageMax = 30),
    "getFilteredReference.*deprecated"
  )

  expect_setequal(filtered_reference$age, c("018", "030"))
  expect_false("weight" %in% names(filtered_reference))
})

test_that("adjusted reference applies only right truncation", {
  reference <- make_standardization_test_reference()
  truncated_reference <- reference$getAdjustedReference(rightTruncation = 30)

  expect_setequal(truncated_reference$age, c("005", "018", "30+"))
  expect_false("weight" %in% names(truncated_reference))
  expect_equal(
    truncated_reference$population[truncated_reference$age == "30+"],
    c(700, 700)
  )
})
