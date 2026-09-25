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

test_that("standardization rejects unsupported gender concept IDs", {
  prevalence <- make_standardization_test_prevalence()
  prevalence$gender[[1]] <- 9999L

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = prevalence,
      referencePopulation = make_standardization_test_reference()
    ),
    "unsupported gender concept IDs.*9999"
  )
})

test_that("built-in standardization references are sex-stratified", {
  references <- listStandardizationReferences()

  expect_equal(references$name, c("usa_census_2020", "japan_census_2020"))
  expect_error(getStandardizationReference("who_world_standard"), "Unknown reference")
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
    variable = c("B01001_001", groups$male_variable, groups$female_variable),
    estimate = c(4600, rep(100, 46)),
    year = rep(2020L, 47),
    stringsAsFactors = FALSE
  )

  mapped_data <- CohortPrevalence:::map_acs_b01001_to_age_sex(acs_raw)

  expect_equal(nrow(mapped_data), 46L)
  expect_false("weight" %in% names(mapped_data))
})

test_that("ACS mapping validates exact coverage, estimates, and totals", {
  groups <- CohortPrevalence:::acs_age_groups()
  valid_input <- data.frame(
    variable = c("B01001_001", groups$male_variable, groups$female_variable),
    estimate = c(4600, rep(100, 46)),
    year = rep(2020L, 47),
    stringsAsFactors = FALSE
  )

  expect_error(
    CohortPrevalence:::map_acs_b01001_to_age_sex(
      valid_input[valid_input$variable != "B01001_003", ]
    ),
    "exactly 46 age/sex rows"
  )

  duplicate_input <- rbind(valid_input, valid_input[valid_input$variable == "B01001_003", ])
  expect_error(
    CohortPrevalence:::map_acs_b01001_to_age_sex(duplicate_input),
    "duplicate age/sex variables"
  )

  missing_total_input <- valid_input[valid_input$variable != "B01001_001", ]
  expect_error(
    CohortPrevalence:::map_acs_b01001_to_age_sex(missing_total_input),
    "missing B01001_001 total"
  )

  invalid_estimate_input <- valid_input
  invalid_estimate_input$estimate[invalid_estimate_input$variable == "B01001_003"] <- -1
  expect_error(
    CohortPrevalence:::map_acs_b01001_to_age_sex(invalid_estimate_input),
    "finite and non-negative"
  )

  inconsistent_total_input <- valid_input
  inconsistent_total_input$estimate[inconsistent_total_input$variable == "B01001_001"] <- 4500
  expect_error(
    CohortPrevalence:::map_acs_b01001_to_age_sex(inconsistent_total_input),
    "do not reconcile"
  )

  two_year_input <- rbind(
    valid_input,
    transform(valid_input, year = 2021L)
  )
  expect_equal(
    nrow(CohortPrevalence:::map_acs_b01001_to_age_sex(two_year_input)),
    92L
  )
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

test_that("reference construction validates keys, counts, totals, and empty input", {
  make_reference <- function(age, gender = rep("Female", length(age)), population) {
    CohortPrevalence:::StandardizationReference$new(
      name = "Validation reference",
      country = "Test",
      year = 2020L,
      source = "Unit test",
      data = data.frame(
        age = age,
        gender = gender,
        population = population,
        stringsAsFactors = FALSE
      )
    )
  }

  expect_error(
    make_reference(c("018", "018"), population = c(10, 20)),
    "duplicate age/gender cells.*age=018, gender=Female"
  )
  expect_error(
    make_reference("018", population = Inf),
    "finite and non-negative.*population=Inf"
  )
  expect_error(
    make_reference("018", population = -1),
    "finite and non-negative.*population=-1"
  )
  expect_error(
    make_reference(c("018", "019"), population = c(0, 0)),
    "finite, positive total"
  )
  expect_error(
    make_reference(c("018", "019"), population = c(1e308, 1e308)),
    "finite, positive total"
  )
  expect_error(
    CohortPrevalence:::StandardizationReference$new(
      name = "Empty reference",
      country = "Test",
      year = 2020L,
      source = "Unit test",
      data = data.frame(
        age = character(),
        gender = character(),
        population = numeric()
      )
    ),
    "at least one population cell"
  )
})

test_that("single-year truncation thresholds must fall within reference support", {
  reference <- CohortPrevalence:::StandardizationReference$new(
    name = "Sparse single-year reference",
    country = "Test",
    year = 2020L,
    source = "Unit test",
    data = data.frame(
      age = c("018", "019"),
      gender = "Female",
      population = c(100, 100),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(reference$validateRightTruncation(18), 18)
  expect_equal(
    reference$getAdjustedReference(rightTruncation = 18)$age,
    "18+"
  )
  expect_error(reference$validateRightTruncation(20), "outside the single-year reference")
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
  expect_error(reference$validateRightTruncation(Inf), "finite, non-negative integer")
  expect_error(reference$validateRightTruncation(-1), "finite, non-negative integer")
  expect_error(reference$validateRightTruncation(5.5), "finite, non-negative integer")
  expect_error(reference$getAdjustedReference(rightTruncation = 7), "falls within group '5-9'")

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

test_that("built-in USA and Japan references map using their actual age labels", {
  reference_cases <- list(
    list(
      name = "usa_census_2020",
      ages = c(0, 18, 102),
      expected = c("000", "018", "102"),
      unsupported_age = 103
    ),
    list(
      name = "japan_census_2020",
      ages = c(1, 18, 101),
      expected = c("001", "018", "101"),
      unsupported_age = 0
    )
  )

  for (case in reference_cases) {
    stored_reference <- getStandardizationReference(case$name)
    reference_data <- stored_reference$getData() |>
      dplyr::select(age, gender, population)
    reference <- CohortPrevalence:::StandardizationReference$new(
      name = case$name,
      country = "Test",
      year = 2020L,
      source = "Built-in reference fixture",
      data = reference_data
    )

    expect_equal(
      reference$mapAgesToReference(case$ages),
      case$expected,
      info = case$name
    )
    expect_true(
      is.na(reference$mapAgesToReference(case$unsupported_age)),
      info = case$name
    )
  }
})

test_that("ACS-style mixed age bands map to their literal labels", {
  reference <- CohortPrevalence:::StandardizationReference$new(
    name = "ACS-style reference",
    country = "Test",
    year = 2020L,
    source = "Unit test",
    data = data.frame(
      age = c("0-4", "5-9", "10-14", "15-17", "18-19", "20", "21", "22-24", "85+"),
      gender = "Female",
      population = rep(100, 9),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    reference$mapAgesToReference(c(3, 8, 12, 16, 18, 20, 21, 23, 90)),
    c("0-4", "5-9", "10-14", "15-17", "18-19", "20", "21", "22-24", "85+")
  )
  expect_true(is.na(reference$mapAgesToReference(25)))
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

test_that("standardization rejects an empty prevalence result", {
  prevalence <- make_standardization_test_prevalence()[0, ]

  expect_error(
    CohortPrevalence:::standardize_prevalence(
      prevalenceData = prevalence,
      referencePopulation = make_standardization_test_reference()
    ),
    "No supported age/gender prevalence strata remain"
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
