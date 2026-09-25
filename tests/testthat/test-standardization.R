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
