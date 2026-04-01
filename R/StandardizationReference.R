#' StandardizationReference R6 Class
#'
#' @description
#' Encapsulates a reference population for direct method standardization.
#' Stores population weights by age and gender with metadata.
#'
#' @details
#' The StandardizationReference class provides a structured way to manage
#' reference populations used for age-sex standardization. It validates
#' data structure, auto-calculates weights as proportions, and provides
#' methods for accessing and manipulating reference data.
#'
#' @examples
#' \dontrun{
#'   # Create a custom reference
#'   ref_data <- data.frame(
#'     age = c("0", "1", "2", "100+", "0", "1", "2", "100+"),
#'     gender = c(rep("Male", 4), rep("Female", 4)),
#'     population = c(100000, 95000, 92000, 50000, 98000, 93000, 90000, 48000)
#'   )
#'
#'   my_ref <- StandardizationReference$new(
#'     name = "Custom Reference",
#'     country = "Custom",
#'     year = 2020L,
#'     source = "My Data",
#'     data = ref_data
#'   )
#'
#'   # View reference
#'   my_ref$viewReference()
#'
#'   # Get total population
#'   my_ref$getTotalPopulation()
#'
#'   # Filter to age range
#'   filtered <- my_ref$getFilteredReference(ageMin = 18, ageMax = 65)
#' }
#'
#' @export
StandardizationReference <- R6::R6Class(
  "StandardizationReference",
  public = list(
    #' @description Create a new StandardizationReference
    #'
    #' @param name Character. Name of reference (e.g., "USA Census 2020")
    #' @param country Character. Country/region name
    #' @param year Integer. Reference year
    #' @param source Character. Data source description
    #' @param reference Character. URL or reference to access the source data
    #' @param data Data frame with columns: age, gender, population
    #'
    #' @return A new StandardizationReference object
    initialize = function(name, country, year, source, data, reference = NULL) {
      # Validate inputs
      if (!is.data.frame(data)) {
        stop("'data' must be a data frame")
      }

      required_cols <- c("age", "gender", "population")
      missing_cols <- setdiff(required_cols, colnames(data))
      if (length(missing_cols) > 0) {
        stop(
          "Data frame must have columns: ",
          paste(required_cols, collapse = ", "),
          ". Missing: ",
          paste(missing_cols, collapse = ", ")
        )
      }

      # Check for missing values in required columns
      if (any(is.na(data$age)) || any(is.na(data$gender)) || any(is.na(data$population))) {
        stop("Data cannot contain NA values in age, gender, or population columns")
      }

      # Check population is numeric and positive
      if (!is.numeric(data$population) || any(data$population < 0)) {
        stop("Population column must be numeric and non-negative")
      }

      # Store metadata in private fields
      private$.name <- name
      private$.country <- country
      private$.year <- as.integer(year)
      private$.source <- source
      private$.reference <- reference

      # Calculate weights and store data
      private$.data <- data |>
        dplyr::mutate(
          age = as.character(age),
          gender = as.character(gender),
          population = as.numeric(population)
        ) |>
        dplyr::mutate(
          weight = population / sum(population)
        ) |>
        dplyr::arrange(gender, age)
    },

    #' @description View the reference population data
    viewReference = function() {
      cat("\n=== ", private$.name, " ===\n", sep = "")
      cat("Country:  ", private$.country, "\n", sep = "")
      cat("Year:     ", private$.year, "\n", sep = "")
      cat("Source:   ", private$.source, "\n", sep = "")
      if (!is.null(private$.reference)) {
        cat("Reference:", private$.reference, "\n", sep = " ")
      }
      cat("\nData (first 10 rows):\n")
      print(head(private$.data, 10))
      cat(
        "\nTotal rows: ", nrow(private$.data),
        " | Total population: ",
        format(sum(private$.data$population), big.mark = ","),
        "\n\n",
        sep = ""
      )
    },

    #' @description Get the full reference data frame
    #' @return Data frame with columns: age, gender, population, weight
    getData = function() {
      private$.data
    },

    #' @description Get total population
    #' @return Numeric value
    getTotalPopulation = function() {
      sum(private$.data$population)
    },

    #' @description Get unique ages
    #' @return Character vector of age values
    getAgeValues = function() {
      sort(unique(private$.data$age))
    },

    #' @description Get unique genders
    #' @return Character vector of gender values
    getGenderValues = function() {
      sort(unique(private$.data$gender))
    },

    #' @description Get metadata as list
    #' @return List with name, country, year, source, reference
    getMetadata = function() {
      list(
        name = private$.name,
        country = private$.country,
        year = private$.year,
        source = private$.source,
        reference = private$.reference
      )
    },

    #' @description Filter reference to demographic bounds and re-normalize weights
    #'
    #' @param ageMin Minimum age (inclusive)
    #' @param ageMax Maximum age (inclusive)
    #'
    #' @return Data frame filtered and re-normalized to bounds
    getFilteredReference = function(ageMin = NULL, ageMax = NULL) {
      result <- private$.data

      # Convert ages to numeric for comparison
      if (!is.null(ageMin) || !is.null(ageMax)) {
        ages_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", result$age)))

        if (!is.null(ageMin)) {
          result <- result[ages_numeric >= ageMin | is.na(ages_numeric), ]
        }

        if (!is.null(ageMax)) {
          result <- result[ages_numeric <= ageMax | is.na(ages_numeric), ]
        }
      }

      # Re-normalize weights to filtered subset
      result <- result |>
        dplyr::mutate(
          weight = population / sum(population)
        )

      result
    },

    #' @description Apply age truncation (collapse ages >= threshold into single group)
    #'
    #' @param rightTruncation Age threshold. Ages >= this value collapse to "threshold+"
    #'
    #' @return Data frame with ages >= threshold collapsed and weights re-normalized
    getAgetruncatedReference = function(rightTruncation) {
      result <- private$.data |>
        dplyr::mutate(
          age_numeric = suppressWarnings(as.numeric(gsub("\\+", "", age)))
        ) |>
        dplyr::mutate(
          age = dplyr::case_when(
            is.na(age_numeric) ~ age,  # Keep "100+" as is if not convertible
            age_numeric >= rightTruncation ~ paste0(rightTruncation, "+"),
            TRUE ~ age
          )
        ) |>
        dplyr::select(-age_numeric) |>
        dplyr::group_by(age, gender) |>
        dplyr::summarise(
          population = sum(population),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          weight = population / sum(population)
        ) |>
        dplyr::arrange(gender, age)

      result
    },

    #' @description Apply both bounds filtering and age truncation
    #'
    #' @param ageMin Minimum age (inclusive)
    #' @param ageMax Maximum age (inclusive)
    #' @param rightTruncation Age threshold for truncation (optional)
    #'
    #' @return Data frame with both transformations applied
    getAdjustedReference = function(ageMin = NULL, ageMax = NULL, rightTruncation = NULL) {
      result <- private$.data

      # First apply age truncation if specified
      if (!is.null(rightTruncation)) {
        result <- result |>
          dplyr::mutate(
            age_numeric = suppressWarnings(as.numeric(gsub("\\+", "", age)))
          ) |>
          dplyr::mutate(
            age = dplyr::case_when(
              is.na(age_numeric) ~ age,
              age_numeric >= rightTruncation ~ paste0(rightTruncation, "+"),
              TRUE ~ age
            )
          ) |>
          dplyr::select(-age_numeric) |>
          dplyr::group_by(age, gender) |>
          dplyr::summarise(
            population = sum(population),
            .groups = "drop"
          )
      }

      # Then apply demographic bounds
      if (!is.null(ageMin) || !is.null(ageMax)) {
        ages_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", result$age)))

        if (!is.null(ageMin)) {
          result <- result[ages_numeric >= ageMin | is.na(ages_numeric), ]
        }

        if (!is.null(ageMax)) {
          result <- result[ages_numeric <= ageMax | is.na(ages_numeric), ]
        }
      }

      # Re-normalize weights
      result <- result |>
        dplyr::mutate(
          weight = population / sum(population)
        )

      result
    }
  ),

  private = list(
    .data = NULL,
    .name = NULL,
    .country = NULL,
    .year = NULL,
    .source = NULL,
    .reference = NULL
  ),

  active = list(
    #' @field name Name of the reference population (active)
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      }
      private$.name <- value
    },

    #' @field country Country or region (active)
    country = function(value) {
      if (missing(value)) {
        return(private$.country)
      }
      private$.country <- value
    },

    #' @field year Year of reference (numeric) (active)
    year = function(value) {
      if (missing(value)) {
        return(private$.year)
      }
      private$.year <- as.integer(value)
    },

    #' @field source Source/citation (active)
    source = function(value) {
      if (missing(value)) {
        return(private$.source)
      }
      private$.source <- value
    },

    #' @field reference URL or reference to access source data (active)
    reference = function(value) {
      if (missing(value)) {
        return(private$.reference)
      }
      private$.reference <- value
    }
  )
)
