#' Helper Functions for Standardization Reference Management
#'
#' Functions to create, import, and access standardization reference populations.

#' Create a Standardization Reference from User Data
#'
#' @param name Character. Name of the reference population
#' @param country Character. Country or region
#' @param year Integer. Reference year
#' @param source Character. Data source description or citation
#' @param data Data frame with columns: age, gender, population
#' @param reference Character. URL or reference to access the source data (optional)
#'
#' @return StandardizationReference object
#'
#' @details
#' The data frame should have one row per age-gender combination.
#' Population values should be absolute counts (not proportions). Standardization
#' calculates weights from the matched strata separately for each analysis.
#' Reference objects and their data-access methods store and return population
#' counts only; they do not calculate weights.
#'
#' @examples
#' \dontrun{
#'   my_data <- data.frame(
#'     age = c("0", "1-4", "5-9", "10-14", "0", "1-4", "5-9", "10-14"),
#'     gender = c(rep("Male", 4), rep("Female", 4)),
#'     population = c(100000, 400000, 420000, 410000, 95000, 380000, 400000, 405000)
#'   )
#'
#'   my_ref <- createStandardizationReference(
#'     name = "Custom Census",
#'     country = "Fictional Country",
#'     year = 2020,
#'     source = "My Data Collection",
#'     data = my_data
#'   )
#' }
#'
#' @export
createStandardizationReference <- function(name, country, year, source, data, reference = NULL) {
  StandardizationReference$new(
    name = name,
    country = country,
    year = year,
    source = source,
    data = data,
    reference = reference
  )
}


#' Import a Standardization Reference from CSV File
#'
#' @param filepath Character. Path to CSV file
#' @param name Character. Name for the reference
#' @param country Character. Country or region
#' @param year Integer. Reference year
#' @param source Character. Data source description
#' @param reference Character. URL or reference to access the source data (optional)
#' @param encoding Character. File encoding (default: "UTF-8")
#'
#' @return StandardizationReference object
#'
#' @details
#' CSV file should have columns: age, gender, population
#' Additional columns are ignored.
#'
#' @examples
#' \dontrun{
#'   my_ref <- importStandardizationReference(
#'     filepath = "data/my_population.csv",
#'     name = "Regional Census",
#'     country = "My Country",
#'     year = 2020,
#'     source = "Regional Statistics Bureau"
#'   )
#' }
#'
#' @export
importStandardizationReference <- function(
    filepath,
    name,
    country,
    year,
    source,
    reference = NULL,
    encoding = "UTF-8") {

  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  data <- readr::read_csv(filepath, locale = readr::locale(encoding = encoding))

  createStandardizationReference(
    name = name,
    country = country,
    year = year,
    source = source,
    data = data,
    reference = reference
  )
}


#' Get Built-in Standardization Reference
#'
#' @param name Character. Name of reference to retrieve.
#'   Valid options: "usa_census_2020", "japan_census_2020"
#'
#' @return StandardizationReference object
#'
#' @examples
#' \dontrun{
#'   usa_ref <- getStandardizationReference("usa_census_2020")
#'   japan_ref <- getStandardizationReference("japan_census_2020")
#' }
#'
#' @export
getStandardizationReference <- function(name) {
  valid_refs <- c("usa_census_2020", "japan_census_2020")

  if (!name %in% valid_refs) {
    stop(
      "Unknown reference '",
      name,
      "'. Valid options: ",
      paste(valid_refs, collapse = ", ")
    )
  }

  data(list = name, envir = environment())
  get(name, envir = environment())
}


#' List Available Standardization References
#'
#' @return Data frame with columns: name, country, year, status
#'
#' @details
#' Shows all built-in reference populations available in the package.
#' Status indicates whether reference is active or deprecated.
#'
#' @examples
#' \dontrun{
#'   listStandardizationReferences()
#' }
#'
#' @export
listStandardizationReferences <- function() {
  refs <- data.frame(
    name = c("usa_census_2020", "japan_census_2020"),
    country = c("United States", "Japan"),
    year = c(2020L, 2020L),
    status = c("Active", "Active"),
    stringsAsFactors = FALSE
  )

  refs
}


#' Fetch USA ACS Reference Population On-Demand
#'
#' @param years Integer or vector of integers. ACS year(s) (e.g., 2020, or 2021:2023)
#' @param survey Character. Survey type: "acs5" (default) or "acs1"
#' @param census_api_key Character. Census API key. If NULL (default), uses CENSUS_API_KEY 
#'   environment variable. Warns if neither provided.
#'
#' @return StandardizationReference object
#'
#' @details
#' Fetches American Community Survey (ACS) estimates from Census API.
#' Requires tidyCensus package and valid Census API key.
#'
#' Note: ACS provides age-grouped estimates (not single-year).
#' When multiple years are requested, data is combined into a single reference with year column.
#'
#' @examples
#' \dontrun{
#'   # Option 1: Single year with environment variable key
#'   # Sys.setenv(CENSUS_API_KEY = "your_key_here")
#'   acs_2020 <- fetchACSReference(2020)
#'
#'   # Option 2: Single year with direct key parameter
#'   acs_2020 <- fetchACSReference(2020, census_api_key = "your_key_here")
#'
#'   # Option 3: Multiple years (combined into one reference)
#'   acs_2021_2023 <- fetchACSReference(2021:2023, survey = "acs5")
#'
#'   # Use in standardization
#'   result <- standardizePrevalence(
#'     prevalenceData = my_data,
#'     referencePopulation = acs_2020
#'   )
#' }
#'
#' @export
fetchACSReference <- function(years, survey = "acs5", census_api_key = NULL) {
  # Ensure years is a vector
  years <- as.integer(years)
  
  # Check if tidyCensus is available
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop(
      "Package 'tidycensus' is required to fetch ACS data. ",
      "Install with: install.packages('tidycensus')"
    )
  }

  # Determine API key to use
  api_key <- if (is.null(census_api_key)) {
    Sys.getenv("CENSUS_API_KEY")
  } else {
    census_api_key
  }

  # Warn if no API key available
  if (api_key == "") {
    cli::cli_alert_warning(
      "No Census API key provided. Census API requests may be rate-limited. ",
      "Set with: Sys.setenv(CENSUS_API_KEY = 'your_key') or pass census_api_key parameter. ",
      "Get a free key at: https://api.census.gov/data/key_signup.html"
    )
  }

  # Fetch and combine data for all years
  all_data <- list()
  
  for (year in years) {
    cli::cli_alert_info("Fetching ACS {year} from Census API...")

    # Build get_acs call with conditional key parameter
    acs_call_args <- list(
      geography = "us",
      table = "B01001",
      year = year,
      survey = survey,
      cache_table = TRUE
    )
    
    # Only add key if we have one
    if (api_key != "") {
      acs_call_args$key <- api_key
    }

    # Fetch ACS population data (B01001: Sex by Age)
    acs_pop <- do.call(tidycensus::get_acs, acs_call_args)

    # Add year column for tracking
    acs_pop <- acs_pop |>
      dplyr::mutate(year = year)

    # Transform to tidy format
    acs_tidy <- map_acs_b01001_to_age_sex(acs_pop)
    all_data[[as.character(year)]] <- acs_tidy
  }

  # Combine all years into single dataframe
  acs_combined <- dplyr::bind_rows(all_data)

  # Create StandardizationReference with combined data
  ref_name <- if (length(years) == 1) {
    paste0("USA ACS ", survey, " ", years)
  } else {
    paste0("USA ACS ", survey, " ", min(years), "-", max(years))
  }

  acs_ref <- StandardizationReference$new(
    name = ref_name,
    country = "United States",
    year = as.integer(min(years)),
    source = "US Census Bureau American Community Survey",
    reference = "https://www.census.gov/programs-surveys/acs",
    data = acs_combined
  )

  return(acs_ref)
}


#' ACS B01001 Age Group Reference Table
#'
#' Single canonical table for the 23 ACS B01001 age-sex groups. Each consumer
#' (API variable mapping, single-year age range lookup, etc.) selects only the
#' columns it needs.
#'
#' Replaces the old \code{get_b01001_mapping()}. The 23 group labels appear
#' in exactly one place.
#'
#' @return Data frame with one row per age group and columns:
#'   \describe{
#'     \item{age_label}{Age group label (e.g., "0-4", "5-9", ..., "85+")}
#'     \item{min_age}{Minimum single-year age in the group (inclusive)}
#'     \item{max_age}{Maximum single-year age in the group (inclusive; \code{Inf} for open-ended)}
#'     \item{male_variable}{B01001 variable code for males (e.g., "B01001_003")}
#'     \item{female_variable}{B01001 variable code for females (e.g., "B01001_027")}
#'   }
acs_age_groups <- function() {
  data.frame(
    age_label = c("0-4", "5-9", "10-14", "15-17", "18-19",
                  "20", "21", "22-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59",
                  "60-61", "62-64", "65-66", "67-69", "70-74",
                  "75-79", "80-84", "85+"),
    min_age = c(0, 5, 10, 15, 18, 20, 21, 22, 25, 30,
                 35, 40, 45, 50, 55, 60, 62, 65, 67, 70,
                 75, 80, 85),
    max_age = c(4, 9, 14, 17, 19, 20, 21, 24, 29, 34,
                 39, 44, 49, 54, 59, 61, 64, 66, 69, 74,
                 79, 84, Inf),
    male_variable = paste0("B01001_", stringr::str_pad(3:25, width = 3, pad = "0")),
    female_variable = paste0("B01001_", stringr::str_pad(27:49, width = 3, pad = "0")),
    stringsAsFactors = FALSE
  )
}


#' Transform Raw ACS B01001 Data to Tidy Format
#'
#' Takes raw output from `tidycensus::get_acs()` (B01001 table)
#' and transforms it into tidy format with age, gender, population, and year columns.
#'
#' @param acs_raw_df Data frame with columns: GEOID, NAME, variable, estimate, moe, year
#'   - Typically output from tidycensus::get_acs(table = "B01001", ...)
#'   - Must include year and B01001_001 total rows
#'
#' @return Data frame with columns: age, gender, population, year (sorted by year, then gender, then age)
#'   - age: Age group label
#'   - gender: "Male" or "Female"
#'   - population: Population estimate (numeric)
#'   - year: Year of estimate
#'
#' @details
#' Filters to only B01001 age-gender variables (excludes B01001_001 total and subtotals).
#' Requires B01001_001 totals and validates complete 46-cell coverage and
#' reconciliation of age/sex estimates to the total for every year.
map_acs_b01001_to_age_sex <- function(acs_raw_df) {
  # Validate input
  if (!is.data.frame(acs_raw_df)) {
    cli::cli_abort("acs_raw_df must be a data frame")
  }

  required_cols <- c("variable", "estimate", "year")
  missing_cols <- setdiff(required_cols, colnames(acs_raw_df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "acs_raw_df is missing required columns:",
      stats::setNames(missing_cols, rep("x", length(missing_cols))),
      "i" = paste0("Required columns: ", paste(required_cols, collapse = ", "))
    ))
  }

  if (nrow(acs_raw_df) == 0) {
    cli::cli_abort("acs_raw_df must contain at least one row.")
  }

  if (anyNA(acs_raw_df$variable) || anyNA(acs_raw_df$year)) {
    cli::cli_abort("ACS variable and year fields cannot contain missing values.")
  }

  # Build variable-to-group lookup from canonical ACS table
  groups <- acs_age_groups()
  var_lookup <- data.frame(
    variable = c(groups$male_variable, groups$female_variable),
    age = rep(groups$age_label, 2),
    gender = rep(c("Male", "Female"), each = 23),
    stringsAsFactors = FALSE
  )

  years <- unique(acs_raw_df$year)
  total_pop <- acs_raw_df |>
    dplyr::filter(variable == "B01001_001") |>
    dplyr::mutate(
      total_population = suppressWarnings(as.numeric(as.character(estimate)))
    ) |>
    dplyr::select(year, total_population)

  total_counts <- total_pop |>
    dplyr::count(year, name = "total_rows")
  missing_total_years <- setdiff(years, total_counts$year)
  duplicate_total_years <- total_counts |>
    dplyr::filter(total_rows != 1)

  if (length(missing_total_years) > 0) {
    cli::cli_abort(c(
      "ACS data is missing B01001_001 total population for year(s):",
      stats::setNames(
        as.character(missing_total_years),
        rep("x", length(missing_total_years))
      )
    ))
  }

  if (nrow(duplicate_total_years) > 0) {
    duplicate_total_descriptions <- paste0(
      "year=", duplicate_total_years$year,
      ", rows=", duplicate_total_years$total_rows
    )
    cli::cli_abort(c(
      "ACS data must contain exactly one B01001_001 total per year:",
      stats::setNames(
        duplicate_total_descriptions,
        rep("x", length(duplicate_total_descriptions))
      )
    ))
  }

  invalid_totals <- !is.finite(total_pop$total_population) | total_pop$total_population <= 0

  if (any(invalid_totals)) {
    invalid_total_descriptions <- paste0(
      "year=", total_pop$year[invalid_totals],
      ", total=", total_pop$total_population[invalid_totals]
    )
    cli::cli_abort(c(
      "ACS total population must be finite and positive:",
      stats::setNames(
        invalid_total_descriptions,
        rep("x", length(invalid_total_descriptions))
      )
    ))
  }

  duplicate_variables <- acs_raw_df |>
    dplyr::filter(variable %in% var_lookup$variable) |>
    dplyr::count(year, variable, name = "variable_rows") |>
    dplyr::filter(variable_rows > 1)

  if (nrow(duplicate_variables) > 0) {
    duplicate_descriptions <- paste0(
      "year=", duplicate_variables$year,
      ", variable=", duplicate_variables$variable
    )
    cli::cli_abort(c(
      "ACS data contains duplicate age/sex variables:",
      stats::setNames(duplicate_descriptions, rep("x", length(duplicate_descriptions)))
    ))
  }

  # Transform: filter to mapped variables, join with mapping, select columns
  result <- acs_raw_df |>
    dplyr::filter(variable %in% var_lookup$variable) |>
    dplyr::left_join(var_lookup, by = "variable") |>
    dplyr::select(age, gender, population = estimate, year) |>
    dplyr::mutate(
      population = suppressWarnings(as.numeric(as.character(population)))
    ) |>
    dplyr::arrange(year, gender, age)

  # Validate exact age/sex coverage for each requested year.
  rows_per_year <- result |>
    dplyr::count(year, name = "age_sex_rows")
  coverage <- data.frame(year = years, stringsAsFactors = FALSE) |>
    dplyr::left_join(rows_per_year, by = "year") |>
    dplyr::mutate(age_sex_rows = dplyr::coalesce(age_sex_rows, 0L))
  invalid_coverage <- coverage$age_sex_rows != 46

  if (any(invalid_coverage)) {
    coverage_descriptions <- paste0(
      "year=", coverage$year[invalid_coverage],
      ", rows=", coverage$age_sex_rows[invalid_coverage]
    )
    cli::cli_abort(c(
      "Expected exactly 46 age/sex rows per year (23 age groups by 2 genders):",
      stats::setNames(coverage_descriptions, rep("x", length(coverage_descriptions)))
    ))
  }

  invalid_estimates <- !is.finite(result$population) | result$population < 0

  if (any(invalid_estimates)) {
    invalid_estimate_descriptions <- paste0(
      "year=", result$year[invalid_estimates],
      ", age=", result$age[invalid_estimates],
      ", gender=", result$gender[invalid_estimates],
      ", population=", result$population[invalid_estimates]
    )
    cli::cli_abort(c(
      "ACS age/sex population estimates must be finite and non-negative:",
      stats::setNames(
        invalid_estimate_descriptions,
        rep("x", length(invalid_estimate_descriptions))
      )
    ))
  }

  age_sex_totals <- result |>
    dplyr::group_by(year) |>
    dplyr::summarize(age_sex_total = sum(population), .groups = "drop") |>
    dplyr::left_join(total_pop, by = "year")
  # Allow half-person rounding error for the 46 cells plus the published total.
  rounding_tolerance <- 47 / 2
  inconsistent_totals <- abs(age_sex_totals$age_sex_total - age_sex_totals$total_population) >
    pmax(rounding_tolerance, abs(age_sex_totals$total_population) * 1e-8)

  if (any(inconsistent_totals)) {
    total_descriptions <- paste0(
      "year=", age_sex_totals$year[inconsistent_totals],
      ", age/sex total=", age_sex_totals$age_sex_total[inconsistent_totals],
      ", B01001_001=", age_sex_totals$total_population[inconsistent_totals]
    )
    cli::cli_abort(c(
      "ACS age/sex estimates do not reconcile to B01001_001:",
      stats::setNames(total_descriptions, rep("x", length(total_descriptions)))
    ))
  }

  return(result)
}

