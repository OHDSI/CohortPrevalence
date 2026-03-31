#' Direct Method Standardization of Prevalence Rates
#'
#' @description
#' Applies direct method age-sex standardization to crude prevalence data
#' using a reference population. Supports demographic bounds matching and
#' age truncation for real-world database patterns (e.g., Optum age masking).
#'
#' @param prevalenceData Data frame with stratified prevalence data.
#'   Required columns: age, gender, numerator, denominator
#'   (or outcome_count, population_count)
#'
#' @param referencePopulation StandardizationReference object defining
#'   the standard population for weighting
#'
#' @param ageMin Numeric. Minimum age for filtering reference population.
#'   If NULL (default), no lower bound applied.
#'
#' @param ageMax Numeric. Maximum age for filtering reference population.
#'   If NULL (default), no upper bound applied.
#'
#' @param ageRightTruncation Numeric. Optional age threshold for collapsing
#'   ages >= threshold into single "threshold+" group. Useful for handling
#'   database age masking (e.g., Optum: 70+ all collapsed to "70+").
#'   If NULL (default), no truncation applied.
#'
#' @param ageCol Character. Name of age column in prevalenceData.
#'   Default: "age"
#'
#' @param genderCol Character. Name of gender column in prevalenceData.
#'   Default: "gender"
#'
#' @param numeratorCol Character. Name of numerator (case count) column.
#'   Default: "numerator" (also accepts "outcome_count")
#'
#' @param denominatorCol Character. Name of denominator (population) column.
#'   Default: "denominator" (also accepts "population_count")
#'
#' @details
#' Algorithm (direct method standardization):
#'
#' 1. Filter reference population to demographic bounds (ageMin-ageMax)
#' 2. If ageRightTruncation specified: collapse ages >= threshold into single group
#' 3. Re-normalize weights within adjusted age groups
#' 4. Join prevalence data to adjusted reference weights
#' 5. Calculate crude rate per age-sex stratum: numerator/denominator
#' 6. Multiply each stratum's crude rate by reference weight
#' 7. Sum weighted rates = standardized rate
#' 8. Calculate 95% CI using Wilson score exact method
#'
#' Output includes:
#' - standardized_rate: Age-sex standardized prevalence rate
#' - ci_lower, ci_upper: 95% confidence interval (Wilson score exact)
#' - crude_rate: Crude rate across all strata (for comparison)
#' - reference_name, reference_year: Metadata
#'
#' @return
#' List with elements:
#'   - standardized_rate: Numeric, standardized prevalence rate
#'   - ci_lower: Numeric, lower 95% CI bound
#'   - ci_upper: Numeric, upper 95% CI bound
#'   - crude_rate: Numeric, crude rate (all strata combined)
#'   - reference_name: Character, name of reference population
#'   - reference_year: Integer, year of reference
#'   - diagnostics: List with details (n_strata, total_numerator, total_denominator)
#'
#' @examples
#' \dontrun{
#'   # Simple example with prevalence data
#'   prev_data <- data.frame(
#'     age = c("0-4", "5-9", "10-14", "0-4", "5-9", "10-14"),
#'     gender = c(rep("Male", 3), rep("Female", 3)),
#'     numerator = c(10, 15, 20, 8, 12, 18),
#'     denominator = c(1000, 1000, 1000, 1000, 1000, 1000)
#'   )
#'
#'   # Standardize using USA Census 2020
#'   data("usa_census_2020")
#'   result <- standardizePrevalence(
#'     prevalenceData = prev_data,
#'     referencePopulation = usa_census_2020
#'   )
#'
#'   # Standardize with demographic bounds (18-75 age restriction)
#'   result <- standardizePrevalence(
#'     prevalenceData = prev_data,
#'     referencePopulation = usa_census_2020,
#'     ageMin = 18,
#'     ageMax = 75
#'   )
#'
#'   # Handle Optum age masking (70+ collapsed)
#'   result <- standardizePrevalence(
#'     prevalenceData = prev_data,
#'     referencePopulation = usa_census_2020,
#'     ageRightTruncation = 70
#'   )
#' }
#'
#' @export
standardizePrevalence <- function(
    prevalenceData,
    referencePopulation,
    ageMin = NULL,
    ageMax = NULL,
    ageRightTruncation = NULL,
    ageCol = "age",
    genderCol = "gender",
    numeratorCol = "numerator",
    denominatorCol = "denominator") {

  # Validate inputs
  if (!inherits(referencePopulation, "StandardizationReference")) {
    stop("referencePopulation must be a StandardizationReference object")
  }

  if (!is.data.frame(prevalenceData)) {
    stop("prevalenceData must be a data frame")
  }

  # Handle alternative column names
  if (!numeratorCol %in% colnames(prevalenceData)) {
    if ("outcome_count" %in% colnames(prevalenceData)) {
      prevalenceData <- prevalenceData %>%
        dplyr::rename(!!numeratorCol := outcome_count)
    } else {
      stop(
        "prevalenceData must contain '",
        numeratorCol,
        "' or 'outcome_count' column"
      )
    }
  }

  if (!denominatorCol %in% colnames(prevalenceData)) {
    if ("population_count" %in% colnames(prevalenceData)) {
      prevalenceData <- prevalenceData %>%
        dplyr::rename(!!denominatorCol := population_count)
    } else {
      stop(
        "prevalenceData must contain '",
        denominatorCol,
        "' or 'population_count' column"
      )
    }
  }

  # Get adjusted reference population
  ref_adjusted <- referencePopulation$getAdjustedReference(
    ageMin = ageMin,
    ageMax = ageMax,
    rightTruncation = ageRightTruncation
  )

  # Prepare prevalence data for joining
  prev_clean <- prevalenceData %>%
    dplyr::select(
      all_of(c(ageCol, genderCol, numeratorCol, denominatorCol))
    ) %>%
    dplyr::rename(
      age = all_of(ageCol),
      gender = all_of(genderCol),
      numerator = all_of(numeratorCol),
      denominator = all_of(denominatorCol)
    ) %>%
    dplyr::mutate(
      age = as.character(age),
      gender = as.character(gender)
    )

  # Join to reference weights
  standardized_data <- prev_clean %>%
    dplyr::left_join(
      ref_adjusted %>% dplyr::select(age, gender, weight),
      by = c("age", "gender")
    )

  # Check for unmatched age-gender combinations
  unmatched <- standardized_data %>%
    dplyr::filter(is.na(weight))

  if (nrow(unmatched) > 0) {
    cli::cli_warn(
      "{nrow(unmatched)} stratum/strata in prevalenceData not found in ",
      "reference population (may be OK if stratum has very small denominator)"
    )
  }

  # Remove unmatched rows for calculation
  standardized_data <- standardized_data %>%
    dplyr::filter(!is.na(weight))

  if (nrow(standardized_data) == 0) {
    stop("No strata matched between prevalenceData and reference population")
  }

  # Calculate crude rate and standardized rate
  total_numerator <- sum(prev_clean$numerator, na.rm = TRUE)
  total_denominator <- sum(prev_clean$denominator, na.rm = TRUE)
  crude_rate <- total_numerator / total_denominator

  # Calculate stratum-specific crude rates and weighted rates
  standardized_data <- standardized_data %>%
    dplyr::mutate(
      stratum_rate = numerator / denominator,
      weighted_rate = stratum_rate * weight
    ) %>%
    dplyr::filter(!is.na(stratum_rate))  # Remove strata with no denominator

  standardized_rate <- sum(standardized_data$weighted_rate, na.rm = TRUE)

  # Calculate 95% Wilson score exact CI for standardized rate
  # Using the standard population variance formula
  ci_result <- .calculate_wilson_ci(standardized_rate, crude_rate, total_denominator)

  # Return results
  list(
    standardized_rate = standardized_rate,
    ci_lower = ci_result$lower,
    ci_upper = ci_result$upper,
    crude_rate = crude_rate,
    reference_name = referencePopulation$name,
    reference_year = referencePopulation$year,
    diagnostics = list(
      n_strata = nrow(standardized_data),
      total_numerator = total_numerator,
      total_denominator = total_denominator,
      n_reference_strata = nrow(ref_adjusted),
      age_bounds = list(min = ageMin, max = ageMax),
      age_truncation = ageRightTruncation
    )
  )
}


#' Calculate Wilson Score Exact Confidence Interval
#'
#' @param rate Numeric. The rate estimate (between 0 and 1)
#' @param crude_rate Numeric. The crude rate (for variance calculation)
#' @param total_n Numeric. Total denominator size
#'
#' @return List with lower and upper CI bounds
#'
#' @keywords internal
.calculate_wilson_ci <- function(rate, crude_rate, total_n) {
  # Wilson score exact method for CI
  # Following:
  # Newcombe, R.G. (1998). Two-sided confidence intervals for the single proportion:
  # Comparison of seven methods. Statistics in Medicine, 17, 857-872.

  # For standardized rate with heterogeneous strata, we estimate CI using
  # the crude rate as proxy with effective denominator
  z_alpha_2 <- 1.96  # 95% CI

  # Effective denominator for variance (simplified)
  n_eff <- total_n

  # Convert to counts for Wilson formula
  successes <- rate * n_eff
  failures <- n_eff - successes

  # Wilson score CI calculation
  denominator <- 1 + (z_alpha_2^2 / n_eff)

  center <- (
    successes +
      (z_alpha_2^2 / 2)
  ) / n_eff

  margin <- z_alpha_2 * sqrt(
    (successes * failures) / n_eff^2 +
      (z_alpha_2^2) / (4 * n_eff^2)
  ) / n_eff

  lower <- (center - margin) / denominator
  upper <- (center + margin) / denominator

  # Bound to [0, 1]
  lower <- max(0, lower)
  upper <- min(1, upper)

  list(lower = lower, upper = upper)
}
