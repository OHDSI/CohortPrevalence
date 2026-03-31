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
#' @param doi Character. DOI or URL (optional)
#'
#' @return StandardizationReference object
#'
#' @details
#' The data frame should have one row per age-gender combination.
#' Population values should be absolute counts (not proportions).
#' Weights are automatically calculated as population / total_population.
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
createStandardizationReference <- function(name, country, year, source, data, doi = NULL) {
  StandardizationReference$new(
    name = name,
    country = country,
    year = year,
    source = source,
    data = data,
    doi = doi
  )
}


#' Import a Standardization Reference from CSV File
#'
#' @param filepath Character. Path to CSV file
#' @param name Character. Name for the reference
#' @param country Character. Country or region
#' @param year Integer. Reference year
#' @param source Character. Data source description
#' @param doi Character. DOI or URL (optional)
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
    doi = NULL,
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
    doi = doi
  )
}


#' Get Built-in Standardization Reference
#'
#' @param name Character. Name of reference to retrieve.
#'   Valid options: "usa_census_2020", "japan_census_2020", "who_world_standard"
#'
#' @return StandardizationReference object
#'
#' @examples
#' \dontrun{
#'   usa_ref <- getStandardizationReference("usa_census_2020")
#'   japan_ref <- getStandardizationReference("japan_census_2020")
#'   who_ref <- getStandardizationReference("who_world_standard")
#' }
#'
#' @export
getStandardizationReference <- function(name) {
  valid_refs <- c("usa_census_2020", "japan_census_2020", "who_world_standard")

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
    name = c("usa_census_2020", "japan_census_2020", "who_world_standard"),
    country = c("United States", "Japan", "World"),
    year = c(2020L, 2020L, 2008L),
    status = c("Active", "Active", "Active"),
    stringsAsFactors = FALSE
  )

  refs
}


#' Fetch USA ACS Reference Population On-Demand
#'
#' @param year Integer. ACS year (e.g., 2020, 2021, 2022)
#' @param cache Logical. Cache locally after download? (default: TRUE)
#'
#' @return StandardizationReference object
#'
#' @details
#' Fetches American Community Survey (ACS) estimates from tidyCensus API.
#' Requires tidyCensus package and valid Census API key (via Sys.setenv).
#'
#' Note: ACS provides 5-year estimates for most age groups (depends on year).
#' First call downloads; subsequent calls use cached version if cache=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Set up Census API key (one-time)
#'   # Sys.setenv(CENSUS_API_KEY = "your_key_here")
#'
#'   # Fetch 2020 ACS
#'   acs_2020 <- fetchACSReference(2020)
#'
#'   # Use in standardization
#'   result <- standardizePrevalence(
#'     prevalenceData = my_data,
#'     referencePopulation = acs_2020
#'   )
#' }
#'
#' @export
fetchACSReference <- function(year = 2020, cache = TRUE) {
  # Check if tidyCensus is available
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop(
      "Package 'tidycensus' is required to fetch ACS data. ",
      "Install with: install.packages('tidycensus')"
    )
  }

  # Check for Census API key
  api_key <- Sys.getenv("CENSUS_API_KEY")
  if (api_key == "") {
    stop(
      "Census API key not found. Set with: ",
      "Sys.setenv(CENSUS_API_KEY = 'your_key')\n",
      "Get a free key at: https://api.census.gov/data/key_signup.html"
    )
  }

  # Cache location
  cache_dir <- file.path(rappdirs::user_cache_dir("CohortPrevalence"), "acs")
  cache_file <- file.path(cache_dir, paste0("usa_acs_", year, ".rds"))

  # Check if cached
  if (cache && file.exists(cache_file)) {
    cli::cli_alert_info("Loading cached ACS {year} from {cache_file}")
    return(readRDS(cache_file))
  }

  cli::cli_alert_info("Fetching ACS {year} from Census API...")

  # Fetch ACS population data
  # Variables: B01001_001 = Total population, B01001_026 = Total female
  # Split by age groups (depends on ACS table structure)
  acs_pop <- tidycensus::get_acs(
    geography = "us",
    table = "B01001",
    year = year,
    survey = "acs5",
    cache_table = TRUE
  )

  # Process into reference format
  # This is simplified; actual implementation would need to
  # - Map B01001_* variables to age-gender groups
  # - Handle 5-year age band groupings
  # - Aggregate to national totals

  acs_processed <- acs_pop %>%
    dplyr::select(variable, estimate) %>%
    dplyr::rename(population = estimate)
  # ... actual processing code here ...

  # Create StandardizationReference
  acs_ref <- StandardizationReference$new(
    name = paste0("USA ACS ", year),
    country = "United States",
    year = as.integer(year),
    source = "US Census Bureau American Community Survey",
    doi = "https://www.census.gov/programs-surveys/acs",
    data = acs_processed  # placeholder
  )

  # Cache if requested
  if (cache) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    saveRDS(acs_ref, cache_file)
    cli::cli_alert_success("Cached to {cache_file}")
  }

  acs_ref
}
