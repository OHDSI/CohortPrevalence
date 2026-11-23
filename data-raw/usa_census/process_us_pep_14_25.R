# U.S. PEP Population Reference, 2014-2025
#
# Retrieves the 2014-2019 single-year age estimates from the PEP 2019 API and
# the 2020-2025 estimates through tidycensus. State estimates are aggregated to
# U.S.-wide age-sex totals before creating the package object.

library(dplyr)
library(usethis)

devtools::load_all()


historical_years <- 2014:2019
recent_years <- 2020:2025
recent_vintage <- 2025L
projection_years <- c(historical_years, recent_years)
expected_cell_count <- 172L

# Retrieve 2014-2019 PEP single-year age estimates from the 2019 API vintage.
census_api_key <- Sys.getenv("CENSUS_API_KEY")

if (!nzchar(census_api_key)) {
  stop(
    "Set CENSUS_API_KEY before retrieving historical PEP data.",
    call. = FALSE
  )
}

historical_api_url <- paste0(
  "https://api.census.gov/data/2019/pep/charage",
  "?get=DATE_CODE,AGE,SEX,POP",
  "&for=us:1&RACE=0&HISP=0",
  "&key=", utils::URLencode(census_api_key, reserved = TRUE)
)

historical_response <- tryCatch(
  jsonlite::fromJSON(historical_api_url),
  error = function(error) {
    stop(
      "Could not retrieve 2014-2019 PEP data. Check network access and CENSUS_API_KEY.",
      call. = FALSE
    )
  }
)

if (!is.matrix(historical_response) || nrow(historical_response) < 2) {
  stop("The 2014-2019 PEP API returned no population rows.", call. = FALSE)
}

historical_pep <- as.data.frame(
  historical_response[-1, , drop = FALSE],
  stringsAsFactors = FALSE
)
names(historical_pep) <- as.character(historical_response[1, ])
historical_pep$DATE_CODE <- as.integer(historical_pep$DATE_CODE)
historical_pep$AGE <- as.integer(historical_pep$AGE)
historical_pep$SEX <- as.integer(historical_pep$SEX)
historical_pep$POP <- as.numeric(historical_pep$POP)

historical_pep <- historical_pep |>
  dplyr::filter(
    DATE_CODE %in% (historical_years - 2007L),
    AGE <= 100L,
    SEX %in% c(1L, 2L)
  )

historical_pep$year <- historical_pep$DATE_CODE + 2007L
historical_pep$age <- rep("85+", nrow(historical_pep))
under_85 <- historical_pep$AGE < 85L
historical_pep$age[under_85] <- sprintf(
  "%03d",
  historical_pep$AGE[under_85]
)
historical_pep$gender <- rep(NA_character_, nrow(historical_pep))
historical_pep$gender[historical_pep$SEX == 1L] <- "Male"
historical_pep$gender[historical_pep$SEX == 2L] <- "Female"

historical_population <- historical_pep |>
  dplyr::transmute(
    year,
    age,
    gender,
    population = POP
  ) |>
  dplyr::group_by(year, age, gender) |>
  dplyr::summarise(population = sum(population), .groups = "drop")

historical_year_counts <- historical_population |>
  dplyr::count(year, name = "cell_count")

if (!identical(historical_year_counts$year, historical_years) ||
    any(historical_year_counts$cell_count != expected_cell_count)) {
  stop(
    "The 2014-2019 PEP data do not contain the expected age-sex strata.",
    call. = FALSE
  )
}

# Retrieve 2020-2025 PEP characteristics through tidycensus and aggregate states.
recent_pep_by_year <- purrr::map(
  recent_years,
  function(year_value) {
    tryCatch(
      tidycensus::get_estimates(
        geography = "state",
        product = "characteristics",
        breakdown = c("SEX", "AGE"),
        vintage = recent_vintage,
        year = year_value,
        breakdown_labels = TRUE
      ),
      error = function(error) {
        stop(
          "Could not retrieve PEP year ", year_value,
          " through tidycensus. Use a tidycensus version that supports vintage ",
          recent_vintage, ". Original error: ", conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
)

recent_pep <- dplyr::bind_rows(recent_pep_by_year)
state_count <- dplyr::n_distinct(recent_pep$GEOID)

if (state_count != 51L) {
  stop(
    "Expected tidycensus PEP estimates for 50 states and the District of Columbia; found ",
    state_count,
    " geographies.",
    call. = FALSE
  )
}

recent_age_labels <- trimws(as.character(recent_pep$AGE))
recent_age_labels <- sub("^Age\\s+", "", recent_age_labels, ignore.case = TRUE)
recent_age_values <- suppressWarnings(
  as.integer(sub("^([0-9]+).*$", "\\1", recent_age_labels))
)
recent_sex_labels <- tolower(trimws(as.character(recent_pep$SEX)))
keep_recent_rows <- !is.na(recent_age_values) &
  recent_age_values <= 85L &
  recent_sex_labels %in% c("1", "male", "2", "female")
recent_pep <- recent_pep[keep_recent_rows, , drop = FALSE]
recent_age_values <- recent_age_values[keep_recent_rows]
recent_sex_labels <- recent_sex_labels[keep_recent_rows]

recent_age <- rep("85+", length(recent_age_values))
under_85 <- recent_age_values < 85L
recent_age[under_85] <- sprintf("%03d", recent_age_values[under_85])
recent_gender <- rep(NA_character_, length(recent_sex_labels))
recent_gender[recent_sex_labels %in% c("1", "male")] <- "Male"
recent_gender[recent_sex_labels %in% c("2", "female")] <- "Female"

recent_population <- data.frame(
  year = as.integer(recent_pep$year),
  age = recent_age,
  gender = recent_gender,
  population = as.numeric(recent_pep$value),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(year, age, gender) |>
  dplyr::summarise(population = sum(population), .groups = "drop")

recent_year_counts <- recent_population |>
  dplyr::count(year, name = "cell_count")

if (!identical(recent_year_counts$year, recent_years) ||
    any(recent_year_counts$cell_count != expected_cell_count)) {
  stop(
    "The 2020-2025 PEP data do not contain the expected age-sex strata.",
    call. = FALSE
  )
}

# Combine both vintages into the common age-sex schema used by the class.
usa_projection_data <- dplyr::bind_rows(
  historical_population,
  recent_population
) |>
  dplyr::arrange(year, gender, age)

if (!identical(sort(unique(usa_projection_data$year)), projection_years)) {
  stop("PEP data must cover every year from 2014 through 2025.", call. = FALSE)
}

usa_projection_2014_2025 <- ProjectionReference$new(
  name = "U.S. PEP Population Estimates 2014-2025",
  country = "United States",
  source = paste(
    "U.S. Census Bureau Population Estimates Program:",
    "2019 and", recent_vintage, "vintages"
  ),
  data = usa_projection_data,
  reference = "https://www.census.gov/programs-surveys/popest.html",
  provenance = list(
    vintage_by_year = c(
      stats::setNames(rep(2019L, length(historical_years)), historical_years),
      stats::setNames(rep(recent_vintage, length(recent_years)), recent_years)
    ),
    source_urls = c(
      historical = "https://api.census.gov/data/2019/pep/charage",
      recent = "https://walker-data.com/tidycensus/reference/get_estimates.html"
    ),
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
)

annual_totals <- usa_projection_2014_2025$getData() |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total_population = sum(population),
    age_sex_cells = dplyr::n(),
    .groups = "drop"
  )

print(annual_totals)

usethis::use_data(usa_projection_2014_2025, overwrite = TRUE)
cat("Saved usa_projection_2014_2025 to data/.\n")
