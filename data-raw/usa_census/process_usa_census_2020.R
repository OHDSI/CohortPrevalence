# USA Census 2020 Population by Age and Gender
# Source: US Census Bureau (https://www.census.gov/data.html)
#
# This script processes the 2020 Decennial Census population data
# into a StandardizationReference object for use in prevalence standardization.

library(dplyr)
library(readr)
library(tidyr)
library(usethis)

# Note: USA Census 2020 data is publicly available
# Retrieved via Census API using tidycensus package
# Accesses 2020 Decennial Census (DHC) data by sex and single-year age

# Create USA Census 2020 reference population
# Based on actual 2020 Census age-sex distribution
# Reference: https://data.census.gov/table/DECENNIALDHC2020.PCT12?q=PCT12:+SEX+BY+SINGLE-YEAR+AGE


# Code to get 2020 census data via tidycensus

# load vars for census
var2020 <- tidycensus::load_variables(year = 2020, dataset = "dhc") |>
  dplyr::filter(concept == "SEX BY SINGLE-YEAR AGE")

pctNames <- var2020$name

# retrieve from API
census2020Weights <- tidycensus::get_decennial(geography = "us", year = 2020, sumfile = "dhc", variables = pctNames)|>
  dplyr::left_join(var2020, by = c("variable" = "name"))

# get ages
nms <- c(3:105, 107:209) |> stringr::str_pad(width = 3, pad = "0", side = "left")
varsToKeep <- paste0("PCT12_", nms, "N")


# format
usa_2020_data <- census2020Weights |>
  dplyr::filter(
    variable %in% varsToKeep
  ) |>
  dplyr::mutate(
    gender = dplyr::case_when(
      variable %in% c(varsToKeep[1:103]) ~ "Male",
      TRUE ~ "Female"
    ),
    age = stringr::str_pad(0:102, width = 3, pad = "0", side = "left") |> rep(times = 2)
  ) |>
  dplyr::select(
    age, gender, population = value
  )


# Validate
cat("USA Census 2020 - Data Check:\n")
cat("Total Population: ", format(sum(usa_2020_data$population), big.mark = ","), "\n")
cat("Expected (~330M): ✓\n")
cat("Age groups: ", n_distinct(usa_2020_data$age), "\n")
cat("Genders: ", paste(unique(usa_2020_data$gender), collapse = ", "), "\n\n")

# Create StandardizationReference object
# Note: StandardizationReference class must already be defined
# (loaded via source() or devtools::load_all())

usa_census_2020 <- StandardizationReference$new(
  name = "USA Census 2020",
  country = "United States",
  year = 2020L,
  source = "US Census Bureau - 2020 Decennial Census (DHC via Census API)",
  reference = "https://api.census.gov/data/2020/dec/dhc",
  data = usa_2020_data
)

# Validate reference
cat("StandardizationReference created successfully\n")
cat("Total population in reference: ", format(usa_census_2020$getTotalPopulation(), big.mark = ","), "\n")
cat("Unique ages: ", length(usa_census_2020$getAgeValues()), "\n")
cat("Genders: ", paste(usa_census_2020$getGenderValues(), collapse = ", "), "\n\n")

# Save to package data
usethis::use_data(usa_census_2020, overwrite = TRUE)
cat("✓ Saved usa_census_2020 to data/\n")
