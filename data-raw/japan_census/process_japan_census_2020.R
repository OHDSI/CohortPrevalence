# Japan Census 2020 Population by Age and Gender
# Source: Statistics Bureau of Japan (総務省統計局)
# URL: https://www.stat.go.jp/data/kokusei/2020/
#
# This script processes the 2020 Decennial Census population data
# into a StandardizationReference object for use in prevalence standardization.

library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(usethis)

# Note: Japan Census 2020 provides single-year ages and official totals
# Reference: https://www.e-stat.go.jp/en/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136466&tclass2val=0
# Table 2-5: Population by Sex, Age (single years) and All nationality or Japanese - Japan

# Specify path to downloaded Excel file
# Download Table 2-5 from e-Stat:
# https://www.e-stat.go.jp/en/stat-search/file-download?statInfId=000032142408&fileKind=0
# And place in data-raw/japan_census/ directory

# Try to find Excel file (b02_05.xlsx from e-Stat)
data_dir <- here::here("data-raw", "japan_census")
excel_file <- file.path(data_dir, "b02_05e.xlsx")


# Now read with proper skip to get headers and data
raw_data <- suppressMessages(
  readxl::read_excel(
    excel_file,
    sheet = 1,
    skip = 9  # Adjust if needed based on inspection above
  )
)

# Process data:

age_cols <- 10:111 # from total to last available age
sex_col <- 2
nationality_col <- 1
area_col <- 9

# Extract Japan total (first row) with age and population by gender
japan_2020_data <- raw_data |>
  slice(-c(1:2)) |>  # First row (Japan total)
  select(all_of(c(nationality_col,sex_col, area_col, age_cols))) |>
  rename(
    nationality = `...1`,
    gender = `...2`,
    area = `...9`
  ) |>
  dplyr::filter(
    nationality == "0_Total", # get all nationalities in japan
    area == "0001_Japan", # results for all of japan,
    gender != "0_Total" # remove combined gender
  ) |>
  # Clean sex column: extract Male/Female from encoded values
  mutate(
    gender = case_when(
      grepl("1_Male", gender, ignore.case = TRUE) ~ "Male",
      grepl("2_Female", gender, ignore.case = TRUE) ~ "Female",
      TRUE ~ gender  # Keep as-is if doesn't match (for inspection)
    )
  ) |>
  # Pivot to long format
  pivot_longer(
    cols = contains("years"),
    names_to = "age",
    values_to = "population"
  ) |>
  # Extract single-year age from column names (e.g., "1_years" -> "1", "100_years" -> "100")
  mutate(
    age = gsub("_.*", "", age),
    population = as.numeric(population)
  ) |>
  select(age, gender, population)



# Validate data structure
cat("===== VALIDATION =====\n")
cat("Dimensions: ", nrow(japan_2020_data), " rows x ", ncol(japan_2020_data), " columns\n")
cat("Expected: 202 rows (101 ages × 2 genders)\n\n")

if (nrow(japan_2020_data) == 0) {
  stop("ERROR: No data extracted. Please verify:\n",
       "1. Column indices are correct (currently age_col=", age_col,
       ", male_col=", male_col, ", female_col=", female_col, ")\n",
       "2. Try adjusting these column numbers based on the inspection output above\n")
}

cat("Age range: ", min(japan_2020_data$age), " to ", max(japan_2020_data$age), "\n")
cat("Genders: ", paste(unique(japan_2020_data$gender), collapse = ", "), "\n")
cat("Population values (summary):\n")
print(summary(japan_2020_data$population))
cat("\n")

# Data quality check
total_pop <- sum(japan_2020_data$population, na.rm = TRUE)
cat("Total Population: ", format(total_pop, big.mark = ","), "\n")
cat("Expected (~123M): ", if_else(total_pop > 120000000 & total_pop < 130000000, "✓", "⚠ Check data!"), "\n")
cat("Age groups: ", n_distinct(japan_2020_data$age), " (expected: 101)\n")
cat("\nSample data (first 10 rows):\n")
print(head(japan_2020_data, 10))

# Create StandardizationReference object
japan_census_2020 <- StandardizationReference$new(
  name = "Japan Census 2020",
  country = "Japan",
  year = 2020L,
  source = "Statistics Bureau of Japan (総務省統計局) - 2020 Census, Table 2-5",
  reference = "https://www.stat.go.jp/data/kokusei/2020/",
  data = japan_2020_data
)

# Validate reference
cat("StandardizationReference created successfully\n")
cat("Total population in reference: ", format(japan_census_2020$getTotalPopulation(), big.mark = ","), "\n")
cat("Unique ages: ", length(japan_census_2020$getAgeValues()), "\n")
cat("Genders: ", paste(japan_census_2020$getGenderValues(), collapse = ", "), "\n")
cat("Sample data (first 10 rows):\n")
print(head(japan_census_2020$getData(), 10))
cat("\n")

# Save to package data
usethis::use_data(japan_census_2020, overwrite = TRUE)
cat("✓ Saved japan_census_2020 to data/\n")

