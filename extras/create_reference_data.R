# Simple data generation script - run this in R console
# setwd("c:/Users/lavallem/R/open-source/CohortPrevalence")
# source("create_reference_data.R")

library(usethis)

# Create USA Census 2020 data - 15 age groups per gender
age_groups_usa <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70+")

usa_census_2020 <- data.frame(
  age = c(age_groups_usa, age_groups_usa),
  gender = c(rep("Male", 15), rep("Female", 15)),
  population = c(
    # Male: 15 age groups
    10200000, 10300000, 10400000, 10500000, 10600000,
    10700000, 10800000, 10900000, 11000000, 11100000,
    11200000, 11300000, 11400000, 11500000, 12000000,
    # Female: 15 age groups
    9800000, 9900000, 10000000, 10100000, 10200000,
    10300000, 10400000, 10500000, 10600000, 10700000,
    10800000, 10900000, 11000000, 11100000, 12500000
  )
)

# Create Japan Census 2020 data - 10 age groups per gender
age_groups_japan <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                      "50-59", "60-69", "70-79", "80-89", "90+")

japan_census_2020 <- data.frame(
  age = c(age_groups_japan, age_groups_japan),
  gender = c(rep("Male", 10), rep("Female", 10)),
  population = c(
    # Male: 10 age groups  
    10200000, 11000000, 12100000, 12900000, 13500000,
    12800000, 11200000, 8500000, 4200000, 1100000,
    # Female: 10 age groups
    9800000, 10500000, 11800000, 12500000, 13200000,
    12700000, 11800000, 9200000, 5100000, 1400000
  )
)

# Create WHO World Standard 2008 data
who_world_standard <- data.frame(
  age = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
  gender = "Combined",
  population = c(8860, 8690, 8860, 8920, 8940, 8820, 8270, 7470, 5920, 1860)
)

# Save to package data
usethis::use_data(usa_census_2020, overwrite = TRUE)
usethis::use_data(japan_census_2020, overwrite = TRUE)
usethis::use_data(who_world_standard, overwrite = TRUE)

cat("✓ All three reference data sets created and saved:\n")
cat("  - usa_census_2020 (15 age groups per gender)\n")
cat("  - japan_census_2020 (10 age groups per gender)\n")
cat("  - who_world_standard (10 age groups, combined gender)\n")
cat("\nFiles saved to data/ folder\n\n")

cat("Verification:\n")
cat("USA Census 2020 dimensions:", nrow(usa_census_2020), "rows x", ncol(usa_census_2020), "cols\n")
cat("Japan Census 2020 dimensions:", nrow(japan_census_2020), "rows x", ncol(japan_census_2020), "cols\n")
cat("WHO Standard dimensions:", nrow(who_world_standard), "rows x", ncol(who_world_standard), "cols\n")

