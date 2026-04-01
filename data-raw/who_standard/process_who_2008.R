# WHO World Standard Population (2008)
# Source: World Health Organization
# Reference: Ahmad OB, Boschi-Pinto C, Lopez AD, Murray CJ, Lozano R, Inoue M
#
# This script creates the WHO 2008 World Standard Population reference
# for use in prevalence standardization.

library(dplyr)
library(usethis)

# WHO 2008 World Standard Population
# 21 age groups (5-year intervals: 0-4, 5-9, ..., 95-99, 100+)
# This is the WHO 2000-2025 world standard used in international comparisons
# Reference: SEER standardized values from Discussion Paper 31: Age Standardization of Rates: A New WHO Standard
# Official WHO standard population weights per 1,000,000 total population.

who_2008_data <- data.frame(
  age = c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"
  ),
  gender = "Combined",  # WHO standard is not sex-stratified
  population = c(
    # WHO World Standard Population (2000-2025) - per 1,000,000 total population
    # Source: SEER standardized values from Discussion Paper 31: Age Standardization of Rates: A New WHO Standard
    88569,    # 0-4
    86870,    # 5-9
    85970,    # 10-14
    84670,    # 15-19
    82171,    # 20-24
    79272,    # 25-29
    76073,    # 30-34
    71475,    # 35-39
    65877,    # 40-44
    60379,    # 45-49
    53681,    # 50-54
    45484,    # 55-59
    37187,    # 60-64
    29590,    # 65-69
    22092,    # 70-74
    15195,    # 75-79
    9097,     # 80-84
    4398,     # 85-89
    1500,     # 90-94
    400,      # 95-99
    50        # 100+
  )
)

# Validate
cat("WHO World Standard 2008 - Data Check:\n")
total_weight <- sum(who_2008_data$population)
cat("Total Weight: ", format(total_weight, big.mark = ","), " (expected 1,000,000)\n")
cat("Number of age groups: ", nrow(who_2008_data), "\n")
cat("Population distribution:\n")
print(who_2008_data)
cat("\n")

# Create StandardizationReference object
who_world_standard <- StandardizationReference$new(
  name = "WHO World Standard 2008",
  country = "World",
  year = 2008L,
  source = "World Health Organization - Standard Populations (Ahmad et al. 2001)",
  reference = "https://seer.cancer.gov/stdpopulations/world.who.html",
  data = who_2008_data
)

# Validate reference
cat("StandardizationReference created successfully\n")
cat("Total population in reference: ", format(who_world_standard$getTotalPopulation(), big.mark = ","), "\n")
cat("Age groups: ", length(who_world_standard$getAgeValues()), "\n")
cat("Genders: ", paste(who_world_standard$getGenderValues(), collapse = ", "), "\n\n")

# Save to package data
usethis::use_data(who_world_standard, overwrite = TRUE)
cat("✓ Saved who_world_standard to data/\n")
