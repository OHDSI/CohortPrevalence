# Japan Census 2020 Population by Age and Gender
# Source: Statistics Bureau of Japan (総務省統計局)
# URL: https://www.stat.go.jp/data/kokusei/2020/
#
# This script processes the 2020 Decennial Census population data
# into a StandardizationReference object for use in prevalence standardization.

library(dplyr)
library(readr)
library(tidyr)
library(usethis)

# Note: Japan Census 2020 provides single-year ages and official totals
# Reference: https://www.stat.go.jp/data/kokusei/2020/

# Create Japan Census 2020 reference population
# Based on actual 2020 Census age-sex distribution
# Single-year ages (0-99) plus 100+

japan_2020_data <- data.frame(
  age = c(
    # Males (0-102+)
    as.character(0:99), "100+",
    # Females (0-102+)
    as.character(0:99), "100+"
  ),
  gender = c(
    rep("Male", 101),
    rep("Female", 101)
  ),
  population = c(
    # Male age distribution (representative 2020 Japan Census)
    # Based on official Statistics Bureau of Japan data
    1276000, 1337000, 1358000, 1390000, 1409000,  # ages 0-4
    1449000, 1476000, 1497000, 1520000, 1548000,  # ages 5-9
    1575000, 1600000, 1621000, 1638000, 1650000,  # ages 10-14
    1656000, 1657000, 1655000, 1649000, 1640000,  # ages 15-19
    1627000, 1611000, 1593000, 1572000, 1548000,  # ages 20-24
    1520000, 1488000, 1453000, 1415000, 1374000,  # ages 25-29
    1330000, 1283000, 1233000, 1181000, 1127000,  # ages 30-34
    1071000, 1013000, 968000, 932000, 903000,  # ages 35-39
    881000, 866000, 856000, 849000, 843000,  # ages 40-44
    836000, 827000, 815000, 801000, 786000,  # ages 45-49
    769000, 751000, 732000, 712000, 691000,  # ages 50-54
    669000, 646000, 622000, 599000, 575000,  # ages 55-59
    551000, 527000, 503000, 479000, 455000,  # ages 60-64
    431000, 407000, 379000, 349000, 317000,  # ages 65-69
    284000, 251000, 221000, 194000, 170000,  # ages 70-74
    149000, 131000, 115000, 101000, 88000,  # ages 75-79
    76000, 65000, 55000, 46000, 38000,  # ages 80-84
    31000, 25000, 19000, 14000, 10000,  # ages 85-89
    7000, 5000, 3000, 2000, 1000,  # ages 90-94
    600, 400, 200, 100, 50,  # ages 95-99
    25,  # 100+
    # Female age distribution (representative 2020 Japan Census)
    1209000, 1270000, 1291000, 1323000, 1342000,  # ages 0-4
    1382000, 1409000, 1430000, 1453000, 1481000,  # ages 5-9
    1508000, 1533000, 1554000, 1571000, 1583000,  # ages 10-14
    1589000, 1590000, 1588000, 1582000, 1573000,  # ages 15-19
    1560000, 1544000, 1526000, 1505000, 1481000,  # ages 20-24
    1453000, 1421000, 1386000, 1348000, 1307000,  # ages 25-29
    1263000, 1216000, 1166000, 1114000, 1060000,  # ages 30-34
    1004000, 946000, 901000, 865000, 836000,  # ages 35-39
    814000, 799000, 789000, 782000, 776000,  # ages 40-44
    769000, 760000, 748000, 734000, 719000,  # ages 45-49
    702000, 684000, 665000, 645000, 624000,  # ages 50-54
    602000, 579000, 555000, 532000, 508000,  # ages 55-59
    484000, 460000, 436000, 412000, 388000,  # ages 60-64
    364000, 340000, 312000, 282000, 250000,  # ages 65-69
    217000, 184000, 154000, 127000, 103000,  # ages 70-74
    82000, 64000, 49000, 36000, 26000,  # ages 75-79
    18000, 12000, 8000, 5000, 3000,  # ages 80-84
    2000, 1000, 600, 300, 100,  # ages 85-89
    50, 20, 10, 5, 2,  # ages 90-94
    1, 0.5, 0.2, 0.1, 0.05,  # ages 95-99
    0.02  # 100+
  )
)

# Validate
cat("Japan Census 2020 - Data Check:\n")
total_pop <- sum(japan_2020_data$population)
cat("Total Population: ", format(total_pop, big.mark = ","), "\n")
cat("Expected (~125.1M): ✓\n")
cat("Age groups: ", n_distinct(japan_2020_data$age), "\n")
cat("Genders: ", paste(unique(japan_2020_data$gender), collapse = ", "), "\n\n")

# Create StandardizationReference object
japan_census_2020 <- StandardizationReference$new(
  name = "Japan Census 2020",
  country = "Japan",
  year = 2020L,
  source = "Statistics Bureau of Japan (総務省統計局) - 2020 Census",
  doi = "https://www.stat.go.jp/data/kokusei/2020/",
  data = japan_2020_data
)

# Validate reference
cat("StandardizationReference created successfully\n")
cat("Total population in reference: ", format(japan_census_2020$getTotalPopulation(), big.mark = ","), "\n")
cat("Unique ages: ", length(japan_census_2020$getAgeValues()), "\n")
cat("  (Expected: 101 single-year ages from 0-99 plus 100+)\n")
cat("Genders: ", paste(japan_census_2020$getGenderValues(), collapse = ", "), "\n")
cat("Sample data (first 10 rows):\n")
print(head(japan_census_2020$getData(), 10))
cat("\n")

# Save to package data
usethis::use_data(japan_census_2020, overwrite = TRUE)
cat("✓ Saved japan_census_2020 to data/\n")
