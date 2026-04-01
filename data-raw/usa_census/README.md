# USA Census 2020 Reference Data

This directory contains a script to download and process official USA Census 2020 population data from the US Census Bureau via the Census API.

## Quick Start

### 1. Get a Census API Key

The script uses the Census Bureau's publicly available API, which requires a free API key.

**To obtain your Census API key:**

1. Visit: https://api.census.gov/data/key_signup.html
2. Complete the signup form (name and email)
3. You'll receive your key via email
4. Save the key for use in R

**To use in R (one-time setup):**

```r
tidycensus::census_api_key("YOUR_API_KEY_HERE", install = TRUE)
```

The `install = TRUE` option saves your key to your `.Renviron` file for automatic loading in future sessions.

### 2. Run the Processing Script

In R (from the project root):

```r
source("data-raw/usa_census/process_usa_census_2020.R")
```

The script will:
- Connect to the Census API using tidycensus
- Load variable definitions for the 2020 Decennial Census (DHC - Demographic Housing Census)
- Query population data for the USA by sex and single-year age (0-102)
- Extract and format the data
- Create a `StandardizationReference` object
- Generate the `data/usa_census_2020.rda` file for use in the package

### 3. Verify Results

After running successfully, you should see:
- Total population ≈ 330 million (2020 USA Census total)
- 206 rows of data (103 ages × 2 genders)
- Ages from 000 to 102 (zero-padded strings)
- Genders: Male, Female

## Data Source Details

**Census Dataset:** 2020 Decennial Census (DHC - Demographic Housing Census)
- Provides complete population counts by detailed characteristics
- Released in phases starting 2021
- DHC summary files contain detailed demographic tabulations

**Variables Used:**
- Census Table: PCT12 (Sex by Single-Year Age)
- Coverage: All 103 age groups (0-102 years)
- Geography: United States (national total)
- Separated by: Male and Female

**API Information:**
- API endpoint: https://api.census.gov/data/2020/dec/dhc
- Uses: `tidycensus::get_decennial()` function
- Free to use (no cost once API key is obtained)

## Files in This Directory

- `process_usa_census_2020.R` - Main processing script
- `README.md` - This file

## File Structure

**Script workflow:**

1. **Load Census Variables**: Uses `tidycensus::load_variables()` to get available variables for the 2020 DHC dataset
2. **Filter for Sex by Age**: Selects only PCT12 variables (Sex by Single-Year Age)
3. **Query Census API**: Makes API call via `tidycensus::get_decennial()` to fetch population data
4. **Format Data**: 
   - Extracts age values (0-102) as zero-padded strings (000-102)
   - Assigns gender (Male/Female) based on variable encoding
   - Keeps only: age, gender, population
5. **Create Reference**: Instantiates `StandardizationReference` object
6. **Save Output**: Uses `usethis::use_data()` to save as R data file

## Troubleshooting

**"Could not find Census API key"**
- Run: `tidycensus::census_api_key("YOUR_KEY", install = TRUE)`
- Verify your `.Renviron` file contains your key
- Restart R after installing the key

**"Could not load variables"**
- Check internet connection
- Verify the Census API is accessible: https://api.census.gov/data/2020/dec/dhc
- The API may have service interruptions

**Data doesn't load**
- Verify you have `tidycensus` package installed: `install.packages("tidycensus")`
- Ensure your API key is valid (test on Census website first)
- Check dataset still uses "dhc" for 2020 (may have changed)

**Expected data quality checks:**
- Total population: ~330.3 million
- Age groups: 103 (ages 0-102)
- Gender breakdown: ~50/50 male/female
- No missing values in population column

## References

- US Census Bureau: https://www.census.gov/
- Census API Documentation: https://api.census.gov/
- tidycensus Package: https://walker-data.com/tidycensus/
- 2020 Decennial Census Data: https://www.census.gov/programs-surveys/decennial/data.html
- Demographic Housing Census (DHC): https://www.census.gov/programs-surveys/decennial/about/2020-census/2020dhc.html
