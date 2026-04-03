# Japan Census 2020 Reference Data

This directory contains scripts to download and process official Japan Census 2020 population data from the Statistics Bureau of Japan (e-Stat portal).

## Quick Start

### 1. Download Excel File

Visit: https://www.e-stat.go.jp/en/stat-search/file-download?statInfId=000032142408&fileKind=0

This is **Table 2-5: Population by Sex, Age (single years) and All nationality or Japanese - Japan**

- Click the **[EXCEL]** button to download
- The file will be named something like: `t_kokusei_20_00200521_00032142408.xlsx`
- Save it to this directory: `data-raw/japan_census/`

### 2. Run the Processing Script

In R (from the project root):

```r
source("data-raw/japan_census/process_japan_census_2020.R")
```

The script will:
- Detect the downloaded Excel file automatically
- Extract Japan's total population by age and sex
- Create a `StandardizationReference` object
- Generate the `data/japan_census_2020.rda` file for use in the package

### 3. Verify Results

After running successfully, you should see:
- Total population ≈ 125.1 million
- 202 rows of data (101 ages × 2 genders)
- Ages from 0 to 100+
- Genders: Male, Female

## File Structure

**Excel File Format (e-Stat Table 2-5):**
- Row 1: Title/metadata
- Row 2: Headers
- Row 3: Japan total data starting here
- Columns: Age (single years), Male population, Female population

**Output Format:**
- 3 columns: `age` (character), `gender` (Male/Female), `population` (integer)
- Sorted by gender, then age
- Ready for `StandardizationReference$new()`

## Data Validation

The script validates:
- ✓ All ages present (0-99 plus 100+)
- ✓ Both genders present
- ✓ Population values are numeric and non-negative
- ✓ Total population matches official 2020 Census (~123M)
- Note drop persons with no recorded age

## Files in This Directory

- `process_japan_census_2020.R` - Main processing script
- `README.md` - This file
- `b02_05e.xlsx` - Downloaded Excel file (you provide)

## Troubleshooting

**"Excel file not found"**
- Verify the file is saved in `data-raw/japan_census/`
- Check the filename starts with `t_kokusei`

**Column mismatch errors**
- Open the Excel file and verify structure matches description above
- May need to adjust `skip=` parameter in `read_excel()` call
- Check printed output from `head(raw_data)` for column names

**Population totals don't match**
- Verify you downloaded Table 2-5 (not another table number)
- Check that you're extracting Japan total, not individual prefectures
- Ensure no rows were accidentally filtered out

## Reference

- Source: Statistics Bureau of Japan (総務省統計局)
- 2020 Population Census - Basic Tabulation
- e-Stat Portal: https://www.e-stat.go.jp/en
- Official Census Page: https://www.stat.go.jp/data/kokusei/2020/
