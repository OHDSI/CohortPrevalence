# Master Script: Generate All Standardization References
#
# This script generates all reference populations used by the standardization feature.
# Run this once to create all reference data files in data/
#
# Prerequisites:
# - StandardizationReference R6 class is defined (via devtools::load_all() or source())
# - All individual processing scripts exist in data-raw/{country}/

library(cli)

# Load the StandardizationReference class definition first
source("R/StandardizationReference.R")

cli::cli_h1("CohortPrevalence Standardization Reference Data Generation")
cli::cli_inform("Generating all reference populations...\n")

# Generate USA Census 2020
cli::cli_h2("1. USA Census 2020")
tryCatch({
  source("data-raw/usa_census/process_usa_census_2020.R")
  cli::cli_alert_success("USA Census 2020 completed\n")
}, error = function(e) {
  cli::cli_alert_danger("USA Census 2020 FAILED: {e$message}\n")
})

# Generate Japan Census 2020
cli::cli_h2("2. Japan Census 2020")
tryCatch({
  source("data-raw/japan_census/process_japan_census_2020.R")
  cli::cli_alert_success("Japan Census 2020 completed\n")
}, error = function(e) {
  cli::cli_alert_danger("Japan Census 2020 FAILED: {e$message}\n")
})

# Generate WHO World Standard 2008
cli::cli_h2("3. WHO World Standard 2008")
tryCatch({
  source("data-raw/who_standard/process_who_2008.R")
  cli::cli_alert_success("WHO World Standard 2008 completed\n")
}, error = function(e) {
  cli::cli_alert_danger("WHO World Standard 2008 FAILED: {e$message}\n")
})

# Summary
cli::cli_h1("Reference Data Generation Complete")
cli::cli_inform("All reference populations saved to data/\n")

# List generated files
ref_files <- list.files("data", pattern = "\\.rda$", full.names = FALSE)
cli::cli_inform("Generated reference files:")
for (f in ref_files) {
  file_size <- file.size(file.path("data", f)) / 1024  # KB
  cli::cli_li("{.file {f}} ({format(file_size, digits=1)} KB)")
}

cli::cli_inform("\nReferences are now available via:")
cli::cli_code('
data("usa_census_2020")
data("japan_census_2020")
data("who_world_standard")

# Or use helper functions:
ref <- getStandardizationReference("usa_census_2020")
listStandardizationReferences()
')

cli::cli_inform("\nNext steps:")
cli::cli_li("Build package: devtools::load_all()")
cli::cli_li("Test standardization: standardizePrevalence(...)")
cli::cli_li("Create vignette: vignettes/standardization.Rmd")
