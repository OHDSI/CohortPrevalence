library(CohortPrevalence)
analysisId <- 1
prevalentCohort <- createPrevalenceCohort(cohortId = 1923,
                                          cohortName = "test")
periodOfInterest <- createYearlyPrevalence(range = c(2016:2020))
lookBackOptions <- createLookBackOptions()
numeratorType <- "pn1"
denominatorType <- createDenominatorType(denomType = "pd3")
minimumObservationLength = 0L
useOnlyFirstObservationPeriod = FALSE
multiplier = 100000
strata = NULL
populationCohort = NULL

prevalenceAnalysisClass <- createCohortPrevalenceAnalysis(analysisId = analysisId,
                                                          prevalentCohort = prevalentCohort,
                                                          periodOfInterest = periodOfInterest,
                                                          lookBackOptions = lookBackOptions,
                                                          numeratorType = numeratorType,
                                                          denominatorType = denominatorType)
# provide connection
configBlock <- "optum_dod_202504"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)
connection <- DatabaseConnector::connect(connectionDetails)

# execution settings
es <- config::get(config = configBlock) |>
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

executionSettings <- ClinicalCharacteristics::createExecutionSettings(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = es$cdmDatabaseSchema,
  workDatabaseSchema = es$workDatabaseSchema,
  tempEmulationSchema = es$tempEmulationSchema,
  cohortTable = "cohortprev_test_cohort",
  cdmSourceName = "Optum"
)

# Results
outputFolder <- here::here("extras")

results <- generateSinglePrevalence(prevalenceAnalysisClass = prevalenceAnalysisClass,
                                    executionSettings = executionSettings,
                                    connection = connection)
exportPrevalenceQuery(prevalenceAnalysisClass,
                      outputFolder = outputFolder)

exportPrevalenceResults(results,
                        outputFolder = outputFolder)

# get rid of strata to compare counts to baseline
results_test <- results |>
  dplyr::group_by(calendarYear) |>
  dplyr::summarise(numerator = sum(numerator),
                   denominator = sum(denominator)) |>
  dplyr::mutate(prevalence_rate = numerator/denominator * prevalenceAnalysisClass$multiplier)

write.csv(results_test,
          here::here("extras", "results_test.csv"),
          row.names = FALSE,
          quote = FALSE)
