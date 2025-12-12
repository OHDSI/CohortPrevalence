/* Step 3a: For yearly case, get calendar_year, start date, and end date
TODO: include @strata param for other strata variables */
DROP TABLE IF EXISTS #obsPopYear;
CREATE TABLE #obsPopYear
AS
SELECT subject_id, span_label, cohort_definition_id,
      calendar_start_date,
      calendar_end_date,
      observation_period_start_date, observation_period_end_date,
      cohort_start_date, cohort_end_date,
      /* compute age */
      EXTRACT(YEAR FROM calendar_start_date) - year_of_birth AS age,
      gender_concept_id AS gender,
      race_concept_id AS race-- put other strata here
FROM(
  SELECT * FROM #obsPopMain
  /* join on years of interest to get valid observation periods */
  INNER JOIN #year_interval  b
    ON EXTRACT(YEAR FROM observation_period_start_date) <= b.calendar_end_date
    AND EXTRACT(YEAR FROM observation_period_end_date) >= b.calendar_start_date
);


