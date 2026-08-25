/* Step 3a: For yearly case, get calendar_year, start date, and end date
TODO: include @strata param for other strata variables */
DROP TABLE IF EXISTS #obsPopYear;
CREATE TABLE #obsPopYear
AS
WITH eligible_periods AS (
  /* join on years of interest to get valid observation periods */
  SELECT *
  FROM #obsPopMain
  INNER JOIN #year_interval b
    ON observation_period_start_date < b.calendar_end_date
    AND observation_period_end_date >= b.calendar_start_date
/* lead-in: days observed before entering the POI must be >= @lead_in_days. Written as a duration so that @lead_in_days = 0 admits everyone the join returned. */
	WHERE DATEDIFF(day, observation_period_start_date,
	               GREATEST(observation_period_start_date, b.calendar_start_date)) >= @lead_in_days
),
demographics AS (
  SELECT subject_id, span_label, cohort_definition_id,
        calendar_start_date,
        calendar_end_date,
        observation_period_start_date, observation_period_end_date,
        cohort_start_date, cohort_end_date,
        /* compute age */
        EXTRACT(YEAR FROM calendar_start_date) - year_of_birth AS age,
        gender_concept_id AS gender,
        race_concept_id AS race-- put other strata here
  FROM eligible_periods
)
/* demographic constraints */
SELECT *
FROM demographics
WHERE (age >= {ageMin} AND age <= {ageMax}) AND gender IN ({genderIds})
;
