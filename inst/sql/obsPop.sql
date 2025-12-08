/* Step 1: Initial obsPop
TODO: parametrize minimum obs period
TODO: parametrize picking first obs period */
DROP TABLE IF EXISTS #obsPop;
CREATE TEMP TABLE #obsPop
AS
SELECT subject_id, observation_period_start_date, observation_period_end_date
FROM
(
    SELECT *, person_id AS subject_id,
    ROW_NUMBER() OVER (
    PARTITION BY subject_id
    ORDER BY observation_period_start_date, observation_period_end_date)
    AS ob_row
FROM @cdm_database_schema.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= @min_obs_time -- add min. obs time
)
{@use_first_op} ? {
/* Limit to the first observation period per person*/
WHERE ob_row = 1
}

;

/* Step 2: Left join with cohort and person to get prevalent cases and demographics*/
DROP TABLE IF EXISTS #obsPopMain;
CREATE TEMP TABLE #obsPopMain
AS
SELECT a.subject_id, b.cohort_definition_id,
    a.observation_period_start_date, a.observation_period_end_date,
    a.gender_concept_id, a.year_of_birth, a.race_concept_id,
    b.cohort_start_date, b.cohort_end_date
FROM (
  -- Add demographics from person table
    SELECT a.subject_id, observation_period_start_date, observation_period_end_date,
        gender_concept_id, year_of_birth, race_concept_id
    FROM #obsPop a
    INNER JOIN @cdm_database_schema.person p ON a.subject_id = p.person_id
)a
LEFT JOIN (
  -- Left join on cohort table to get events (cohortId NULL/ not NULL)
    SELECT * FROM @cohort_database_schema.@cohort_table WHERE cohort_definition_id = @prevalent_cohort_id
) b ON a.subject_id = b.subject_id
{@use_observed_time} ? {
/*Use the observed time option*/
}
;
