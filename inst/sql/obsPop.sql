/* Step 1: Initial obsPop
TODO: parametrize minimum obs period
TODO: parametrize picking first obs period */
DROP TABLE IF EXISTS #obsPop;
CREATE TEMP TABLE #obsPop
AS
SELECT
  person_id, observation_period_start_date, observation_period_end_date
FROM (
    SELECT person_id, observation_period_start_date, observation_period_end_date,
    ROW_NUMBER() OVER ( PARTITION BY subject_id ORDER BY observation_period_start_date, observation_period_end_date) AS ob_row
FROM @cdmDatabaseSchema.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= 0 -- add min. obs time
)
--WHERE ob_row = 1
;

/* Step 2: Left join with cohort and person to get prevalent cases and demographics*/
DROP TABLE IF EXISTS #obsPopMain;
CREATE TEMP TABLE #obsPopMain
AS
SELECT a.person_id AS subject_id, b.cohort_definition_id,
    a.observation_period_start_date, a.observation_period_end_date,
    a.gender_concept_id, a.year_of_birth, b.cohort_start_date, b.cohort_end_date
FROM (
  -- Add demographics from person table
    SELECT a.person_id, observation_period_start_date, observation_period_end_date,
        gender_concept_id, year_of_birth
    FROM #obsPop a
    INNER JOIN @cdmDatabseSchema.person p ON a.person_id = p.person_id
)a
LEFT JOIN (
  -- Left join on cohort table to get events (cohortId NULL/ not NULL)
    SELECT * FROM @cohortDatabaseSchema.@cohortTable WHERE cohort_definition_id = @targetId
) b ON a.person_id = b.subject_id
;
