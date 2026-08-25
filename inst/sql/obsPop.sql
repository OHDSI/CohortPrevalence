/* 
Step 1: Join with person and cohort to get demographics + prevalent case flag
   obsPop is inlined as a CTE since it's only a filter/rename over observation_period
   gender filter applied here (pre-year-join) so it doesn't get re-checked once
   rows are fanned out per calendar year in obsPopYear.sql 
*/
DROP TABLE IF EXISTS #obsPopMain;
CREATE TEMP TABLE #obsPopMain
AS
WITH obsPop AS (
    {@use_first_op} ? {
    /* Select first row of observation period table since only want first observation period*/
    SELECT subject_id, observation_period_start_date, observation_period_end_date
    FROM
    (
        SELECT person_id AS subject_id, observation_period_start_date, observation_period_end_date,
        ROW_NUMBER() OVER (
        PARTITION BY person_id
        ORDER BY observation_period_start_date, observation_period_end_date)
        AS ob_row
        FROM @cdm_database_schema.observation_period
    ) ob
    WHERE ob_row = 1
    } : {
    /* Select unfiltered observation period table */
    SELECT person_id AS subject_id, observation_period_start_date, observation_period_end_date
    FROM @cdm_database_schema.observation_period
    }
)
SELECT a.subject_id, b.cohort_definition_id,
    a.observation_period_start_date, a.observation_period_end_date,
    p.gender_concept_id, p.year_of_birth, p.race_concept_id, p.ethnicity_concept_id,
    b.cohort_start_date, b.cohort_end_date
FROM obsPop a
INNER JOIN @cdm_database_schema.person p ON a.subject_id = p.person_id
LEFT JOIN (
  -- Left join on cohort table to get events (cohortId NULL/ not NULL)
    SELECT * FROM @cohort_database_schema.@cohort_table WHERE cohort_definition_id = @prevalent_cohort_id
) b ON a.subject_id = b.subject_id
;