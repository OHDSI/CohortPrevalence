/* Find eligible observation periods per patient
1) does the op have a minimum amount of time
2) do we want the first op or any op
*/
DROP TABLE IF EXISTS #obsPop;
CREATE TEMP TABLE #obsPop
AS
SELECT
o.subject_id, observation_period_start_date, observation_period_end_date, cohort_start_date, cohort_end_date, year_of_birth, gender_concept_id
FROM
(
    SELECT *, person_id AS subject_id,
    ROW_NUMBER() OVER (
    PARTITION BY subject_id
    ORDER BY observation_period_start_date, observation_period_end_date)
    AS ob_row
FROM @cdmDatabaseSchema.observation_period
WHERE DATEDIFF(day, observation_period_start_date, observation_period_end_date) >= 0 -- add min. obs time
) o
LEFT JOIN(
SELECT subject_id, cohort_start_date, cohort_end_date FROM @cohort_database_schema.@cohort_table
) cohort
ON o.subject_id = cohort.subject_id
INNER JOIN @cdmDatabaseSchema.person p
ON o.subject_id = p.person_id
--WHERE ob_row = 1
;
