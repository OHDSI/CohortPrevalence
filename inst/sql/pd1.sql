/* Option 1 denominator: day one
 the number of persons in the population who were observed on the first day of the period of interest
 */

DROP TABLE IF EXISTS #denom;
CREATE TABLE #denom AS
WITH qualified AS (
  SELECT *
  FROM #obsPopYear
  WHERE calendar_start_date BETWEEN observation_period_start_date AND observation_period_end_date
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, calendar_year
      ORDER BY cohort_start_date, calendar_start_date, observation_period_start_date
    ) AS rn1
  FROM qualified
)
SELECT
  subject_id,
  cohort_definition_id,
  cohort_start_date,
  cohort_end_date,
  calendar_year,
  calendar_start_date,
  calendar_end_date,
  age,
  gender_concept_id
FROM ranked
WHERE rn1 = 1;
