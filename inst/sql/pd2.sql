/* Option 2 denominator: complete-period
 the number of persons in the population who contribute all observable person-days in the period of interest. This is the strictest denominator
*/
DROP TABLE IF EXISTS #denom2;
CREATE TEMP TABLE #denom2 AS
WITH qualified AS (
  SELECT *
  FROM #obsPopYear
  WHERE calendar_start_date >= observation_period_start_date
    AND calendar_end_date <= observation_period_end_date;
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
  age,
  gender_concept_id
FROM ranked
WHERE rn1 = 1;
