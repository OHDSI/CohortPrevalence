/* Option 4 denominator: sufficient time
 the number of persons who contributes sufficient time in the period of interest based on at least n observable person-days in the period of interest
*/
DROP TABLE IF EXISTS #denom;
CREATE TEMP TABLE #denom AS
WITH qualified AS (
  SELECT *
  FROM #obsPopYear
  WHERE calendar_start_date >= observation_period_start_date
    AND DATEADD(day, @requiredDays, observation_period_start_date) <= observation_period_end_date
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
