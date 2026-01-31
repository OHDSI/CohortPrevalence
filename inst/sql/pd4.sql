/* Option 4 denominator: sufficient time
 the number of persons who contributes sufficient time in the period of interest based on at least n observable person-days in the period of interest
*/
DROP TABLE IF EXISTS #denom;
CREATE TABLE #denom AS
WITH qualified AS (
  SELECT *
  FROM #obsPopYear
  WHERE DATEDIFF(day, GREATEST(calendar_start_date, observation_period_start_date), LEAST(calendar_end_date, observation_period_end_date)) >= {sufficientDays}
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, span_label
      ORDER BY cohort_start_date, calendar_start_date, observation_period_start_date
    ) AS rn1
  FROM qualified
)
SELECT
  subject_id,
  cohort_definition_id,
  cohort_start_date,
  cohort_end_date,
  span_label,
  calendar_start_date,
  calendar_end_date
  {strata}
FROM ranked
WHERE rn1 = 1;

