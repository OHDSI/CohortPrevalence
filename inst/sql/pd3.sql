/* Option 3 denominator: any time
 the number of persons who who contributes at least 1 day in the period of interest. This is the most naive denominator and what IHD uses.
*/
DROP TABLE IF EXISTS #denom;
CREATE TABLE #denom AS
WITH ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, calendar_year
      ORDER BY cohort_start_date, calendar_start_date, observation_period_start_date
    ) AS rn1
  FROM #obsPopYear
)
SELECT
  subject_id,
  cohort_definition_id,
  cohort_start_date,
  cohort_end_date,
  calendar_year,
  calendar_start_date,
  calendar_end_date
  {strata}
FROM ranked
WHERE rn1 = 1;
