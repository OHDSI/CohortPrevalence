/* Option 3 denominator: any time
 the number of persons who who contributes at least 1 day in the period of interest. This is the most naive denominator and what IHD uses.
*/
DROP TABLE IF EXISTS #denom;
CREATE TABLE #denom AS
WITH diff AS(
    SELECT *, ABS(DATEDIFF(day, cohort_start_date, calendar_start_date)) AS dd
    FROM scratch_lavallem_rwesnow_schema.obsPopYear
    {@pn == "pn1"} ? {WHERE cohort_start_date < calender_start_date}
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, span_label
      ORDER BY dd, observation_period_start_date
    ) AS rn1
  FROM diff
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


{@pn == "pn1"} ? {
  /* Option 1 numerator: day one
  The number of patients who have been observed to have the condition of interest on the first day of the period of interest or within the lookback time */

  DROP TABLE IF EXISTS #allEvents;
  CREATE TABLE #allEvents AS
  SELECT *,
    CASE WHEN cohort_start_date BETWEEN DATEADD(day, -@lookback, calendar_start_date) AND calendar_start_date THEN 1 ELSE 0 END AS case_event
  FROM #denom
  ;
}

{@pn == "pn2"} ? {
  /* Option 2 numerator: any time
  The number of patients who have been observed to have the condition of interest at any time in the period of interest or within the lookback time */

  DROP TABLE IF EXISTS #allEvents;

  CREATE TABLE #allEvents AS
  SELECT *,
    CASE WHEN cohort_start_date BETWEEN DATEADD(day, -@lookback, calendar_start_date) AND calendar_end_date THEN 1 ELSE 0 END AS case_event
  FROM #denom
  ;
}