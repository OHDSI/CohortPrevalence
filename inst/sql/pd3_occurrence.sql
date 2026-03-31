/* Period Prevalence Denominator (pd3) - Any time in POI - OCCURRENCE PATTERN
 the number of persons who contributes at least 1 day in the period of interest
 Used with: pn2 (period prevalence numerator)
 Pattern: OCCURRENCE - Point-in-Time Detection (closest to calendar_start_date)
 */
DROP TABLE IF EXISTS #denom;
CREATE TABLE #denom AS
SELECT
  subject_id,
  cohort_definition_id,
  cohort_start_date,
  cohort_end_date,
  span_label,
  calendar_start_date,
  calendar_end_date
  {strata}
FROM (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, span_label
      ORDER BY 
        ABS(DATEDIFF(day, calendar_start_date, cohort_start_date)),
        observation_period_start_date
    ) AS rn1
  FROM #obsPopYear
) ranked
WHERE rn1 = 1;

/* Period Prevalence Numerator (pn2) - OCCURRENCE PATTERN
 Point-in-Time: Event (cohort_start_date) must fall within the lookback + POI window
 If multiple events exist, the closest to calendar_start_date is already selected above */
DROP TABLE IF EXISTS #allEvents;
CREATE TABLE #allEvents AS
SELECT *,
  CASE WHEN cohort_start_date BETWEEN DATEADD(day, -@lookback, calendar_start_date) AND calendar_end_date THEN 1 ELSE 0 END AS case_event
FROM #denom
;
