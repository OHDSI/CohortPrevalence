/* Period Prevalence Denominator (pd2) - All time observed during POI - ERA PATTERN
 the number of persons in the population who contribute all observable person-days in the period of interest
 Used with: pn2 (period prevalence numerator)
 Pattern: ERA - Interval Overlap Detection
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
  FROM (
    /* Get qualified events that are observed over the complete poi */
    SELECT *
    FROM #obsPopYear
    WHERE calendar_start_date >= observation_period_start_date AND calendar_end_date < observation_period_end_date
  ) diff
) ranked
WHERE rn1 = 1;

/* Period Prevalence Numerator (pn2) - ERA PATTERN
 Interval Overlap: Person has the condition era overlapping with the lookback + POI window */
DROP TABLE IF EXISTS #allEvents;
CREATE TABLE #allEvents AS
SELECT *,
  CASE WHEN
    cohort_start_date <= calendar_end_date
    AND cohort_end_date >= DATEADD(day, -@lookback, calendar_start_date)
  THEN 1 ELSE 0 END AS case_event
FROM #denom
;
