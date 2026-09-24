/* Period Prevalence Denominator (pd4) - Sufficient days in POI - ERA PATTERN
 the number of persons who contributes sufficient time in the period of interest based on at least n observable person-days
 Used with: pn2 (period prevalence numerator)
 Pattern: ERA - Interval Overlap Detection
 Case status is evaluated for every eligible cohort episode, then collapsed
 with MAX() so a subject-span is a case if ANY episode qualifies.
 */
DROP TABLE IF EXISTS #allEvents;
CREATE TEMP TABLE #allEvents AS
WITH withCase AS (
  SELECT *,
    CASE WHEN
      cohort_start_date < calendar_end_date
      AND @anchor_date >= DATEADD(day, -@lookback, calendar_start_date)
    THEN 1 ELSE 0 END AS case_event
  FROM #obsPopYear
  /* Count observed days over the half-open POI; observation-period ends are inclusive. */
  WHERE DATEDIFF(
    day,
    GREATEST(calendar_start_date, observation_period_start_date),
    LEAST(calendar_end_date, DATEADD(day, 1, observation_period_end_date))
  ) >= {sufficientDays}
)
SELECT subject_id, span_label, calendar_start_date, calendar_end_date,
  MIN(CASE WHEN case_event = 1 THEN cohort_start_date END) AS cohort_start_date,
  MAX(case_event) AS case_event
  {strata}
FROM withCase
GROUP BY subject_id, span_label, calendar_start_date, calendar_end_date{strata}
;