/* Period Prevalence Denominator (pd3) - Any time in POI - ERA PATTERN
 the number of persons who contributes at least 1 day in the period of interest
 Used with: pn2 (period prevalence numerator)
 Pattern: ERA - Interval Overlap Detection
 case_event is computed on every eligible row (per cohort episode), then collapsed with
 MAX() so a subject-span counts as a case if ANY episode qualifies. */
DROP TABLE IF EXISTS #allEvents;
CREATE TEMP TABLE #allEvents AS
WITH withCase AS (
  -- flag every eligible row (one per cohort episode) as a case or not, before collapsing
  SELECT *,
    CASE WHEN
      cohort_start_date <= calendar_end_date
      AND @anchor_date >= DATEADD(day, -@lookback, calendar_start_date)
    THEN 1 ELSE 0 END AS case_event
  FROM #obsPopYear
)
SELECT subject_id, span_label, calendar_start_date, calendar_end_date,
  MIN(CASE WHEN case_event = 1 THEN cohort_start_date END) AS cohort_start_date,
  MAX(case_event) AS case_event
  {strata}
FROM withCase
GROUP BY subject_id, span_label, calendar_start_date, calendar_end_date{strata}
;


