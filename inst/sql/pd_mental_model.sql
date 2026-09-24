/*
Demonstration only: explicit denominator-pool mental model for PD2 ERA prevalence.

This file is not selected by the analysis runner, which loads the file named for
the selected denominator type with the _era.sql suffix (e.g., pd2_era.sql).
The production query combines pool eligibility and case
collapse in one aggregation; see the optimization notes at the end of this file.
*/
WITH eligible AS (
  /* First establish the rows eligible for the denominator pool:
     observation covers the complete half-open POI [calendar_start_date, calendar_end_date).
     observation_period_end_date is inclusive, so add one day to its boundary. */
  SELECT *
  FROM #obsPopYear
  WHERE calendar_start_date >= observation_period_start_date
    AND calendar_end_date <= DATEADD(day, 1, observation_period_end_date)
),
denominator_pool AS (
  /* One denominator record per subject-span (and configured strata). */
  SELECT DISTINCT
    subject_id,
    span_label,
    calendar_start_date,
    calendar_end_date
    {strata}
  FROM eligible
),
case_by_span AS (
  /* Case status is evaluated across every eligible cohort episode.
     MAX means a subject-span is a case if ANY eligible episode qualifies. */
  SELECT
    subject_id,
    span_label,
    calendar_start_date,
    calendar_end_date,
    MIN(CASE WHEN
      cohort_start_date <= calendar_end_date
      AND @anchor_date >= DATEADD(day, -@lookback, calendar_start_date)
      THEN cohort_start_date END) AS cohort_start_date,
    MAX(CASE WHEN
      cohort_start_date <= calendar_end_date
      AND @anchor_date >= DATEADD(day, -@lookback, calendar_start_date)
      THEN 1 ELSE 0 END) AS case_event
  FROM eligible
  GROUP BY subject_id, span_label, calendar_start_date, calendar_end_date
)
SELECT
  pool.*,
  cases.cohort_start_date,
  COALESCE(cases.case_event, 0) AS case_event
FROM denominator_pool pool
LEFT JOIN case_by_span cases
  ON pool.subject_id = cases.subject_id
  AND pool.span_label = cases.span_label
  AND pool.calendar_start_date = cases.calendar_start_date
  AND pool.calendar_end_date = cases.calendar_end_date
;

/*
Optimization notes:
- This separates denominator eligibility (DISTINCT pool) from case ascertainment
  (episode-level CASE followed by GROUP BY), then joins the results. It is written
  this way to make the statistical logic explicit, not as the recommended runtime SQL.
- The production pd2_era.sql fuses those steps: it filters eligible rows, computes
  case_event on those rows, and uses one GROUP BY with MAX(case_event). That avoids
  the separate DISTINCT pool operation and final join, and may avoid repeated work
  over the eligible rows.
- SQL engines differ: a CTE may be inlined, materialized, or otherwise optimized.
  Compare translated SQL and execution plans before claiming a performance gain.
- This demonstration assumes strata are person-span attributes. If a configured
  stratum can vary within a subject-span, include those columns in case_by_span's
  grouping and in the final join, using null-safe equality where nullable.
*/
