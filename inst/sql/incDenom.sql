/*
Denominator for incidence calcuation
*/
DROP TABLE IF EXISTS #denomInc;
CREATE TABLE #denomInc AS
WITH ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id, span_label
      ORDER BY cohort_start_date, calendar_start_date, observation_period_start_date
    ) AS rn1,
    /* identify incident events */
    CASE WHEN cohort_start_date IS NULL THEN 0
            WHEN (cohort_start_date >= calendar_start_date AND cohort_start_date < calendar_end_date) THEN 1
            ELSE 2 END AS inc_event
  FROM #obsPopYear
)
SELECT
  subject_id,
  inc_event,
  cohort_definition_id,
  cohort_start_date,
  cohort_end_date,
  span_label,
  calendar_start_date,
  calendar_end_date
  {strata}
FROM ranked
WHERE rn1 = 1 and inc_event != 2 /* remove prevalent events*/
;


