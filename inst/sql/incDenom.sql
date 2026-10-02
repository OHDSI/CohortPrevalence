/*
Denominator for incidence calcuation
*/
DROP TABLE IF EXISTS #denomInc;
CREATE TABLE #denomInc AS
/* Find prevalent subjects and the earliest incident date once per person/span. */
WITH event_status AS (
  SELECT
    subject_id,
    span_label,
    MAX(CASE WHEN cohort_start_date < calendar_start_date THEN 1 ELSE 0 END) AS prevalent_event,
    MIN(
      CASE
        WHEN cohort_start_date >= calendar_start_date
          AND cohort_start_date < calendar_end_date
        THEN cohort_start_date
      END
    ) AS incident_date
  FROM #obsPopYear
  GROUP BY subject_id, span_label
),
/* Remove repeated cohort-join rows but retain each distinct observation window. */
observation_periods AS (
  SELECT DISTINCT
    subject_id,
    span_label,
    calendar_start_date,
    calendar_end_date,
    observation_period_start_date,
    observation_period_end_date
    {strata}
  FROM #obsPopYear
),
/* Clip each observation window to the POI and observation dates; censor at the
   day before the incident so the event date itself contributes no risk time. */
risk_intervals AS (
  SELECT
    observation_periods.subject_id,
    observation_periods.span_label,
    event_status.incident_date,
    CASE WHEN event_status.incident_date IS NULL THEN 0 ELSE 1 END AS inc_event,
    GREATEST(
      DATEDIFF(
        day,
        GREATEST(
          observation_periods.calendar_start_date,
          observation_periods.observation_period_start_date
        ),
        LEAST(
          observation_periods.calendar_end_date,
          observation_periods.observation_period_end_date,
          COALESCE(
            DATEADD(day, -1, event_status.incident_date),
            observation_periods.calendar_end_date
          )
        )
      ),
      0
    ) AS time_at_risk
    {strata}
  FROM observation_periods
  INNER JOIN event_status
    ON observation_periods.subject_id = event_status.subject_id
    AND observation_periods.span_label = event_status.span_label
  WHERE event_status.prevalent_event = 0
)
SELECT
  subject_id,
  MAX(inc_event) AS inc_event,
  span_label,
  SUM(time_at_risk) AS time_at_risk
  {strata}
FROM risk_intervals
GROUP BY subject_id, span_label{strata}
;


