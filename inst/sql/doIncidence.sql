-- Do prevalence
IF OBJECT_ID('#incidence', 'U') IS NOT NULL
  DROP TABLE #incidence;

CREATE TABLE #incidence AS
SELECT
  span_label
  {strata}
  ,SUM(inc_event) AS numerator
  ,SUM(time_at_risk) / 365.25 AS denominator
  ,(SUM(inc_event) / SUM(time_at_risk) / 365.25) * @multiplier AS incidence_rate
FROM (
  SELECT *,
        CASE WHEN inc_event = 1 THEN DATEDIFF(day, calendar_start_date, cohort_start_date)
            ELSE DATEDIFF(day, calendar_start_date, calendar_end_date) END AS time_at_risk
  FROM #denomInc
)
GROUP BY span_label{strata};
