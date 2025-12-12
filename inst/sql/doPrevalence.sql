-- Do prevalence
IF OBJECT_ID('#prevalence', 'U') IS NOT NULL
  DROP TABLE #prevalence;

CREATE TABLE #prevalence AS
SELECT
  calendar_year
  {strata}
  ,SUM(case_event) AS numerator
  ,COUNT(DISTINCT subject_id) AS denominator
  ,(SUM(case_event) / COUNT(DISTINCT subject_id)) * @multiplier AS prevalence_rate
FROM #allEvents
GROUP BY calendar_year{strata};
