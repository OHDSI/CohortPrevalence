/* Option 2 numerator: any time
 The number of patients who have been observed to have the condition of interest at any time in the period of interest or within the lookback time */

DROP TABLE IF EXISTS #allEvents;

CREATE TEMP TABLE #allEvents AS
SELECT *,
  CASE WHEN cohort_start_date BETWEEN DATEADD(day, -@lookback, calendar_start_date) AND calendar_end_date THEN 1 ELSE 0 END AS case_event
FROM #denom
;
