/* Option 1 numerator: day one
 The number of patients who have been observed to have the condition of interest on the first day of the period of interest or within the lookback time */

IF OBJECT_ID('#allEvents', 'U') IS NOT NULL
  DROP TABLE #allEvents;

CREATE TEMP TABLE #allEvents AS
SELECT * FROM #denom1
WHERE cohort_definition_id = @target_id
  AND cohort_start_date BETWEEN DATEADD(day, -@lookback, calendar_start_date) AND calendar_start_date;
