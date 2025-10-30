IF OBJECT_ID('#allEvents', 'U') IS NOT NULL
  DROP TABLE #allEvents;

CREATE TEMP TABLE #allEvents AS
SELECT * FROM #denom3
WHERE COHORT.cohort_definition_id = @target_id
  AND COHORT.cohort_start_date BETWEEN DATEADD(day, -@lookback, OBSPOP.observation_period_start_date) AND OBSPOP.observation_period_end_date;
