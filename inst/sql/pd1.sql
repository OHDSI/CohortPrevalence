-- get events table
drop table if exists #denom1;
create temp table #denom1
as
SELECT distinct
  COHORT.subject_id,
  COHORT.cohort_start_date,
  COHORT.cohort_end_date,
  OBSPOP.observation_period_start_date,
  OBSPOP.observation_period_end_date,
  y.calendar_year,
  COHORT.cohort_definition_id
FROM @cohort_database_schema.@cohort_table COHORT
JOIN #obsPopYear OBSPOP ON COHORT.subject_id = OBSPOP.subject_id
JOIN #yearInterval y
  ON DATEFROMPARTS(y.calendar_year, 1, 1) BETWEEN OBSPOP.observation_period_start_date AND OBSPOP.observation_period_end_date;
