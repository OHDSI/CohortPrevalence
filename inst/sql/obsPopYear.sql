DROP TABLE IF EXISTS #year_interval;
CREATE TEMP TABLE #year_interval
AS
SELECT
DISTINCT EXTRACT(YEAR FROM a) AS calendar_year
FROM
(
  SELECT cohort_start_date AS a FROM @cohort_database_schema.@cohort_table
  UNION ALL
  SELECT cohort_end_date FROM @cohort_database_schema.@cohort_table
)
WHERE EXTRACT(YEAR FROM a) BETWEEN @start_year AND @end_year
ORDER BY calendar_year;

DROP TABLE IF EXISTS #obsPopYear;
CREATE TEMP TABLE #obsPopYear
AS
SELECT person_id, calendar_year, calendar_start_date, calendar_end_date, observation_period_start_date, observation_period_end_date,
gender_concept_id, (op.calendar_year - c.year_of_birth) AS age
FROM(
  SELECT * FROM #obsPop
  INNER JOIN #year_interval  b
    ON EXTRACT(YEAR FROM observation_period_start_date) <= b.calendar_year
    AND EXTRACT(YEAR FROM observation_period_end_date) >= b.calendar_year
);
