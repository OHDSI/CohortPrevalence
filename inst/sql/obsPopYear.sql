/* create poi by year */
/* THIS IS TEMPORARY FOR TESTING
DROP TABLE IF EXISTS #year_interval;
  CREATE TEMP TABLE #year_interval  (calendar_year INTEGER);
INSERT INTO #year_interval (calendar_year)
VALUES ( '2016'),
('2017'),
('2018'),
('2019'),
('2020'),
('2021'),
('2022'),
('2023'),
('2024');

*/

/* Step 3a: For yearly case, get calendar_year, start date, and end date
TODO: include @strata param for other strata variables */
DROP TABLE IF EXISTS #obsPopYear;
CREATE TEMP TABLE #obsPopYear
AS
SELECT subject_id, calendar_year, cohort_definition_id,
      DATEFROMPARTS(calendar_year, 1, 1) AS calendar_start_date,
      DATEFROMPARTS(calendar_year + 1, 1, 1) AS calendar_end_date,
      observation_period_start_date, observation_period_end_date,
      cohort_start_date, cohort_end_date,
      /* compute age */
      calendar_year - year_of_birth AS age,
      gender_concept_id -- put other strata here
FROM(
  SELECT * FROM #obsPopMain
  /* join on years of interest to get valid observation periods */
  INNER JOIN #year_interval  b
    ON EXTRACT(YEAR FROM observation_period_start_date) <= b.calendar_year
    AND EXTRACT(YEAR FROM observation_period_end_date) >= b.calendar_year
);

/*
-- old code for SqlRender compatibilities
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
*/
