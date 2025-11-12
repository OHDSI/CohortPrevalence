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

/* create poi by year */
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

 -- Get one entry per patient
IF OBJECT_ID('#firstEvents', 'U') IS NOT NULL
  DROP TABLE #firstEvents;

CREATE TEMP TABLE #firstEvents AS
SELECT *
FROM(
  SELECT
  person_id,
  ROW_NUMBER() OVER (PARTITION BY person_id, calendar_year ORDER BY index_date, observation_period_start_date) as rn1,
  calendar_year,
  gender_concept_id,
  age
  FROM #allEvents
)
WHERE rn1 = 1;

