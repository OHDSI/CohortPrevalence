/* -- Get one entry per patient
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
*/

-- Do prevalence
IF OBJECT_ID('#prevalence', 'U') IS NOT NULL
  DROP TABLE #prevalence;

CREATE TEMP TABLE #prevalence AS
SELECT
  d.calendar_year,
  d.age,
  d.gender_concept_id,
  SUM(case_event) AS numerator,
  COUNT(DISTINCT subject_id) AS denominator,
  (SUM(case_event) / COUNT(DISTINCT subject_id)) * 100000 AS prevalence_rate
FROM #allEvents
GROUP BY d.calendar_year, d.age, d.gender_concept_id;
