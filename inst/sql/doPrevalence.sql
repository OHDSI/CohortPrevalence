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
  COUNT(DISTINCT d.person_id) AS denominator,
  COUNT(DISTINCT n.person_id) AS numerator,
  COUNT(DISTINCT n.person_id) / COUNT(DISTINCT d.person_id) AS prevalence_rate
FROM #denom1 d
LEFT JOIN #allEvents n
  ON d.person_id = n.person_id
GROUP BY d.calendar_year, d.age, d.gender_concept_id;
