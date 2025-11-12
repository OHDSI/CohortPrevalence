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
  COUNT(DISTINCT d.subject_id) AS denominator,
  COUNT(DISTINCT n.subject_id) AS numerator,
  COUNT(DISTINCT n.subject_id) / COUNT(DISTINCT d.subject_id) AS prevalence_rate
FROM #denom1 d
LEFT JOIN #allEvents n
  ON d.person_id = n.person_id
    AND d.calendar_year = n.calendar_year
GROUP BY d.calendar_year, d.age, d.gender_concept_id; -- todo: add @strata
