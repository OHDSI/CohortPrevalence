

DROP TABLE IF EXISTS #target_drug;
CREATE TABLE #target_drug AS
SELECT
    @prevalent_cohort_id AS target_id,
    t.subject_id,
    t.cohort_start_date AS target_start,
    /*t.cohort_end_date AS target_end,*/
    t.span_label,
    t.calendar_start_date,
    t.calendar_end_date,
    d.cohort_definition_id AS drug_occurrence_id,
    d.cohort_start_date AS drug_start_date,
    d.cohort_end_date AS drug_end_date
FROM (
  SELECT *
  FROM #allEvents
  WHERE case_event = 1
) t
JOIN (
    SELECT *
    FROM @work_database_schema.@cohort_table
    WHERE cohort_definition_id IN (@drug_cohort_ids)
)  d ON t.subject_id = d.subject_id
JOIN @cdm_database_schema.observation_period op
  ON t.subject_id = op.person_id
  AND d.cohort_start_date <= op.observation_period_end_date
  AND d.cohort_end_date >= op.observation_period_start_date
WHERE t.calendar_start_date <= d.cohort_end_date AND t.calendar_end_date >= d.cohort_start_date
;

DROP TABLE IF EXISTS #drug_cal_res;
CREATE TABLE #drug_cal_res AS
WITH T1 AS (
  SELECT @prevalent_cohort_id as target_id, span_label, COUNT(DISTINCT subject_id) as tot_year
  FROM #allEvents
  WHERE case_event = 1
  GROUP BY target_id, span_label
),
T2 AS (
  SELECT target_id, span_label, drug_occurrence_id,  COUNT(DISTINCT subject_id) AS n_drug
  FROM #target_drug
  GROUP BY target_id, span_label, drug_occurrence_id
)
SELECT
    a.target_id, a.span_label, b.drug_occurrence_id,
    b.n_drug, a.tot_year
FROM T1 a
JOIN T2 b ON a.target_id = b.target_id and a.span_label = b.span_label
ORDER BY span_label, drug_occurrence_id
;

