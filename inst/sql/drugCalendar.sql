
DROP TABLE IF EXISTS #target_drug;
CREATE TABLE #target_drug AS
SELECT
    t.cohort_definition_id AS target_id,
    t.subject_id,
    t.cohort_start_date AS target_start,
    t.cohort_end_date AS target_end,
    t.span_label,
    t.calendar_start_date,
    t.calendar_end_date,
    cc.codeset_id AS raw_occurrence_id,
    d.drug_exposure_start_date AS event_date
FROM (
  SELECT *
  FROM #denom
  WHERE cohort_definition_id IS NOT NULL
  AND cohort_start_date BETWEEN DATEADD (day, -{lookBack}, calendar_start_date) AND calendar_end_date
) t
JOIN @cdm_database_schema.drug_exposure d ON t.subject_id = d.person_id
JOIN #Codeset cc
    ON d.drug_concept_id = cc.concept_id
  WHERE t.calendar_start_date <= d.drug_exposure_start_date AND t.calendar_end_date >= d.drug_exposure_start_date
;



/* Calculate the proportion of drug use per calendar year */
DROP TABLE IF EXISTS #drug_cal_res;
CREATE TABLE #drug_cal_res AS
WITH T1 AS (
  SELECT cohort_definition_id AS target_id, span_label, COUNT(DISTINCT subject_id) AS tot_year
  FROM #denom
  WHERE cohort_definition_id IS NOT NULL
  AND cohort_start_date BETWEEN DATEADD (day, -{lookBack}, calendar_start_date) AND calendar_end_date
  GROUP BY cohort_definition_id, span_label
),
T2 AS (
    SELECT target_id, span_label, raw_occurrence_id, COUNT(DISTINCT subject_id) AS n_drug
    FROM #target_drug
    GROUP BY target_id, span_label, raw_occurrence_id
)
SELECT a.target_id, a.span_label, b.raw_occurrence_id, b.n_drug, a.tot_year, (b.n_drug / a.tot_year) * 100 as pct
FROM T1 a
JOIN T2 b ON a.target_id = b.target_id and a.span_label = b.span_label
ORDER BY span_label, raw_occurrence_id
;
