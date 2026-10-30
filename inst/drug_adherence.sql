/* Keep one eligible target row per patient and calendar span. */
DROP TABLE IF EXISTS #drug_adherence_res;
CREATE TABLE #drug_adherence_res AS
WITH target_years AS (
	SELECT DISTINCT
		@prevalent_cohort_id AS target_id,
		subject_id,
		span_label,
		calendar_start_date,
		calendar_end_date
	FROM #allEvents
	WHERE case_event = 1
),
/* Clip inclusive drug-era dates to the half-open calendar span. */
drug_intervals AS (
	SELECT DISTINCT
		y.target_id,
		y.subject_id,
		y.span_label,
		d.cohort_definition_id AS drug_occurrence_id,
		GREATEST(y.calendar_start_date, d.cohort_start_date) AS drug_start,
		LEAST(
			y.calendar_end_date,
			DATEADD(day, 1, d.cohort_end_date)
		) AS drug_end
	FROM target_years y
	JOIN @work_database_schema.@cohort_table d
		ON y.subject_id = d.subject_id
		AND d.cohort_definition_id IN (@drug_cohort_ids)
		AND d.cohort_start_date < y.calendar_end_date
		AND DATEADD(day, 1, d.cohort_end_date) > y.calendar_start_date
),
/* Discard empty intersections after clipping the intervals. */
valid_drug_intervals AS (
	SELECT *
	FROM drug_intervals
	WHERE drug_start < drug_end
),
/* Sum days on drug and find the first-to-last treatment span per patient. */
patient_adherence_days AS (
	SELECT
		target_id,
		subject_id,
		span_label,
		drug_occurrence_id,
		SUM(DATEDIFF(day, drug_start, drug_end)) AS drug_days,
		DATEDIFF(day, MIN(drug_start), MAX(drug_end)) AS treatment_days
	FROM valid_drug_intervals
	GROUP BY target_id, subject_id, span_label, drug_occurrence_id
)
/* Pool patient day counts by target, drug, and span; return percent. */
SELECT
	target_id,
	drug_occurrence_id,
	span_label,
	SUM(drug_days) AS drug_days,
	SUM(treatment_days) AS treatment_days,
	COALESCE(
		100.0 * SUM(drug_days) / NULLIF(SUM(treatment_days), 0),
		0
	) AS adherence_rate
FROM patient_adherence_days
GROUP BY target_id, drug_occurrence_id, span_label
ORDER BY target_id, span_label, drug_occurrence_id
;
