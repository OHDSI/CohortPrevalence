/* Keep one eligible target row per patient and calendar span. */
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
/* Find the furthest end among prior intervals to detect nested overlaps. */
intervals_with_prior_end AS (
	SELECT
		*,
		MAX(drug_end) OVER (
			PARTITION BY target_id, subject_id, span_label, drug_occurrence_id
			ORDER BY drug_start, drug_end
			ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
		) AS prior_drug_end
	FROM valid_drug_intervals
),
/* Mark the beginning of each non-overlapping run of drug intervals. */
interval_island_starts AS (
	SELECT
		*,
		CASE
			WHEN prior_drug_end IS NULL OR drug_start > prior_drug_end THEN 1
			ELSE 0
		END AS starts_new_island
	FROM intervals_with_prior_end
),
/* Assign a shared island ID to each overlapping or adjacent run. */
numbered_drug_intervals AS (
	SELECT
		*,
		SUM(starts_new_island) OVER (
			PARTITION BY target_id, subject_id, span_label, drug_occurrence_id
			ORDER BY drug_start, drug_end
			ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
		) AS island_id
	FROM interval_island_starts
),
/* Collapse each island so overlapping days count only once in the numerator. */
merged_drug_intervals AS (
	SELECT
		target_id,
		subject_id,
		span_label,
		drug_occurrence_id,
		island_id,
		MIN(drug_start) AS drug_start,
		MAX(drug_end) AS drug_end
	FROM numbered_drug_intervals
	GROUP BY target_id, subject_id, span_label, drug_occurrence_id, island_id
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
	FROM merged_drug_intervals
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
