/* Keep one eligible target row per patient and calendar span. */
DROP TABLE IF EXISTS #drug_pers_res;
CREATE TABLE #drug_pers_res AS
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
/* Clip each observation period to its half-open calendar span. */
observed_intervals AS (
    SELECT DISTINCT
        y.target_id,
        y.subject_id,
        y.span_label,
        y.calendar_start_date,
        y.calendar_end_date,
        GREATEST(y.calendar_start_date, op.observation_period_start_date) AS observed_start,
        LEAST(
            y.calendar_end_date,
            DATEADD(day, 1, op.observation_period_end_date)
        ) AS observed_end
    FROM target_years y
    JOIN @cdm_database_schema.observation_period op
        ON y.subject_id = op.person_id
        AND op.observation_period_start_date < y.calendar_end_date
        AND op.observation_period_end_date >= y.calendar_start_date
),
/* Sum observed days per patient and span, independently of drug records. */
observed_days AS (
    SELECT
        target_id,
        subject_id,
        span_label,
        SUM(DATEDIFF(day, observed_start, observed_end)) AS window_time
    FROM observed_intervals
    WHERE observed_start < observed_end
    GROUP BY target_id, subject_id, span_label
),
/* Clip inclusive drug intervals to both the calendar span and observation time. */
drug_intervals AS (
    SELECT DISTINCT
        y.target_id,
        y.subject_id,
        y.span_label,
        d.cohort_definition_id AS drug_occurrence_id,
        GREATEST(
            y.calendar_start_date,
            op.observation_period_start_date,
            d.cohort_start_date
        ) AS drug_start,
        LEAST(
            y.calendar_end_date,
            DATEADD(day, 1, op.observation_period_end_date),
            DATEADD(day, 1, d.cohort_end_date)
        ) AS drug_end
    FROM target_years y
    JOIN @work_database_schema.@cohort_table d
        ON y.subject_id = d.subject_id
        AND d.cohort_definition_id IN (@drug_cohort_ids)
        AND d.cohort_start_date < y.calendar_end_date
        AND DATEADD(day, 1, d.cohort_end_date) > y.calendar_start_date
    JOIN @cdm_database_schema.observation_period op
        ON y.subject_id = op.person_id
        AND op.observation_period_start_date < y.calendar_end_date
        AND op.observation_period_end_date >= y.calendar_start_date
        AND d.cohort_start_date <= op.observation_period_end_date
        AND d.cohort_end_date >= op.observation_period_start_date
),
/* Discard empty intersections after clipping the intervals. */
valid_drug_intervals AS (
    SELECT *
    FROM drug_intervals
    WHERE drug_start < drug_end
),
/* Sum the already non-overlapping eras per patient, span, and drug cohort. */
drug_days AS (
    SELECT
        target_id,
        subject_id,
        span_label,
        drug_occurrence_id,
        SUM(DATEDIFF(day, drug_start, drug_end)) AS comb_drug_time
    FROM valid_drug_intervals
    GROUP BY target_id, subject_id, span_label, drug_occurrence_id
)
/* Pool patient-level day counts by target, drug, and span; return percent. */
SELECT
    d.target_id,
    d.drug_occurrence_id,
    d.span_label,
    SUM(d.comb_drug_time) AS comb_drug_time,
    SUM(COALESCE(o.window_time, 0)) AS window_time,
    COALESCE(
        100.0 * SUM(d.comb_drug_time)
            / NULLIF(SUM(COALESCE(o.window_time, 0)), 0),
        0
    ) AS persistence_rate
FROM drug_days d
LEFT JOIN observed_days o
    ON d.target_id = o.target_id
    AND d.subject_id = o.subject_id
    AND d.span_label = o.span_label
GROUP BY d.target_id, d.drug_occurrence_id, d.span_label
ORDER BY d.target_id, d.span_label, d.drug_occurrence_id
;