/* Summarize selected case demographics by requested measure. */
DROP TABLE IF EXISTS #demographics;
CREATE TEMP TABLE #demographics AS
WITH case_rows AS (
  {caseRows}
),
span_totals AS (
  SELECT measure_type, span_label, COUNT(DISTINCT subject_id) AS total_cases
  FROM case_rows
  GROUP BY measure_type, span_label
),
demographic_counts AS (
  {demographicCounts}
)
SELECT
  counts.measure_type,
  counts.span_label,
  counts.demographic,
  counts.demographic_value,
  counts.case_count,
  totals.total_cases,
  1.0 * counts.case_count / NULLIF(totals.total_cases, 0) AS proportion
FROM demographic_counts counts
JOIN span_totals totals
  ON counts.measure_type = totals.measure_type
  AND counts.span_label = totals.span_label
ORDER BY counts.measure_type, counts.span_label, counts.demographic, counts.demographic_value;
