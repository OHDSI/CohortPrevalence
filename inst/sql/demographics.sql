/* Summarize selected case demographics from #allEvents. */
DROP TABLE IF EXISTS #demographics;
CREATE TEMP TABLE #demographics AS
WITH case_rows AS (
  SELECT *
  FROM #allEvents
  WHERE case_event = 1
),
span_totals AS (
  SELECT span_label, COUNT(DISTINCT subject_id) AS total_cases
  FROM case_rows
  GROUP BY span_label
),
demographic_counts AS (
  {demographicCounts}
)
SELECT
  counts.span_label,
  counts.demographic,
  counts.demographic_value,
  counts.case_count,
  totals.total_cases,
  1.0 * counts.case_count / NULLIF(totals.total_cases, 0) AS proportion
FROM demographic_counts counts
JOIN span_totals totals
  ON counts.span_label = totals.span_label
ORDER BY counts.span_label, counts.demographic, counts.demographic_value;
