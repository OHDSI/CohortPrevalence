/*
Summarize case demographics from #allEvents.

Returns one row per span, demographic dimension, and value. Gender, race, and
ethnicity are the concept IDs carried by #allEvents; NULL remains its own group.
Each proportion is the category's distinct case count divided by all distinct
cases in that span.
*/
WITH case_rows AS (
	SELECT subject_id, span_label, age, gender, race, ethnicity
	FROM #allEvents
	WHERE case_event = 1
),
span_totals AS (
	SELECT span_label, COUNT(DISTINCT subject_id) AS total_cases
	FROM case_rows
	GROUP BY span_label
),
demographic_counts AS (
	SELECT span_label, 'gender' AS demographic,
		CAST(gender AS VARCHAR(255)) AS demographic_value,
		COUNT(DISTINCT subject_id) AS case_count
	FROM case_rows
	GROUP BY span_label, gender

	UNION ALL

	SELECT span_label, 'race' AS demographic,
		CAST(race AS VARCHAR(255)) AS demographic_value,
		COUNT(DISTINCT subject_id) AS case_count
	FROM case_rows
	GROUP BY span_label, race

	UNION ALL

	SELECT span_label, 'ethnicity' AS demographic,
		CAST(ethnicity AS VARCHAR(255)) AS demographic_value,
		COUNT(DISTINCT subject_id) AS case_count
	FROM case_rows
	GROUP BY span_label, ethnicity

	UNION ALL

	SELECT span_label, 'age' AS demographic,
		CAST(age AS VARCHAR(255)) AS demographic_value,
		COUNT(DISTINCT subject_id) AS case_count
	FROM case_rows
	GROUP BY span_label, age
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
