/*
  This function selects the patients that have a disease at the first day of
  the period of interest or in the lookback period.
  
  Placeholders
  temp_schema: Schema containing the tables to select into
  result_schema: Schema containing the cohort table
  cohort_id: id of the cohort to take the patients from
*/

with overlaps as (
  select subject_id, year_number,
  sum(case
    when cohort_end_date < pre_start or cohort_start_date > year_start then 0
	else
      datediff(day,
        case when cohort_start_date > pre_start then cohort_start_date else pre_start end,
        case when cohort_end_date < year_start then cohort_end_date else year_start end
        ) + 1
	end
  ) as overlap_days
  from @result_schema.cohort
  join @temp_schema.years on 1=1
  where cohort_definition_id = @cohort_id
  group by subject_id, year_number
)
insert into @temp_schema.dis_point
select subject_id, year_number
from overlaps
where overlap_days > 0
order by subject_id, year_number;