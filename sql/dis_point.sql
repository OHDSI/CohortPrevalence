/*
  This function selects the patients that have a disease at the first day of
  the period of interest or in the lookback period.
  
  Placeholders
  temp_schema: Schema containing the tables to select into
  result_schema: Schema containing the cohort table
  cohort_id: id of the cohort to take the patients from
*/

delete from @temp_schema.dis_point;
select distinct subject_id, year_number
from @result_schema.cohort
join @temp_schema.years on 1=1
where cohort_definition_id = @cohort_id
and cohort_start_date >= pre_start
and cohort_start_date <= year_start;