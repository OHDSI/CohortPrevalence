/*
  This function selects the patients that are observable at the first day
  of the period of interest (day 1 population). In case of multiple calendar years 
  Jan. 1st of each year is checked.
  
  Placeholders
  temp_schema: Schema containing the tables to select into
*/

insert into @temp_schema.obs_first_day
select person_id, o.year_number, 0 as overlap_days
from @temp_schema.obs_periods o
join @temp_schema.years y on o.year_number = y.year_number
where year_start >= observation_period_start_date
and year_start <= observation_period_end_date
order by person_id, o.year_number;