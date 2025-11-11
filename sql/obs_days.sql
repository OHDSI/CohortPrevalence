/*
  This function calculates the number of observable days in the period of interest per patient. 
  In case of multiple calendar years the observable days are calculated for each year separately.
  
  Placeholders
  temp_schema: Schema containing the tables to select into
*/

with obs_sums as (
  select person_id, o.year_number,
  sum(datediff(day,
    case when observation_period_start_date > year_start then observation_period_start_date else year_start end,
    case when observation_period_end_date < year_end then observation_period_end_date else year_end end
          ) + 1
  ) as overlap_days
  from @temp_schema.obs_periods o
  join @temp_schema.years y on o.year_number = y.year_number
  group by person_id, o.year_number
)
insert into @temp_schema.obs_days
select person_id, year_number, overlap_days
from obs_sums
order by person_id, year_number;