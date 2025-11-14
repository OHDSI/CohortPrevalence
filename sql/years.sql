/*
  The first function selects the calendar years between a start year and end year 
  including first day, last day and first day of lookback period for each
  calendar-year period.
  
  The second function selects arbitrary start and end date that are passed as
  string parameters and calculates the start of the lookback period based on those.
  Only a single period is created this way.
  
  Placeholders common
  temp_schema: Schema containing the tables to select into
  lookback: The length of the lockback period in days
  
  Placeholders function 1
  year_numbers: The list of year numbers to insert in the format (year1), (year2) etc.
  
  Placeholders function 2
  fixed_start_date: first day of the period of interest in format 'YYYY-MM-DD'
  fixed_end_date: last day of the period of interest in format 'YYYY-MM-DD'
*/

delete from @temp_schema.year_list;
insert into @temp_schema.year_list values
@year_numbers
;

delete from @temp_schema.years;
with borders as (
select year_number,
cast(cast(year_number as varchar) + '-01-01' as date) as year_start,
cast(cast(year_number as varchar) + '-12-31' as date) as year_end
from @temp_schema.year_list
)
insert into @temp_schema.years
select year_number, year_start, year_end,
dateadd(day, -@lookback, year_start) as pre_start
from borders;


-- second function starts here
delete from @temp_schema.years;
with borders(year_start, year_end) as (
  select *
  from (values
  (cast('@fixed_start_date' as date), cast('@fixed_end_date' as date))
  ) as v(year_start, year_end)
)
insert into @temp_schema.years
select 0 as year_number, year_start, year_end,
dateadd(day, -@lookback, year_start) as pre_start
from borders;
