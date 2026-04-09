/*
  These functions select the observation periods overlapping the period of interest
  by at least 1 day. In case of multiple calendar years the overlapping observation
  periods are selected for each year separately.
  
  The first function only considers the first observation period of each patient.
  If the first observation periods are shorter than min_obs_time days, the first
  sufficiently long observation period will be considered instead.
  The second function considers all observation periods of sufficient length.
  
  Placeholders
  temp_schema: Schema containing the tables to select into
  cdm_schema: Schema containing the transaction data
  min_obs_time: The minimum length an observation period needs to have in order to be considered.
*/

delete from @temp_schema.obs_periods;
with first_starts as (
  select person_id as p_id, min(observation_period_start_date) as min_start
  from @cdm_schema.observation_period
  where datediff(day, observation_period_start_date, observation_period_end_date) >= @min_obs_time
  group by person_id
)
insert into @temp_schema.obs_periods
select person_id, year_number, observation_period_start_date, observation_period_end_date
from @cdm_schema.observation_period
join first_starts on person_id = p_id and observation_period_start_date = min_start
join @temp_schema.years on 1=1
where observation_period_end_date >= year_start
and observation_period_start_date <= year_end
order by person_id, year_number, observation_period_start_date;


delete from @temp_schema.obs_periods;
insert into @temp_schema.obs_periods
select person_id, year_number, observation_period_start_date, observation_period_end_date
from @cdm_schema.observation_period
join @temp_schema.years on 1=1
where observation_period_end_date >= year_start
and observation_period_start_date <= year_end
and datediff(day, observation_period_start_date, observation_period_end_date) >= @min_obs_time
order by person_id, year_number, observation_period_start_date;
