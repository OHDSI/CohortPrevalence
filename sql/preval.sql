/*
  This function calculates point prevalence or period prevalence based on the
  numerator and denominator table chosen.
  
  Result columns
  person_id
  year_number: In case of multiple year periods the calendar year, otherwise zero
  overlap_days: In case of period denominator the observed days per person and period, otherwise zero
  has_cond: 1 if the person has the condition of interest, 0 otherwise
  gender_concept_id
  age: persons age at the end of each period
  
  Placeholders
  cdm_schema: Schema containing the transaction data
  temp_schema: Schema containing the tables to select into
  num_table: table containing the numerator (dis_point, dis_range)
  denom_table: table containing the denominator (obs_first_day, obs_days)
  
  Common combinations are
  dis_point, obs_first_day: point prevalence
  dis_range, obs_days: period prevalence
*/

with overlaps as (
  select person_id, d.year_number, overlap_days,
  case when subject_id is not null then 1 else 0 end as has_cond
  from @temp_schema.@denom_table d
  left join @temp_schema.@num_table n on person_id = subject_id and d.year_number = n.year_number
),
birth_years as (
  select o.person_id, year_number, gender_concept_id, year_of_birth
  from overlaps o
  join @cdm_schema.person p on o.person_id = p.person_id
),
ages as (
  select person_id, b.year_number, gender_concept_id,
  datediff(year, cast(cast(year_of_birth as varchar) + '-01-01' as date), year_end) as age
  from birth_years b
  join @temp_schema.years y on b.year_number = y.year_number
)
insert into @temp_schema.prevalence
select o.person_id, o.year_number, overlap_days, has_cond, gender_concept_id, age
from overlaps o
join ages a on o.person_id = a.person_id and o.year_number = a.year_number
order by o.person_id, o.year_number;