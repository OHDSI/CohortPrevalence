create table @temp_schema.years (
  year_number int,
  year_start date,
  year_end date,
  pre_start date
);

create table @temp_schema.obs_periods (
  person_id int,
  year_number int,
  observation_period_start_date date,
  observation_period_end_date date
);

create table @temp_schema.obs_first_day (
  person_id int,
  year_number int,
  overlap_days int
);


create table @temp_schema.obs_days (
  person_id int,
  year_number int,
  overlap_days int
);

create table @temp_schema.dis_point (
  subject_id int,
  year_number int
);

create table @temp_schema.dis_range (
  subject_id int,
  year_number int
);

create table @temp_schema.prevalence (
  person_id int,
  year_number int,
  overlap_days int,
  has_cond int,
  gender_concept_id int,
  age int
);