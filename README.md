# CohortPrevalence

The goal of `CohortPrevalence` is to provide a standard prevalence calculation for data mapped to the OMOP CDM. 
This package relies on `DatabaseConnector` and `SqlRender` to perform its prevalence calculation. 

The methods for calculating prevalence are sourced from [Rassen et al 2018](https://pmc.ncbi.nlm.nih.gov/articles/PMC6301730/).
Users have the option of selecting:

- Period of Interest: the specific time for which prevalence is anchored for its enumeration. This is often one year but can also be defined as a span of multiple years.

- Lookback time: A defined span of time prior to the period of interest during which the database is queried for existing evidence of disease. In longitudinal observational databases, we are unable to dip into the data at a single point in time to determine whether a chronic condition is present. Instead, we define a period where we surveil for existing disease. If the chronic disease occurs during the lookback time, then it is considered to have prevalent disease.

- Denominator Option: 
    - Option 1: "day 1" population - the number of persons in the population who were observed on the first day of the period of interest
    - Option 2: "complete-period" population - the number of persons in the population who contribute all observable person-days in the period of interest. This is the strictest denominator
    - Option 3: "any-time" population -  the number of persons who who contributes at least 1 day in the period of interest. This is the most naive denominator and what IHD uses.
    - Option 4: "sufficient-time" population - the number of persons who contributes *sufficient* time in the period of interest based on at least *n* observable person-days in the period of interest

- Numerator Option:
    - Option 1: The number of patients who have been observed to have the condition of interest on the first day of the period of interest or within the lookback time
    - Option 2: The number of patients who have been observed to have the condition of interest at any time in the period of interest or within the lookback time.
    
- Defining Eligible Observation Periods in the population
    1) Minimum Observation Period Length: This is the required time that persons in the database must have been observed to be eligible. This can be any number in days; typical options would be 0 days or 365 days. 
    2) First or any observation period: Determine whether to use first observation period or any observation period to evaluate the prevalence of a disease during the period of interest. In claims data, it is possible for patients to leave the database and return. 

- Demographic Stratafication: age, gender and race
