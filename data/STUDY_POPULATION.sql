SELECT               subject_id,
                     cohort_definition_id,
                     cohort_start_date,
                     cohort_end_date,
                     0 as outcome
INTO #target_population
FROM @resultDatabaseSchema.@targettab
WHERE cohort_definition_id = @tcdi

SELECT               subject_id,
                     cohort_definition_id,
                     cohort_start_date,
                     cohort_end_date,
                     1 as outcome
INTO #outcome_population
FROM @resultDatabaseSchema.@targettab
WHERE cohort_definition_id = @ocdi

SELECT * 
FROM #target_population
UNION
SELECT * 
FROM #outcome_population

drop table #target_population
drop table #outcome_population


##
select top 100 * from nhis_nsc.dbo.cost

select * from WEBAPI_2_6_NHIS_NSC.dbo.cohort
where COHORT_DEFINITION_ID = 855



select * 
from nhis_nsc.dbo.cost
where cost_id in (
			select visit_occurrence_id 
			from nhis_nsc.dbo.visit_occurrence
			where person_id in(
								select subject_id
								from WEBAPI_2_6_NHIS_NSC.dbo.cohort
								where COHORT_DEFINITION_ID = 855
								)
				 )


select top 10000 * from nhis_nsc.dbo.cost
order by cost_event_id

select top 10 * from nhis_nsc.dbo.DEVICE_EXPOSURE


select * 
from nhis_nsc.dbo.cost
where payer_plan_period_id in (
			select payer_plan_period_id
			from nhis_nsc.dbo.payer_plan_period
			where person_id in(
								select subject_id
								from WEBAPI_2_6_NHIS_NSC.dbo.cohort
								where COHORT_DEFINITION_ID = 855
								)
				 )




