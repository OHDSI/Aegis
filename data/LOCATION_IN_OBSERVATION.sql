       SELECT t.cohort_definition_id, t.subject_id, t.cohort_start_date, t.cohort_end_date
       INTO #target_cohort
       FROM
              (
                     SELECT
                     @distinct subject_id,
                     cohort_definition_id,
                     cohort_start_date,
                     cohort_end_date
                     FROM
                     @resultDatabaseSchema.@targettab
              ) t
       WHERE cohort_definition_id = @tcdi
       -- Set the index date period
       AND '@startdt' <= t.cohort_start_date
       AND '@enddt' >= t.cohort_start_date
       
       -- Outcome cohort
       SELECT o.cohort_definition_id, o.subject_id, o.cohort_start_date, o.cohort_end_date
       INTO #outcome_cohort
       FROM
              (
                     SELECT
                     @distinct subject_id,
                     cohort_definition_id,
                     cohort_start_date,
                     cohort_end_date
                     FROM
                     @resultDatabaseSchema.@targettab
              ) o
       WHERE cohort_definition_id = @ocdi
       --AND '@startdt' <= o.cohort_start_date 
       --AND '@enddt' >= o.cohort_end_date
       
       
       SELECT o.subject_id, o.cohort_definition_id, o.cohort_start_date, o.cohort_end_date
       INTO #including_cohort
       FROM #outcome_cohort o
       LEFT JOIN #target_cohort t
       ON t.subject_id = o.subject_id
       WHERE t.cohort_start_date <= o.cohort_start_date
       AND t.cohort_end_date >= o.cohort_start_date
       AND dateadd(day, @timeatrisk_startdt, t.cohort_start_date) <= o.cohort_start_date 
       AND dateadd(day, @timeatrisk_enddt, t.@timeatrisk_enddt_panel) >= o.cohort_end_date
       

       select a.*,b.location_id, 
                     case
                     when year(a.cohort_start_date)-b.year_of_birth >= 79 then 8
                     when year(a.cohort_start_date)-b.year_of_birth > 69 then 7
                     when year(a.cohort_start_date)-b.year_of_birth > 59 then 6
                     when year(a.cohort_start_date)-b.year_of_birth > 49 then 5
                     when year(a.cohort_start_date)-b.year_of_birth > 39 then 4
                     when year(a.cohort_start_date)-b.year_of_birth > 29 then 3
                     when year(a.cohort_start_date)-b.year_of_birth > 19 then 2
                     when year(a.cohort_start_date)-b.year_of_birth >= 10 then 1
                     when year(a.cohort_start_date)-b.year_of_birth <= 9 then 0
                     end as age_cat,
                     case when b.gender_concept_id = '8507' then 0
                            when b.gender_concept_id = '8532' then 1
                     end as sex_cat
       into #including_cohort2
       from #including_cohort a join @cdmDatabaseSchema.person b
       on a.subject_id=b.person_id


       select distinct a.subject_id, b.fact_id_1, a.age_cat, a.sex_cat
       into #including_cohort3
       from #including_cohort2 a left join @cdmDatabaseSchema.fact_relationship b on a.location_id=b.fact_id_2
       where b.domain_concept_id_1 = 4083586
       order by b.fact_id_1, a.age_cat, a.sex_cat

       --------- 
       SELECT a.* 
       into #location_temp 
       FROM @resultDatabaseSchema.@targettab a 
       WHERE COHORT_DEFINITION_ID = @tcdi
       
       SELECT z.cohort_definition_id, z.subject_id, max(z.cohort_start_date) as cohort_start_date, max(z.cohort_end_date) as cohort_end_date, max(z.observation_date) as observation_date 
       into #location_temp2
       FROM
              (
                     SELECT a.* 
                     FROM
              (
                     select x.*, y.* from #location_temp x join (select * from @cdmDatabaseSchema.observation where observation_concept_id='4083586') y
                     on x.SUBJECT_ID = y.person_id
              ) a
       wHERE a.observation_date <= a.cohort_start_date
              ) z
       GROUP BY z.COHORT_DEFINITION_ID, z.SUBJECT_ID, z.cohort_start_date, z.cohort_end_date
       
       SELECT a.*, b.value_as_number
       into #location_temp3
       FROM #location_temp2 a left join (select * from @cdmDatabaseSchema.observation where observation_concept_id = '4083586')  b
       ON a.subject_id=b.person_id and
       a.observation_date=b.observation_date

       SELECT a.*, case
              when year(a.cohort_start_date)-b.year_of_birth >= 79 then 8
              when year(a.cohort_start_date)-b.year_of_birth > 69 then 7
              when year(a.cohort_start_date)-b.year_of_birth > 59 then 6
              when year(a.cohort_start_date)-b.year_of_birth > 49 then 5
              when year(a.cohort_start_date)-b.year_of_birth > 39 then 4
              when year(a.cohort_start_date)-b.year_of_birth > 29 then 3
              when year(a.cohort_start_date)-b.year_of_birth > 19 then 2
              when year(a.cohort_start_date)-b.year_of_birth >= 10 then 1
              when year(a.cohort_start_date)-b.year_of_birth <= 9 then 0
          end as age_cat,
                     case when b.gender_concept_id = '8507' then 0
                            when b.gender_concept_id = '8532' then 1
                     end as sex_cat
       into #location_temp4
       FROM #location_temp3 a left join @cdmDatabaseSchema.person b
       ON a.subject_id=b.person_id
       ----------
       
       SELECT cast(a.gadm_id as integer) as gadm_id, cast(a.target_count as integer) as target_count, cast(b.outcome_count as integer) as outcome_count, a.age_cat, a.sex_cat
       FROM
              (
                     SELECT b.fact_id_1 AS gadm_id, count(a.subject_id) AS target_count, a.age_cat, a.sex_cat
                     FROM #location_temp4 a LEFT JOIN
                     @cdmDatabaseSchema.fact_relationship b ON a.value_as_number = b.fact_id_2
                     WHERE cohort_definition_id = @tcdi
                     AND '@startdt' <= a.cohort_start_date
                     AND '@enddt' >= a.cohort_start_date
                     GROUP BY b.fact_id_1, a.age_cat, a.sex_cat
              ) A
       LEFT JOIN
              (
                     SELECT fact_id_1 as gadm_id, count(subject_id) AS outcome_count, age_cat, sex_cat
                     FROM #including_cohort3
                     GROUP BY fact_id_1, age_cat, sex_cat
              ) B
       ON a.gadm_id = b.gadm_id and a.age_cat = b.age_cat and a.sex_cat = b.sex_cat
       GROUP BY a.gadm_id, a.age_cat, a.sex_cat, a.target_count, b.outcome_count
       ORDER BY a.gadm_id, a.age_cat
       
       DROP TABLE #including_cohort
	   DROP TABLE #including_cohort2
       DROP TABLE #including_cohort3
       DROP TABLE #target_cohort
       DROP TABLE #outcome_cohort
       DROP TABLE #location_temp
       DROP TABLE #location_temp2
       DROP TABLE #location_temp3
       DROP TABLE #location_temp4