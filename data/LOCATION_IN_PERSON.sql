      --target cohort
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
       AND '@startdt' <= t.cohort_start_date
       AND '@enddt' >= t.cohort_start_date
       ---- end of setting for target cohort
       


       --outcome cohort
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

       ---- end of setting for outcome cohort
       


       --including cohort
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
       from #including_cohort a join @resultDatabaseSchema.person b
       on a.subject_id=b.person_id

     select distinct a.subject_id, b.fact_id_2, a.age_cat, a.sex_cat
       into #including_cohort3
       from #including_cohort2 a join @resultDatabaseSchema.fact_relationship b on a.location_id=b.fact_id_1
       where b.domain_concept_id_1 = 4083586
       order by b.fact_id_2, a.age_cat, a.sex_cat
       ---- end of setting for including cohort
     






       ---- person cohort
       SELECT a.* 
       into #person_temp
       FROM @resultDatabaseSchema.cohort a 
       WHERE COHORT_DEFINITION_ID = @tcdi


       SELECT a.*, b.location_id, 
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
          case 
              when b.gender_concept_id = '8507' then 0
              when b.gender_concept_id = '8532' then 1
          end as sex_cat
       into #person_temp2
       from #person_temp a left join @resultDatabaseSchema.person b
       on a.subject_id=b.person_id
       ---- end of setting for person_cohort



       SELECT cast(a.gadm_id as integer) as gadm_id, cast(a.target_count as integer) as target_count, cast(b.outcome_count as integer) as outcome_count, a.age_cat, a.sex_cat
       FROM
              (
                     SELECT b.fact_id_2 AS gadm_id, count(a.subject_id) AS target_count, a.age_cat, a.sex_cat
                     FROM #person_temp2 a JOIN
                     @cdmDatabaseSchema.fact_relationship b ON a.location_id = b.fact_id_1
                     WHERE cohort_definition_id = @tcdi
                     AND '@startdt' <= a.cohort_start_date
                     AND '@enddt' >= a.cohort_start_date
                     GROUP BY b.fact_id_2, a.age_cat, a.sex_cat
              ) A
       LEFT JOIN
         (
                     SELECT fact_id_2 as gadm_id, count(subject_id) AS outcome_count, age_cat, sex_cat
                     FROM #including_cohort3
                     GROUP BY fact_id_2, age_cat, sex_cat
         ) B
       ON a.gadm_id = b.gadm_id and a.age_cat = b.age_cat and a.sex_cat = b.sex_cat
       GROUP BY a.gadm_id, a.age_cat, a.sex_cat, a.target_count, b.outcome_count
       ORDER BY a.gadm_id, a.age_cat
       
       DROP TABLE #including_cohort
       DROP TABLE #target_cohort
       DROP TABLE #outcome_cohort
       DROP TABLE #person_temp
       DROP TABLE #person_temp2
