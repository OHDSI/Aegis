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
                     @cdmDatabaseSchema.@targettab
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
                     @cdmDatabaseSchema.@targettab
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
       
       --------- 
       SELECT a.* 
       into #location_temp 
       FROM @cdmDatabaseSchema.@targettab a 
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
       ----------
       
       SELECT a.gadm_id, a.target_count as target_count, b.outcome_count as outcome_count
       FROM
              (
                     SELECT b.fact_id_1 AS gadm_id, count(a.subject_id) AS target_count
                     FROM #location_temp3 a LEFT JOIN
                     @cdmDatabaseSchema.fact_relationship b ON a.value_as_number = b.fact_id_2
                     WHERE cohort_definition_id = @tcdi
                     AND '@startdt' <= a.cohort_start_date
                     AND '@enddt' >= a.cohort_start_date
                     GROUP BY b.fact_id_1
              ) A
       LEFT JOIN
              (
                     SELECT c.fact_id_1 as gadm_id, count(a.subject_id) AS outcome_count
                     FROM #including_cohort a
                     LEFT JOIN
                     #location_temp3 b ON a.subject_id = b.SUBJECT_ID LEFT JOIN @cdmDatabaseSchema.fact_relationship c ON b.value_as_number = c.fact_id_2
                     GROUP BY c.fact_id_1
              ) B
       ON a.gadm_id = b.gadm_id
       GROUP BY a.gadm_id, a.target_count, b.outcome_count
       ORDER BY a.gadm_id
       
       DROP TABLE #including_cohort
       DROP TABLE #target_cohort
       DROP TABLE #outcome_cohort
       DROP TABLE #location_temp
       DROP TABLE #location_temp2
       DROP TABLE #location_temp3