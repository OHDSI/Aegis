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
       
       SELECT o.subject_id, o.cohort_definition_id, o.cohort_start_date, o.cohort_end_date
       INTO #including_cohort
       FROM #outcome_cohort o
       LEFT JOIN #target_cohort t
       ON t.subject_id = o.subject_id
       WHERE t.cohort_start_date <= o.cohort_start_date
       AND t.cohort_end_date >= o.cohort_start_date
       
       SELECT a.gadm_id, a.target_count as target_count, b.outcome_count as outcome_count
       FROM
         (
           SELECT c.fact_id_1 AS gadm_id, count(a.subject_id) AS target_count
           FROM @resultDatabaseSchema.@targettab a LEFT JOIN
           @cdmDatabaseSchema.person b ON a.subject_id = b.person_id LEFT JOIN @cdmDatabaseSchema.fact_relationship c ON b.location_id = c.fact_id_2
           WHERE cohort_definition_id = @tcdi
           AND '@startdt' <= a.cohort_start_date
           AND '@enddt' >= a.cohort_start_date
           GROUP BY c.fact_id_1
         ) A
       LEFT JOIN
         (
           SELECT c.fact_id_1 as gadm_id, count(a.subject_id) AS outcome_count
           FROM #including_cohort a
           LEFT JOIN
           @cdmDatabaseSchema.person b ON a.subject_id = b.person_id LEFT JOIN @cdmDatabaseSchema.fact_relationship c ON b.location_id = c.fact_id_2
           GROUP BY c.fact_id_1
         ) B
       ON a.gadm_id = b.gadm_id
       GROUP BY a.gadm_id, a.target_count, b.outcome_count
       ORDER BY a.gadm_id
       
       DROP TABLE #including_cohort
       DROP TABLE #target_cohort
       DROP TABLE #outcome_cohort