
GIS.extraction<-function(connectionDetails, CDMschema, Resultschema, targettab="cohort", startdt, enddt, distinct,
                         tcdi, ocdi, fraction, timeatrisk_startdt, timeatrisk_enddt, timeatrisk_enddt_panel){
  distinct <- distinct
  startdt <- startdt
  enddt <- enddt
  tcdi <- tcdi
  ocdi <- ocdi
  fraction <- as.numeric(fraction)
  timeatrisk_startdt <- timeatrisk_startdt
  timeatrisk_enddt <- timeatrisk_enddt
  timeatrisk_enddt_panel <- timeatrisk_enddt_panel #params: @cohort_start_date and @cohort_end_date
  connectionDetails <- connectionDetails
  connection <- DatabaseConnector::connect(connectionDetails)
  cdmDatabaseSchema <- paste0(CDMschema,".dbo")
  resultDatabaseSchema <- paste0(Resultschema,".dbo")
  targettab <- targettab
  cdmVersion <- "5"
  Sys.setlocale(category="LC_CTYPE", locale="C")

  sql <-  "select top 1 * from @cdmDatabaseSchema.observation
           where observation_concept_id = '4083586'"
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  temp <- DatabaseConnector::querySql(connection, sql)

  ifelse(is.na(temp[1,1]),
         temp <- paste0(.libPaths()[1],"/AEGIS/data/LOCATION_IN_PERSON.sql"),  #in person
         temp <- paste0(.libPaths()[1],"/AEGIS/data/LOCATION_IN_OBSERVATION.sql"))  #in observation


  sql <- readSql(temp)
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              resultDatabaseSchema=resultDatabaseSchema,
                              targettab=targettab,
                              startdt=startdt,
                              enddt=enddt,
                              distinct=distinct,
                              tcdi=tcdi,
                              ocdi=ocdi,
                              timeatrisk_startdt=timeatrisk_startdt,
                              timeatrisk_enddt=timeatrisk_enddt,
                              timeatrisk_enddt_panel=timeatrisk_enddt_panel)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  df <- DatabaseConnector::querySql(connection, sql)
  Direct_population <- AEGIS::GIS.DirectAgeAdjustment(cdmDatabaseSchema, startdt)

  colnames(df) <- tolower(colnames(df))
  colnames(Direct_population) <- tolower(colnames(Direct_population))
  df[, c("outcome_count", "target_count")][is.na(df[, c("outcome_count", "target_count")])] <- 0
  df$incidence <- (df$outcome_count / df$target_count) * fraction

  #Direct age-adjustment

  #Calculation for age & gender adjusted incidence rate(Direct)
  Direct_population <-data.frame(Direct_population %>%
                                   group_by(age_cat, sex_cat) %>%
                                   summarise(target_sum = sum(count)))

  outcome_count <-data.frame(df %>%
                               group_by(age_cat, sex_cat) %>%
                               summarise(outcome_sum = sum(outcome_count)))

  Direct_population <- dplyr::left_join(Direct_population, outcome_count, by=c("age_cat"="age_cat", "sex_cat"="sex_cat"))

  Direct_population$expected <- Direct_population$outcome_sum / Direct_population$target_sum
  Direct_population[, c("outcome_sum", "expected")][is.na(Direct_population[, c("outcome_sum", "expected")])] <- 0

  standard_population <-data.frame(Direct_population %>%
                                     group_by(age_cat, sex_cat) %>%
                                     summarise(population = sum(target_sum),
                                               expected = sum(expected)))

  #Calculation adjusted incidence by region
  temporal_table <- left_join(df, standard_population, by=c("age_cat"="age_cat", "sex_cat"="sex_cat"))
  temporal_table$adjusted_incidence <- ((temporal_table$incidence/fraction)*temporal_table$population)
  temporal_table$adjusted_expected <- temporal_table$target_count * temporal_table$expected


  temporal_table2 <- data.frame(temporal_table %>%
                                  group_by(gadm_id) %>%
                                  summarise (adjusted_incidence = sum(adjusted_incidence),
                                             adjusted_expected = sum(adjusted_expected)))

  temporal_table2$adjusted_incidence <- temporal_table2$adjusted_incidence / fraction
  calc <- temporal_table2

  colnames(calc) <- c("gadm_id", "direct_incidence", "direct_expected")
  Direct_calc <- calc
  #End of direct adjuetment


  #Indirect age-adjustment

  #Calculation for age & gender adjusted incidence rate(Indirect)
  df$incidence <- (df$outcome_count / df$target_count) * fraction

  Indirect_population <-data.frame(df %>%
                                     group_by(age_cat, sex_cat) %>%
                                     summarise(population = sum(target_count),
                                               outcome_sum = sum(outcome_count)))
  Indirect_population$expected <- Indirect_population$outcome_sum / Indirect_population$population

  temporal_table <- left_join(df, Indirect_population, by=c("age_cat"="age_cat", "sex_cat"="sex_cat"))
  temporal_table$adjusted_incidence <- ((temporal_table$incidence/fraction)*temporal_table$population)
  temporal_table$adjusted_expected <- temporal_table$target_count * temporal_table$expected

  temporal_table2 <- data.frame(temporal_table %>%
                                  group_by(gadm_id) %>%
                                  summarise (adjusted_incidence = sum(adjusted_incidence),
                                             adjusted_expected = sum(adjusted_expected)))

  temporal_table2$adjusted_incidence <- temporal_table2$adjusted_incidence / fraction
  calc <- temporal_table2

  colnames(calc) <- c("gadm_id", "indirect_incidence", "indirect_expected")
  Indirect_calc <- calc
  #End of indirect adjuetment



  cohort <-data.frame(df %>%
                        group_by(gadm_id) %>%
                        summarise(target_count = sum(target_count),
                                  outcome_count = sum(outcome_count))
  )


  cohort$proportion <- (cohort$outcome_count / cohort$target_count) * fraction
  cohort$SIR <-(cohort$outcome_count  / cohort$target_count) /
    (sum(cohort$outcome_count) / sum(cohort$target_count))
  cohort<-cbind(cohort, expected=cohort$target_count*sum(cohort$outcome_count)/sum(cohort$target_count))
  cohort<-dplyr::left_join(cohort, Direct_calc , by=("gadm_id" = "gadm_id"))
  cohort<-dplyr::left_join(cohort, Indirect_calc , by=("gadm_id" = "gadm_id"))
  cohort$indirect_SIR <- cohort$outcome_count / cohort$indirect_expected
  cohort$direct_SIR <- cohort$outcome_count / cohort$direct_expected

  return(cohort)
}




