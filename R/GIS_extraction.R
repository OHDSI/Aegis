
GIS.extraction<-function(connectionDetails, CDMschema, Resultschema, targettab="cohort", startdt, enddt, distinct,
                         tcdi, ocdi, fraction){
  distinct <- distinct
  startdt <- startdt
  enddt <- enddt
  tcdi <- tcdi
  ocdi <- ocdi
  fraction <- as.numeric(fraction)
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
                              ocdi=ocdi)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  df <- DatabaseConnector::querySql(connection, sql)
  Direct_population <- AEGIS::GIS.DirectAgeAdjustment(cdmDatabaseSchema, startdt)
  
  colnames(df) <- tolower(colnames(df))
  colnames(Direct_population) <- tolower(colnames(Direct_population))
  df[, c("outcome_count", "target_count")][is.na(df[, c("outcome_count", "target_count")])] <- 0
  
  #Direct age-adjustment
  Direct_population <-data.frame(Direct_population %>%
                                   group_by(age_cat) %>%
                                   summarise(target_sum = sum(count)))
  
  outcome_count <-data.frame(df %>%
                               group_by(age_cat) %>%
                               summarise(outcome_sum = sum(outcome_count)))
  
  Direct_population <- dplyr::left_join(Direct_population, outcome_count, by=c("age_cat"="age_cat"))
  
  Direct_population$expected <- Direct_population$outcome_sum / Direct_population$target_sum 
  
  df$mortality <- (df$outcome_count / df$target_count) * 100000
  
  standard_population <-data.frame(Direct_population %>%
                                     group_by(age_cat) %>%
                                     summarise(pop = sum(target_sum)))
  
  standard_population$pop_per <- standard_population$pop/fraction
  
  calc <- data.frame()
  for(i in 1:max(df$gadm_id)){
    calc_temp <- sum(standard_population$pop_per*df[df$gadm_id==i,][,c("mortality")]) / sum(standard_population$pop)
    calc_temp2 <- sum(df[df$gadm_id==i,][2] * Direct_population$expected)
    calc <- rbind(calc, cbind(i, calc_temp, calc_temp2))
  }
  
  colnames(calc) <- c("gadm_id", "indirect_incidence", "indirect_expected")
  calc$indirect_incidence <- calc$indirect_incidence*fraction
  Direct_calc <- calc
  #
  
  
  #Indirect age-adjustment
  Indirect_population <-data.frame(df %>%
                                     group_by(age_cat) %>%
                                     summarise(target_sum = sum(target_count),
                                               outcome_sum = sum(outcome_count)))
  
  Indirect_population$expected <- Indirect_population$outcome_sum / Indirect_population$target_sum 
  
  standard_population <-data.frame(df %>%
                                     group_by(age_cat) %>%
                                     summarise(pop = sum(target_count)))
  
  standard_population$pop_per <- standard_population$pop/fraction
  
  calc <- data.frame()
  for(i in 1:max(df$gadm_id)){
    calc_temp <- sum(standard_population$pop_per*df[df$gadm_id==i,][,c("mortality")]) / sum(standard_population$pop)
    calc_temp2 <- sum(df[df$gadm_id==i,][2] * Indirect_population$expected)
    calc <- rbind(calc, cbind(i, calc_temp, calc_temp2))
  }
  
  colnames(calc) <- c("gadm_id", "direct_incidence", "direct_expected")
  calc$direct_incidence <- calc$direct_incidence*fraction
  Indirect_calc <- calc
  #
  
  
  
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