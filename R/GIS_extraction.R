
GIS.extraction<-function(connectionDetails, schema, targettab="cohort", startdt, enddt, distinct,
                         tcdi, ocdi, fraction){
  startdt <- startdt
  enddt <- enddt
  tcdi <- tcdi
  ocdi <- ocdi
  fraction <- as.numeric(fraction)
  connectionDetails <- connectionDetails
  connection <- DatabaseConnector::connect(connectionDetails)
  cdmDatabaseSchema <- paste0(schema,".dbo")
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
                              targettab=targettab,
                              startdt=startdt,
                              enddt=enddt,
                              distinct=distinct,
                              tcdi=tcdi,
                              ocdi=ocdi)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  cohort <- DatabaseConnector::querySql(connection, sql)
  colnames(cohort) <- tolower(colnames(cohort))
  cohort <- na.omit(cohort)
  cohort$proportion <- (cohort$outcome_count / cohort$target_count) * fraction
  cohort$SIR <-(cohort$outcome_count  / cohort$target_count) /
    (sum(cohort$outcome_count) / sum(cohort$target_count))
  cohort<-cbind(cohort, Expected=cohort$target_count*sum(cohort$outcome_count)/sum(cohort$target_count))
  return(cohort)
}
