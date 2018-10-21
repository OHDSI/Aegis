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

  colnames(df) <- tolower(colnames(df))
  df[, c("outcome_count", "target_count")][is.na(df[, c("outcome_count", "target_count")])] <- 0
  
  #Indirect age-adjustment
  cohort <- GIS.Indirect.AgeGenderadjust(df, fraction)

  return(cohort)
}