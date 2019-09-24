GIS.extraction<-function(connectionDetails, cdmDatabaseSchema, resultDatabaseSchema, targettab="cohort", startdt, enddt,
                         tcdi, ocdi, fraction, timeatrisk_startdt, timeatrisk_enddt, timeatrisk_enddt_panel, maxLevel){

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  maxLevel <- maxLevel

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


  sql <- SqlRender::readSql(temp)
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              resultDatabaseSchema=resultDatabaseSchema,
                              targettab=targettab,
                              startdt=startdt,
                              enddt=enddt,
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
  fraction <<- fraction
  cohort <- GIS.Indirect.AgeGenderadjust(df, as.numeric(fraction))

  return(cohort)
}
