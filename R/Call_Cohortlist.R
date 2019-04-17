Call.Cohortlist<-function(WebapiDBserver,WebapiDBname,WebapiDBschema,Resultschema, targettab="cohort", cdmVersion="5"){

  Sys.setlocale(category="LC_CTYPE", locale="C")
  ## Check if a cohort exists
  sql <- "SELECT distinct cohort_definition_id FROM @resultDatabaseSchema.@targettab order by cohort_definition_id"

  sql <- SqlRender::renderSql(sql,
                              resultDatabaseSchema=Resultschema,
                              targettab=targettab)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  cohort<-DatabaseConnector::querySql(connection, sql)

  ##Check Cohort Definition
  sql <- 'SELECT name,id FROM [@WebapiDBserver].@WebapiDBname.@WebapiDBschema.cohort_definition'
  sql <- SqlRender::renderSql(sql,
                              WebapiDBserver=WebapiDBserver,
                              WebapiDBname=WebapiDBname,
                              WebapiDBschema=WebapiDBschema)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  cohortName<-DatabaseConnector::querySql(connection, sql)

  ## Print List
  cohortList <- cbind(cohortName,'exists' = rep('Cohort does not exist',nrow(cohortName)))
  cohortList$exists <- as.character(cohortList$exists)
  for(i in cohort$COHORT_DEFINITION_ID){
    cohortList$exists[cohortList$ID == i] <- 'Cohort exists'
  }

  cohortList <- paste(cohortList$ID,cohortList$NAME,'(',cohortList$exists,')')

  return(cohortList)
}

