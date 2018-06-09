Call.Cohortlist<-function(connectionDetails, connection, Resultschema, targettab="cohort", cdmVersion="5"){
  resultDatabaseSchema <- paste0(Resultschema,".dbo")
  connectionDetails <-connectionDetails
  connection <- connection

  Sys.setlocale(category="LC_CTYPE", locale="C")

  sql <- "SELECT distinct cohort_definition_id FROM @resultDatabaseSchema.@targettab order by cohort_definition_id"
  targettab <- targettab
  sql <- SqlRender::renderSql(sql,
                              resultDatabaseSchema=resultDatabaseSchema,
                              targettab=targettab)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  Cohortlist<-DatabaseConnector::querySql(connection, sql)
  return(Cohortlist)
}
