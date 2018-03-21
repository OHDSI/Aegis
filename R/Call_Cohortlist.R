Call.Cohortlist<-function(connectionDetails, connection, schema, targettab="cohort", cdmVersion="5"){
  connectionDetails <-connectionDetails
  connection <- connection

  sql <- "SELECT distinct cohort_definition_id FROM @cdmDatabaseSchema.@targettab order by cohort_definition_id"
  cdmDatabaseSchema <- paste0(schema,".dbo")
  targettab <- targettab
  cdmVersion <- cdmVersion
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              targettab=targettab)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  Cohortlist<-DatabaseConnector::querySql(connection, sql)
  return(Cohortlist)
}
