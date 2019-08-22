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
  cohort<-DatabaseConnector::querySql(connection, sql)

  sql <- "use @Resultschema select * from information_schema.tables"
  sql <- SqlRender::renderSql(sql,
                              Resultschema=Resultschema)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  tablelist <- DatabaseConnector::querySql(connection, sql)
  tablelist$TABLE_NAME <- tolower(tablelist$TABLE_NAME)


  if (isTRUE(which(tablelist[,3] %in% "cohort_definition") >= 1) == TRUE) {
          sql <- "SELECT distinct name, id FROM @resultDatabaseSchema.cohort_definition"
          sql <- SqlRender::renderSql(sql,
                                      resultDatabaseSchema=resultDatabaseSchema)$sql
          sql <- SqlRender::translateSql(sql,
                                         targetDialect=connectionDetails$dbms)$sql
          cohortname <- DatabaseConnector::querySql(connection, sql)

          cohortlist <- dplyr::left_join(cohort, cohortname, by = c("COHORT_DEFINITION_ID" = "ID"))
          cohortlist$cohortname <- paste(cohortlist$COHORT_DEFINITION_ID, cohortlist$NAME, sep="; ")
        } else {
          cohortlist <- cohort
        }

  return(cohortlist)
}
