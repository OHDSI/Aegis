#Developing now...
GIS.extraction<-function(connectionDetails, CDMschema, Resultschema, targettab="cohort", tcdi, ocdi, fraction){
  tcdi <- tcdi
  ocdi <- ocdi
  connectionDetails <- connectionDetails
  connection <- DatabaseConnector::connect(connectionDetails)
  cdmDatabaseSchema <- paste0(CDMschema,".dbo")
  resultDatabaseSchema <- paste0(Resultschema,".dbo")
  targettab <- targettab
  cdmVersion <- "5"
  Sys.setlocale(category="LC_CTYPE", locale="C")


  temp <- paste0(.libPaths()[1],"/AEGIS/data/STUDY_POPULATION.sql")  #Query to creating Study Population

  sql <- readSql(temp)
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              resultDatabaseSchema=resultDatabaseSchema,
                              targettab=targettab,
                              tcdi=tcdi,
                              ocdi=ocdi)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  StudyPopulation <- DatabaseConnector::querySql(connection, sql)

  StudyPopulation <-

  return(cohort)
}
