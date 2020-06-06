# #Developing now...
# GIS.extraction<-function(cdmDatabaseSchema, resultDatabaseSchema, targettab="cohort", tcdi, ocdi, fraction){
#
#   Sys.setlocale(category="LC_CTYPE", locale="C")
#
#   temp <- paste0(.libPaths()[1],"/AEGIS/data/STUDY_POPULATION.sql")  #Query to creating Study Population
#
#   sql <- readSql(temp)
#   sql <- SqlRender::renderSql(sql,
#                               cdmDatabaseSchema=cdmDatabaseSchema,
#                               resultDatabaseSchema=resultDatabaseSchema,
#                               targettab=targettab,
#                               tcdi=tcdi,
#                               ocdi=ocdi)$sql
#   sql <- SqlRender::translateSql(sql,
#                                  targetDialect=connectionDetails$dbms)$sql
#   StudyPopulation <- DatabaseConnector::querySql(connection, sql)
#
#   StudyPopulation <-
#
#   return(cohort)
# }
