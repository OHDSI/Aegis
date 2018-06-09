
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
  population <- AEGIS::GIS.DirectAgeAdjustment(cdmDatabaseSchema, startdt)

  colnames(df) <- tolower(colnames(df))
  colnames(population) <- tolower(colnames(population))

  df <- dplyr::left_join(population, df, by=c("gadm_id"="gadm_id", "age_cat"="age_cat"))

  df[, c("outcome_count", "target_count")][is.na(df[, c("outcome_count", "target_count")])] <- 0

  df$mortality <- (df$outcome_count / df$count) * 100000

  standard_population <-data.frame(df %>%
                                     group_by(age_cat) %>%
                                     summarise(pop = sum(count)))

  standard_population$pop_per <- standard_population$pop/100000

  calc <- data.frame()
  for(i in 1:max(df$gadm_id)){
    calc_temp <- sum(standard_population$pop_per*df[df$gadm_id==i,][,c("mortality")]) / sum(standard_population$pop)
    calc <- rbind(calc, cbind(i, calc_temp))
  }

  calc$calc_temp <- calc$calc_temp*100000
  colnames(calc) <- c("gadm_id", "age_mortality")


  cohort <-data.frame(df %>%
                        group_by(gadm_id) %>%
                        summarise(count = sum(count),
                                  target_count = sum(target_count),
                                  outcome_count = sum(outcome_count))
  )

  cohort$proportion <- (cohort$outcome_count / cohort$target_count) * fraction
  cohort$SIR <-(cohort$outcome_count  / cohort$target_count) /
    (sum(cohort$outcome_count) / sum(cohort$target_count))
  cohort<-cbind(cohort, Expected=cohort$target_count*sum(cohort$outcome_count)/sum(cohort$target_count))
  cohort<-dplyr::left_join(cohort, calc, by=("gadm_id" = "gadm_id"))
  return(cohort)
}
