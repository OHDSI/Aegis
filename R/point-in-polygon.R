PIP <- function(CDMschema, maxLevel, country){

  mapping.path <- paste0(.libPaths()[1], "/AEGIS/map/", country, "/mapping")

  if (!file.exists(mapping.path))
    dir.create(file.path(mapping.path))

  setwd(mapping.path)
  mapping.file <- paste0(CDMschema, ".rds")
  demographics.file <- paste0(CDMschema, "_demographics", ".rds")

  #Information call from person table
  if (!file.exists(demographics.file)){
  sql <- "select cast(year_of_birth as int) as year_of_birth, cast(gender_concept_id as int) as gender_concept_id,
            cast(location_id as int) as location_id from @cdmDatabaseSchema.person"
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=CDMschema)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  person <- DatabaseConnector::querySql(connection, sql)
  colnames(person) <- tolower(colnames(person))
  person$gender_concept_id <- factor(person$gender_concept_id)
  demographics <- data.frame(person %>%
                               group_by(location_id, gender_concept_id, year_of_birth) %>%
                               summarise(n <- n()))
  colnames(demographics) <- c("location_id", "gender_concept_id", "year_of_birth", "n")
  saveRDS(demographics, demographics.file)
  }

  if (file.exists(mapping.file)){
    output <- readRDS(mapping.file)
    #call location table
    sql <-  "select  * from @cdmDatabaseSchema.location"
    sql <- SqlRender::renderSql(sql,
                                cdmDatabaseSchema=CDMschema)$sql
    sql <- SqlRender::translateSql(sql,
                                   targetDialect=connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection, sql)
    colnames(df) <- tolower(colnames(df))
    location <- df
    preTable <- left_join(location, output, by=c("location_id", "location_id"))
    return(preTable)
  } else {

    #call location table
    sql <-  "select  * from @cdmDatabaseSchema.location"
    sql <- SqlRender::renderSql(sql,
                                cdmDatabaseSchema=CDMschema)$sql
    sql <- SqlRender::translateSql(sql,
                                   targetDialect=connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection, sql)
    colnames(df) <- tolower(colnames(df))
    location <- df

    #exclude no lat/lon rows
    df <- data.frame(df %>% filter(!is.na(longitude)))
    df <- data.frame(df %>% filter(longitude!="NA"))

    df <- data.frame(df %>% filter(!is.na(latitude)))
    df <- data.frame(df %>% filter(latitude!="NA"))

    df$longitude <- as.double(df$longitude)
    df$latitude <- as.double(df$latitude)

    coordinates(df) <- ~ longitude + latitude
    proj4string(df) <- proj4string(GADM[[maxLevel+1]])

    output <- sp::over(df, GADM[[maxLevel+1]])
    output <- cbind(df@data, output)
    colnames(output) <- tolower(colnames(output))
    output <- output[,c("location_id", "objectid")]
    saveRDS(output, file.path(mapping.path, mapping.file))

    preTable <- left_join(location, output, by=c("location_id", "location_id"))

    return(preTable)
  }
}
