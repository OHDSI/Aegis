GIS.calc1 <- function(GIS.level, GIS.distribution, fraction){

  df <- dplyr::left_join(GADM.table, CDM.table, by=c("ID_2" = "gadm_id"))
  i <- GIS.level
  switch(GIS.distribution,
         "count"={
           temp <- paste0("select ID_",i," as id, sum(outcome_count) as outcome_count from df group by ID_",i, " order by ID_",i)
           temp_df <- sqldf::sqldf(temp)
         },
         "proportion"={
           temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count from df group by ID_",i," order by ID_",i)
           temp_df <- sqldf::sqldf(temp)
         },
         "SIR"={
           temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count from df group by ID_",i," order by ID_",i)
           temp_df <- sqldf::sqldf(temp)
         },
         "age_mortality"={
           temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, age_mortality as age_mortality from df group by ID_",i," order by ID_",i)
           temp_df <- sqldf::sqldf(temp)
         }
  )
  countdf_level <- na.omit(temp_df)
  return(countdf_level)
}
