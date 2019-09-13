GIS.calc1 <- function(GADM.table,CDM.table, GIS.level, GIS.distribution, GIS.Age){

  df <- dplyr::left_join(GADM.table, CDM.table, by=c(idxNum = "gadm_id"))
  i <- GIS.level
    switch(GIS.Age,
           "no"={
                   switch(GIS.distribution,
                          "count"={
                            temp <- paste0("select ID_",i," as id, sum(target_count) as target_count from df group by ID_",i, " order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "proportion"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(crd_prop) as proportion from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "SIR"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(outcome_count) / sum(crd_expected) as SIR from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "BYM"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(outcome_count) / sum(crd_expected) as SIR from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          }
                   )
                },
           "yes"={
                     switch(GIS.distribution,
                            "count"={
                              temp <- paste0("select ID_",i," as id, sum(target_count) as target_count from df group by ID_",i, " order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "proportion"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(std_prop) as proportion from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "SIR"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(outcome_count) / sum(std_expected) as SIR from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "BYM"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(outcome_count) / sum(std_expected) as SIR from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            }
                     )

           }


    )
  temp_df[is.na(temp_df[, "target_count"]), "target_count"] <- 0
  countdf_level <- temp_df
  return(countdf_level)
}
