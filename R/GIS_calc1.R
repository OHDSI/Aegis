GIS.calc1 <- function(GIS.level, GIS.distribution, GIS.Age){

  df <- dplyr::left_join(GADM.table, CDM.table, by=c("ID_2" = "gadm_id"))
  i <- GIS.level
    switch(GIS.Age,
           "no"={
                   switch(GIS.distribution,
                          "count"={
                            temp <- paste0("select ID_",i," as id, sum(target_count) as target_count from df group by ID_",i, " order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "proportion"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(proportion) as proportion from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "SIR"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(SIR) as SIR from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          },
                          "BYM"={
                            temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(SIR) as SIR from df group by ID_",i," order by ID_",i)
                            temp_df <- sqldf::sqldf(temp)
                          }
                   )
                },
           "indrect"={
                     switch(GIS.distribution,
                            "count"={
                              temp <- paste0("select ID_",i," as id, sum(target_count) as target_count from df group by ID_",i, " order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "proportion"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(indirect_incidence) as proportion from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "SIR"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(indirect_SIR) as SIR from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            },
                            "BYM"={
                              temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(SIR) as SIR from df group by ID_",i," order by ID_",i)
                              temp_df <- sqldf::sqldf(temp)
                            }
                     )
             
                },
           "direct"={
                       switch(GIS.distribution,
                              "count"={
                                temp <- paste0("select ID_",i," as id, sum(target_count) as target_count from df group by ID_",i, " order by ID_",i)
                                temp_df <- sqldf::sqldf(temp)
                              },
                              "proportion"={
                                temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(direct_incidence) as proportion from df group by ID_",i," order by ID_",i)
                                temp_df <- sqldf::sqldf(temp)
                              },
                              "SIR"={
                                temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(direct_SIR) as SIR from df group by ID_",i," order by ID_",i)
                                temp_df <- sqldf::sqldf(temp)
                              },
                              "BYM"={
                                temp <- paste0("select ID_",i, " as id, sum(outcome_count) as outcome_count, sum(target_count) as target_count, sum(SIR) as SIR from df group by ID_",i," order by ID_",i)
                                temp_df <- sqldf::sqldf(temp)
                              }
                       )
             
                }
      
      )
  countdf_level <- na.omit(temp_df)
  return(countdf_level)
}