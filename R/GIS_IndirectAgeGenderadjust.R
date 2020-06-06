# GIS.Indirect.AgeGenderadjust <- function(df, fraction, maxLevel, conf.level = 0.95){
#   zv <- qnorm(0.5*(1+conf.level))
#
#   #Calculation to indirect age and gender standardization rate
#     stdratedf <-data.frame(df %>%
#                              group_by(age_cat, sex_cat) %>%
#                              summarise(targetsum = sum(target_count),
#                                        outcomesum = sum(outcome_count)))
#
#     stdratedf$std_rate <- stdratedf$outcomesum / stdratedf$targetsum
#
#     tmp <- dplyr::left_join(df, stdratedf, by=c("age_cat" = "age_cat", "sex_cat" = "sex_cat"))
#     tmp$std_expected <- tmp$target_count * tmp$std_rate
#
#     tmp <- data.frame(tmp%>%
#                         group_by(gadm_id) %>%
#                         summarise(target_count = sum(target_count),
#                                   outcome_count = sum(outcome_count),
#                                   std_expected = sum(std_expected)))
#
#
#     #Standardized Incidence Ratio calculation by administirative district
#
#     tmp$std_sir <- tmp$outcome_count / tmp$std_expected
#
#     tmp$std_logsirlower <- log(tmp$std_sir) - zv * (1/sqrt(tmp$outcome_count))
#     tmp$std_logsirupper <- log(tmp$std_sir) + zv * (1/sqrt(tmp$outcome_count))
#
#     tmp$std_sirlower <- exp(tmp$std_logsirlower)
#     tmp$std_sirupper <- exp(tmp$std_logsirupper)
#
#
#     #Standaridzation proportion
#     tmp$std_prop <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sir) * fraction
#     tmp$std_proplower <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sirlower) * fraction
#     tmp$std_propupper <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sirupper) * fraction
#
#
#     #Crude stat
#
#     #Crude SIR
#
#     tmp$crd_expected <- tmp$target_count * (sum(tmp$outcome_count) / sum(tmp$target_count))
#     tmp$crd_sir <- tmp$outcome_count / tmp$crd_expected
#
#     tmp$crd_logsirlower <- log(tmp$crd_sir) - zv * (1/sqrt(tmp$outcome_count))
#     tmp$crd_logsirupper <- log(tmp$crd_sir) + zv * (1/sqrt(tmp$outcome_count))
#
#     tmp$crd_sirlower <- exp(tmp$crd_logsirlower)
#     tmp$crd_sirupper <- exp(tmp$crd_logsirupper)
#
#
#     #Crude proportion
#     tmp$crd_prop <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sir) * fraction
#     tmp$crd_proplower <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sirlower) * fraction
#     tmp$crd_propupper <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sirupper) * fraction
#
#       return(tmp)
# }



GIS.Indirect.AgeGenderadjust <- function(df, fraction, conf.level = 0.95){
  zv <- qnorm(0.5*(1+conf.level))

  #Calculation to indirect age and gender standardization rate
  stdratedf <-data.frame(df %>%
                           group_by(age_cat, sex_cat) %>%
                           summarise(targetsum = sum(target_count),
                                     outcomesum = sum(outcome_count)))

  stdratedf$std_rate <- stdratedf$outcomesum / stdratedf$targetsum


  tmp <- dplyr::left_join(df, stdratedf, by=c("age_cat" = "age_cat", "sex_cat" = "sex_cat"))
  tmp$std_expected <- tmp$target_count * tmp$std_rate

  tmp <- data.frame(tmp%>%
                      group_by(gadm_id) %>%
                      summarise(target_count = sum(target_count),
                                outcome_count = sum(outcome_count),
                                std_expected = sum(std_expected)))

  #Standardized Incidence Ratio calculation by administirative district

  tmp$std_sir <- tmp$outcome_count / tmp$std_expected

  tmp$std_logsirlower <- log(tmp$std_sir) - zv * (1/sqrt(tmp$outcome_count))
  tmp$std_logsirupper <- log(tmp$std_sir) + zv * (1/sqrt(tmp$outcome_count))

  tmp$std_sirlower <- exp(tmp$std_logsirlower)
  tmp$std_sirupper <- exp(tmp$std_logsirupper)


  #Standaridzation proportion
  tmp$std_prop <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sir) * fraction
  tmp$std_proplower <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sirlower) * fraction
  tmp$std_propupper <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$std_sirupper) * fraction


  #Crude stat

  #Crude SIR

  tmp$crd_expected <- tmp$target_count * (sum(tmp$outcome_count) / sum(tmp$target_count))
  tmp$crd_sir <- tmp$outcome_count / tmp$crd_expected

  tmp$crd_logsirlower <- log(tmp$crd_sir) - zv * (1/sqrt(tmp$outcome_count))
  tmp$crd_logsirupper <- log(tmp$crd_sir) + zv * (1/sqrt(tmp$outcome_count))

  tmp$crd_sirlower <- exp(tmp$crd_logsirlower)
  tmp$crd_sirupper <- exp(tmp$crd_logsirupper)


  #Crude proportion
  tmp$crd_prop <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sir) * fraction
  tmp$crd_proplower <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sirlower) * fraction
  tmp$crd_propupper <- ((sum(tmp$outcome_count)/sum(tmp$target_count)) * tmp$crd_sirupper) * fraction

  return(tmp)
}
